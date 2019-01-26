
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(gridExtra)
library(corrplot)
library(lubridate)

# Loading 3 files
emp.survey<- read.csv("employee_survey_data.csv", stringsAsFactors = F)
general<- read.csv("general_data.csv", stringsAsFactors = F)
mgr.survey<- read.csv("manager_survey_data.csv", stringsAsFactors = F)
in_time <- read.csv("in_time.csv",stringsAsFactors = F)
out_time <- read.csv("out_time.csv",stringsAsFactors = F)
#Change columnname X to EmployeeID
colnames(in_time)[1]<-"EmployeeID"
colnames(out_time)[1]<-"EmployeeID"


str(emp.survey)    # 4410 obs of 4 variables 
str(general) # 4410 obs of 24 variables
str(mgr.survey) # 4410 obs of 3 variables

# Collate the data together in one single file
length(unique(tolower(emp.survey$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(general$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(mgr.survey$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(in_time$EmployeeID))) # 4410, confirming X = EmployeeID is key
length(unique(tolower(out_time$EmployeeID))) # 4410, confirming X = EmployeeID is key

setdiff(general$EmployeeID,emp.survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general$EmployeeID,mgr.survey$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general$EmployeeID,out_time$EmployeeID) # Identical EmployeeID across these datasets
setdiff(out_time$EmployeeID,in_time$EmployeeID) # Identical EmployeeID across these datasets

hrdb<- merge(general,emp.survey, by="EmployeeID", all = F)
hrdb<- merge(hrdb,mgr.survey, by="EmployeeID", all = F)


# see total duplicate IDs
sum(duplicated(hrdb$EmployeeID)) #0

sapply(hrdb, function(x) length(which(x == "")))
#no blank value

colnames(hrdb)[which(sapply(hrdb,function(x) length(unique(x)) == 1))]
# [1] "EmployeeCount" "Over18"        "StandardHours"
# Lets remove these columns first
hrdb<- dplyr::select(hrdb, -c(which(sapply(hrdb,function(x) length(unique(x)) == 1))))


### Analysis on in and out timings of employees

# Remove days which were company holidays. Such days are characterized by all 
# rows being NA
in_time<- in_time[ , !sapply(in_time, function(x)all(is.na(x)))]
out_time<- out_time[ , !sapply(out_time, function(x)all(is.na(x)))]

#Convert in and out time table in  long formate
in_long<- gather(in_time,key=date,value=time,-EmployeeID)
out_long<- gather(out_time,key=date,value=time,-EmployeeID)

# Merge two tables by employee ID, Rename columns properly
in_out_time <- merge(in_long,out_long, by=c("EmployeeID","date"))
colnames(in_out_time) [3] <- "in.time"
colnames(in_out_time) [4] <- "out.time"

#Change date column: remove X from date and comvert string into date
in_out_time[,"date"]<-as.Date(sub("X","",in_out_time[,"date"]),format="%Y.%m.%d")

#Convert in and out time stamp into Date time 
in_out_time$in.time <- parse_date_time(x=as.character(in_out_time$in.time),orders="%Y-%m-%d %H:%M:%S")
in_out_time$out.time <- parse_date_time(x=as.character(in_out_time$out.time),orders="%Y-%m-%d %H:%M:%S")
in_out_time<-mutate(in_out_time,duration = out.time - in.time)

head(in_out_time,10)

holiday_frequency<-in_out_time %>% 
        group_by(EmployeeID) %>%
        summarise(NoOfLeaves = sum(is.na(in.time)),
                  AvgWorkHours = as.numeric(round(mean(duration,na.rm = T),2)))
hrdb<-cbind(hrdb,holiday_frequency[,2:3])



sapply(hrdb, function(x) sum(is.na(x)))
#NumCompaniesWorked:19; TotalWorkingYears:9; EnvironmentSatisfaction:25; JobSatisfaction:20
#WorkLifeBalance:38

#remove these observations from the analysis
#It means that 110/4410 = 0.02494331 i.e 2.4%, best is to remove these observations 
#from the analysis
hrdb <- hrdb[complete.cases(hrdb),]

sum(is.na(hrdb))

#View(hrdb) #master file
write.csv(hrdb,"MasterHR.csv",row.names = FALSE)

################################################################

## Converting following fetures in to Factors
# Attrition,BusinessTravel,Department,EducationField,Gender,JobRole,
# JobInvolvement,PerformanceRating,Education,JobLevel
col_names<-c("Attrition","Department","BusinessTravel",
             "Gender","Education","EducationField","EnvironmentSatisfaction",
             "JobInvolvement","JobLevel","JobRole","JobSatisfaction",
             "MaritalStatus","PerformanceRating","StockOptionLevel",
             "WorkLifeBalance")
columns <- hrdb[,col_names]
hrdb[,col_names] <- lapply(columns,factor)

############## EDA ##############-----------------------------------------------

### Data Preparation & Exploratory Data Analysis

# Understanding the structure of the collated file
str(hrdb) 

# Univariate and segmented Analysis

## Barcharts for categorical features with stacked HR information
bar_theme1<- theme(axis.text.x = element_text(angle = 45, hjust =1, vjust = 1), 
                   legend.position="none")
# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

plot_grid(ggplot(hrdb, aes(Age))+ geom_histogram(binwidth = 10),
          ggplot(hrdb, aes(MonthlyIncome))+ geom_histogram(binwidth = 10000)+bar_theme1,
          ggplot(hrdb, aes(DistanceFromHome)) + geom_histogram(binwidth = 5),
          ggplot(hrdb, aes(NumCompaniesWorked)) + geom_histogram(binwidth= 1),
          ggplot(hrdb, aes(x="",y=Age))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ggplot(hrdb, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ggplot(hrdb, aes(x="",y=DistanceFromHome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ggplot(hrdb, aes(x="",y=NumCompaniesWorked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 4)

plot_grid(ggplot(hrdb, aes(NoOfLeaves))+ geom_histogram(binwidth = 2),
          ggplot(hrdb, aes(AvgWorkHours))+ geom_histogram(binwidth = 0.5),
          ggplot(hrdb, aes(YearsWithCurrManager)) + geom_histogram(binwidth = 1),
          ggplot(hrdb, aes(YearsSinceLastPromotion)) + geom_histogram(binwidth = 1),
          ggplot(hrdb, aes(x="",y=NoOfLeaves))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ggplot(hrdb, aes(x="",y=AvgWorkHours))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          ggplot(hrdb, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ggplot(hrdb, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 4) 

plot_grid(ggplot(hrdb, aes(PercentSalaryHike))+ geom_histogram(binwidth = 2),
          ggplot(hrdb, aes(TotalWorkingYears))+ geom_histogram(binwidth = 1),
          ggplot(hrdb, aes(TrainingTimesLastYear)) + geom_histogram(binwidth = 1),
          ggplot(hrdb, aes(YearsAtCompany)) + geom_histogram(binwidth = 2),
          ggplot(hrdb, aes(x="",y=PercentSalaryHike))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ggplot(hrdb, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme,
          ggplot(hrdb, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          ggplot(hrdb, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 4) 

col_names<-c("Age","MonthlyIncome","DistanceFromHome","NumCompaniesWorked","NoOfLeaves",
             "AvgWorkHours","YearsWithCurrManager","YearsSinceLastPromotion",
             "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany")
summary(hrdb[,which(colnames(hrdb) %in% col_names)])



#Observations
#Age: Distributed well among age groups -- no outliers
#MonthlyIncome: Left skewed distribution  -- have outliers
#DistanceFromHome: Left skewed distribution -- no outliers
#NumCompaniesWorked: Left skewed distribution  -- have outliers
#NoOfLeaves: Distributed well among age groups -- no outliers
#AvgWorkHours: Left skewed distribution  -- have outliers
#YearsWithCurrManager:Left skewed distribution  -- have outliers
#YearsSinceLastPromotion:Left skewed distribution  -- have outliers
#PercentSalaryHike:Left skewed distribution  -- no outliers
#TotalWorkingYears:Left skewed distribution  -- have outliers
#TrainingTimesLastYear:Distributed well among age groups -- have outliers
#YearsAtCompany":Left skewed distribution  -- have outliers


outlier_col <-c ("MonthlyIncome","NumCompaniesWorked","AvgWorkHours","YearsWithCurrManager",
                 "YearsSinceLastPromotion","TotalWorkingYears","TrainingTimesLastYear",
                 "YearsAtCompany")


# Bivariate Analysis of continuous variables
# Correlation between numeric variables 
library(corrplot)
cor_matrix<-cor(hrdb[,which(colnames(hrdb) %in% col_names)])
corrplot(cor_matrix, type="upper", method = "color",
         order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black",
         tl.cex = .75, #Text size
         tl.srt=15,#Text label color and rotation
         number.cex = 0.5,
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         #Hide correlation coefficient on the principal diagonal
         diag=FALSE )

bar_chart<-geom_bar(stat = "count",position="stack") 
text_ann<- geom_text(stat= "count",aes(label=paste(round(..count../sum(..count..)*100,0),"%")),
                     position = position_stack(),size=3)
#Remove legend
bar_theme2<- theme(legend.position="none")

#Categorical data bar charts
plot_grid(
        ggplot(hrdb,aes(Department,fill=Attrition))+ bar_chart + text_ann+ theme_gray()+ bar_theme1 ,
        ggplot(hrdb,aes(EducationField,fill=Attrition))+ bar_chart + text_ann+ theme_gray() +bar_theme1  ,
        ggplot(hrdb,aes(JobRole,fill=Attrition))+ bar_chart + text_ann+theme_gray()+bar_theme1 ,
        ggplot(hrdb,aes(MaritalStatus,fill=Attrition))+bar_chart + text_ann+theme_gray()+bar_theme1,
        align = "h")

plot_grid(
        ggplot(hrdb,aes(Gender,fill=Attrition))+ bar_chart + text_ann+ theme_gray() +bar_theme2  ,
        ggplot(hrdb,aes(JobInvolvement,fill=Attrition))+ bar_chart + text_ann+theme_gray() +bar_theme2 ,
        ggplot(hrdb,aes(JobLevel,fill=Attrition))+ bar_chart + text_ann+theme_gray() +bar_theme2 ,
        ggplot(hrdb,aes(JobSatisfaction,fill=Attrition))+ bar_chart + text_ann +theme_gray()+bar_theme2,
        ggplot(hrdb,aes(BusinessTravel,fill=Attrition))+ bar_chart + text_ann +theme_gray(),
        align = "h")

plot_grid(
        ggplot(hrdb,aes(Education,fill=Attrition))+ bar_chart + text_ann+theme_gray() +bar_theme2 ,
        ggplot(hrdb,aes(EnvironmentSatisfaction,fill=Attrition))+bar_chart + text_ann+theme_gray() +bar_theme2,
        ggplot(hrdb,aes(PerformanceRating,fill=Attrition))+bar_chart + text_ann+theme_gray() +bar_theme2 ,
        ggplot(hrdb,aes(StockOptionLevel,fill=Attrition))+bar_chart + text_ann+theme_gray() +bar_theme2 ,      
        ggplot(hrdb,aes(WorkLifeBalance,fill=Attrition))+bar_chart + text_ann + theme_gray(),
        align = "h")


# Chi test
# H0: The The two variables are independent
# H1: The two variables relate to each other.
# p-value < 0.05  reject  the hypothesis -- variables are related to each other

chisq.test(hrdb$Education,hrdb$EducationField) #X-squared = 68.082, df = 20, p-value = 3.735e-07
chisq.test(hrdb$JobRole,hrdb$StockOptionLevel) #X-squared = 91.242, df = 24, p-value = 8.972e-10
chisq.test(hrdb$JobRole,hrdb$JobLevel) #X-squared = 87.289, df = 32, p-value = 5.054e-07
chisq.test(hrdb$JobRole,hrdb$JobInvolvement) # X-squared = 81.511, df = 24, p-value = 3.489e-08
chisq.test(hrdb$JobInvolvement,hrdb$JobSatisfaction) # X-squared = 22.345, df = 9, p-value = 0.007847
chisq.test(hrdb$JobRole,hrdb$Education) # X-squared = 101.64, df = 32, p-value = 3.545e-09
chisq.test(hrdb$JobSatisfaction,hrdb$Attrition) #X-squared = 50.946, df = 3, p-value = 5.023e-11
chisq.test(hrdb$Department,hrdb$Attrition)#X-squared = 24.026, df = 2, p-value = 6.066e-06

# Observations------------------------------
# Department : 3 levels  - independent variable
# EducationField : 6 levels -- correlated with Eduacation
# JobRole : 9 levels -- correlated with StockOptionLevel, Education
# MaritalStatus: 3 levels
# Gender: 2 levels
# JobInvolvement : 4 levels -- correlated with JobRole
# JobLevel: 5 levels -- correlated with JobRole
# JobSatisfaction: 4 levels
# Education: 5 levels - correlated with EducationField
# EnvironmentSatisfaction: 4 levels
# PerformanceRating: 2 levels
# StockOptionLevel:4 levels -- correlated with JobRole
# WorkLifeBalance: 4 levels
# BusinessTravel: 3 levels

################################################################
### Data Preparation

#Removing Outiliers

sapply(hrdb[,c("MonthlyIncome", "NumCompaniesWorked","AvgWorkHours","YearsWithCurrManager","YearsSinceLastPromotion","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) 

hrdb$MonthlyIncome[(which(hrdb$MonthlyIncome > 152020.0))] <- 152020.0 # jump from 91% to 92%
hrdb$AvgWorkHours[(which(hrdb$AvgWorkHours > 10.9))] <- 10.9 # jump from 99% to 100%
hrdb$YearsWithCurrManager[(which(hrdb$YearsWithCurrManager > 13))] <- 13 # jump from 99% to 100%
hrdb$YearsSinceLastPromotion[(which(hrdb$YearsSinceLastPromotion > 9))] <- 9 # jump from 95%  to 96%
hrdb$TotalWorkingYears[(which(hrdb$TotalWorkingYears > 32))] <- 32 # jump from 98%  to 99%
hrdb$YearsAtCompany[(which(hrdb$YearsAtCompany > 22.00))] <- 22.00 # jump from 97%   98%


levels(hrdb$JobInvolvement) <- c("Low","Medium","High","Very High")
levels(hrdb$Education) <- c("Below_College","College","Bachelor","Master","Doctor")
levels(hrdb$JobLevel) <- c("Level1","Level2","Level3","Level4","Level5")
levels(hrdb$StockOptionLevel)<-c("Level0","Level1","Level2","Level3")
levels(hrdb$EnvironmentSatisfaction)<-c("Low","Medium","High","Very High")
levels(hrdb$JobSatisfaction)<-c("Low","Medium","High","Very High")
levels(hrdb$WorkLifeBalance)<-c("Bad","Good","Better","Best")


# Factor with 2 levels -- converting values into numeric 0,1 
hrdb$Gender<-ifelse(hrdb$Gender=="Female",1,0)
hrdb$PerformanceRating<-ifelse(hrdb$PerformanceRating==3,0,1)
hrdb$Attrition<-ifelse(hrdb$Attrition=="No",0,1)

# Giving meaningful name to columns
colnames(hrdb)[9] <- "IsFemale"
colnames(hrdb)[26] <- "IsOutstandingPerformance"


#Creating dummy variables for all factors with more than 3 levels
col_names<-c("JobInvolvement","Education","JobLevel","StockOptionLevel",
             "EnvironmentSatisfaction","JobSatisfaction","WorkLifeBalance",
             "Department","EducationField","BusinessTravel","JobRole",
             "MaritalStatus")

dummies<-data.frame(sapply(hrdb[,col_names],
        function(x) data.frame(model.matrix(~x,data = hrdb[,col_names]))[,-1]))

hrdb.final<-cbind(hrdb[,-which(colnames(hrdb) %in% col_names)],dummies)

# Feature standardisation -- scaling
col_names<-c("Age","MonthlyIncome","DistanceFromHome","NumCompaniesWorked","NoOfLeaves",
             "AvgWorkHours","YearsWithCurrManager","YearsSinceLastPromotion",
             "PercentSalaryHike","TotalWorkingYears","TrainingTimesLastYear","YearsAtCompany")
hrdb.final[,col_names]<- data.frame(sapply(hrdb.final[,col_names], function(x) scale(x)))
hrdb.final <- hrdb.final[,-1] #Removing EmployeeID
str(hrdb.final)


########################################################################
# splitting the data between train and test
set.seed(100)

indices = sample.split(hrdb.final$Attrition, SplitRatio = 0.7)

train = hrdb.final[indices,]

test = hrdb.final[!(indices),]

########################################################################


########################################################################
# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2121....31 coeff..nullDev 2661.4...resDev 2007.0

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2) #AIC: 2094.3

# Removing multicollinearity through VIF check
sort(vif(model_2), decreasing  = T)

#Excluding EducationField.xLife.Sciences with highestvif, and high p value
model_3<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + Education.xDoctor + 
                JobLevel.xLevel2 + JobLevel.xLevel5 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + Department.xResearch...Development + 
                Department.xSales + EducationField.xMarketing + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_3)  #AIC: 2095.2

sort(vif(model_3), decreasing  = T)

#Excluding EducationField.xMarketing with high p value
model_4<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + Education.xDoctor + 
                JobLevel.xLevel2 + JobLevel.xLevel5 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + Department.xResearch...Development + 
                Department.xSales + 
                EducationField.xMedical + EducationField.xOther + EducationField.xTechnical.Degree + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_4)  #AIC: 2094.3

sort(vif(model_4), decreasing  = T)


#Excluding EducationField.xMedical with high p value
model_5<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + Education.xDoctor + 
                JobLevel.xLevel2 + JobLevel.xLevel5 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + Department.xResearch...Development + 
                Department.xSales + 
                EducationField.xOther + EducationField.xTechnical.Degree + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_5)  #AIC: 2093

sort(vif(model_5), decreasing  = T)

#Excluding EducationField.xOther high p value
model_6<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + Education.xDoctor + 
                JobLevel.xLevel2 + JobLevel.xLevel5 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + Department.xResearch...Development + 
                Department.xSales + 
                EducationField.xTechnical.Degree + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_6)  #AIC: 2092

sort(vif(model_6), decreasing  = T)

#Excluding EducationField.xTechnical.Degree high p value
model_6<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + Education.xDoctor + 
                JobLevel.xLevel2 + JobLevel.xLevel5 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest + Department.xResearch...Development + 
                Department.xSales + 
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_6)  #AIC: 2091.4


sort(vif(model_6), decreasing  = T)

#Excluding JobLevel.xLevel5e high p value
model_7<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + Education.xDoctor + 
                JobLevel.xLevel2 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales + Department.xResearch...Development +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xHuman.Resources + JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_7)  #AIC: 2092.5

sort(vif(model_7), decreasing  = T)

#Excluding JobRole.xHuman.Resources high p value
model_8<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + Education.xDoctor + 
                JobLevel.xLevel2 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales + Department.xResearch...Development +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_8)  #AIC: 2094.2

sort(vif(model_8), decreasing  = T)

#Excluding JobRole.xHuman.Resources high p value
model_9<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + 
                JobLevel.xLevel2 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales + Department.xResearch...Development +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManager + JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_9)  #AIC: 2096.2

sort(vif(model_9), decreasing  = T)

#Excluding JobRole.xManager high p value
model_10<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + 
                JobLevel.xLevel2 + StockOptionLevel.xLevel1 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales + Department.xResearch...Development +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_10)  #AIC: 2098.6

sort(vif(model_10), decreasing  = T)

#Excluding StockOptionLevel.xLevel1 high p value
model_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours + JobInvolvement.xHigh + 
                JobLevel.xLevel2 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales + Department.xResearch...Development +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_11)  #AIC: 2101.7

sort(vif(model_11), decreasing  = T)

#Excluding JobInvolvement.xHigh high p value
model_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                JobLevel.xLevel2 + 
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales + Department.xResearch...Development +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_12)  #AIC: 2106.2

sort(vif(model_12), decreasing  = T)

#Excluding JobLevel.xLevel2 high p value
model_13<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales + Department.xResearch...Development +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_13)  #AIC: 2110.4

sort(vif(model_13), decreasing  = T)


#Excluding Department.xResearch...Development high p value & vif
model_14<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                Department.xSales +
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_14)  #AIC: 2122.7

sort(vif(model_14), decreasing  = T)


#Excluding Department.xResearch...Development high p value & vif
model_15<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                
                BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_15)  #AIC: 2120.8

sort(vif(model_15), decreasing  = T)


#Excluding BusinessTravel.xTravel_Rarely high p value & vif
model_16<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High + WorkLifeBalance.xGood + 
                WorkLifeBalance.xBetter + WorkLifeBalance.xBest +  
                
                BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_16)  #AIC: 2129.6

sort(vif(model_16), decreasing  = T)


#Excluding WorkLifeBalance.xGood & WorkLifeBalance.xBetter for high vif
model_17<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High  + 
                WorkLifeBalance.xBetter  +  
                
                BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director + JobRole.xSales.Executive + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_17)  #AIC: 2148.3

sort(vif(model_17), decreasing  = T)



#Excluding JobRole.xSales.Executive  for high p value
model_18<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High  + 
                WorkLifeBalance.xBetter  +  
                
                BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + 
                JobRole.xResearch.Director  + MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_18)  #AIC: 2152.1

sort(vif(model_18), decreasing  = T)


#Excluding TotalWorkingYears  for high vif
model_19<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + JobSatisfaction.xMedium + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High  + 
                WorkLifeBalance.xBetter  +  
                
                BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_19)  #AIC: 2181.3

sort(vif(model_19), decreasing  = T) #### I  am here

#Excluding JobSatisfaction.xMedium  for high p value
model_20<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xHigh + JobSatisfaction.xVery.High  + 
                WorkLifeBalance.xBetter  +  
                
                BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_20)  #AIC: 2190.1

sort(vif(model_20), decreasing  = T)


#Excluding JobSatisfaction.xHigh  for high p value
model_21<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xVery.High  + 
                WorkLifeBalance.xBetter  +  
                
                BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_21)  #AIC: 2192.6

sort(vif(model_21), decreasing  = T)

#Excluding WorkLifeBalance.xBetter  for high p value
model_22<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                AvgWorkHours +
                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
                EnvironmentSatisfaction.xVery.High + 
                JobSatisfaction.xVery.High  + 
                BusinessTravel.xTravel_Frequently + 
                JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)

summary(model_22)  #AIC: 2200.2

sort(vif(model_22), decreasing  = T) #### i am here


#Excluding TrainingTimesLastYear  for high p value
#model_7<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
#                YearsSinceLastPromotion + YearsWithCurrManager + 
#                AvgWorkHours +
#                EnvironmentSatisfaction.xMedium + EnvironmentSatisfaction.xHigh + 
#                EnvironmentSatisfaction.xVery.High + 
#                JobSatisfaction.xVery.High  + 
#                BusinessTravel.xTravel_Frequently + 
#               
#                MaritalStatus.xSingle, 
#              family = "binomial", data = train)

#summary(model_7)  #AIC: 2223.9

#sort(vif(model_7), decreasing  = T)


########################################################################
# With  significant variables in the model

final_model<- model_22

#######################################################################

### Model Evaluation

### Test Data ####

#predicted probabilities of Attrition 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2]) # 2nd column in Attrition

# Let's see the summary 

summary(test_pred)

test$prob <- test_pred
View(test)
# Let's use the probability cutoff of 50%.

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)


#######################################################################
test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- caret::confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

#   Accuracy : 0.8519   
#Sensitivity : 0.37321         
# Specificity : 0.94450 


#########################################################################################
# Let's Choose the cutoff value. 


# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}



# Creating cutoff values from 0.000807 to 0.815725 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.16)]

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
abline(v=0.17,lty=3)
box()
legend("right",col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
title("Cutoff Value")
    
# Let's choose a cutoff value of 0.169596 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.169596, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

(acc <- conf_final$overall[1]) #0.7465116

(sens <- conf_final$byClass[1])#0.7559809

(spec <- conf_final$byClass[2]) #0.7446809


##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)


library(ROCR)
#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test) #0.5006617 which is greater than 40%
plot(performance_measures_test)
title("ROC curve")

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
          summarise_at(vars(labels ), funs(observations = n(),
                                           attr=sum(., na.rm = TRUE))) %>%
          
          mutate(Cum_attr = cumsum(attr),
                 Gain=Cum_attr/sum(attr)*100,
                 Cumlift=Gain/(bucket*(100/groups)),
                 Cum_non_attr = cumsum(observations - attr),
                 Gain_non_attr = Cum_non_attr/sum(observations - attr) *100,
                 KS = Gain - Gain_non_attr)
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)
Attrition_decile = as.data.frame(Attrition_decile)


#Lift Plot
ggplot(Attrition_decile,aes(x=bucket,y=Cumlift)) +
        geom_line() + scale_x_discrete(name ="Decile", 
                                       limits=seq(1:10)) +
        theme_gray()+
        labs(y="Lift")


#Gain Plot
ggplot(Attrition_decile,aes(x=bucket,y=Gain)) +
        geom_line() + scale_x_discrete(name ="Decile", 
                                       limits=seq(1:10)) +
        theme_gray()+
        labs(y="Gain")

#KS Plot
ggplot(Attrition_decile,aes(x=bucket,y=KS)) +
        geom_line() + scale_x_discrete(name ="Decile", 
                                       limits=seq(1:10)) +
        theme_gray()+
        labs(y="KS")        
