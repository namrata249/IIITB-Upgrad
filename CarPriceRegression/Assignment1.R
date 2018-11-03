#setwd("~/Documents/Learning/IIITb/RFiles/Datafiles/Regression_assignment")

remove(list = ls())


#Load libraries-----------------------------
library(stringr)
library(MASS)
library(car)
library(ggplot2)
library(dplyr)

#import data / data cleaning -------------------------
cars_raw<-read.csv("CarPrice_Assignment.csv",stringsAsFactors = FALSE)
#View(cars)
str(cars_raw)

#check for duplicate records
#here unique records are 205 = total records => no diplicate records found
unique(cars_raw)

#check for null record
# There is no na records found.
sum(is.na(cars_raw))


#check unique values per column 
sapply(cars_raw,function(x){return(length(unique(x)))})
#carname has 147 unique values from 205 total record -- 
#need to remove after extracting company name

#check for null records
sapply(cars_raw,function(x)sum(is.na(x)),simplify = T)

#Derived metrics company
#creating company from CarName (companyName + model name)
#Deleteing CarName and ID -not useful 
cars_raw$company<-str_to_lower(gsub("([A-Za-z]+).*", "\\1", cars_raw$CarName))
cars<-cars_raw[,-c(1,3)] # removing #CarName,car_ID

table(cars$company)
# clearly there are companies which are miss spelled 
# Correcting those mistakes
cars$company[which(cars$company=="toyouta")]<-"toyota"
cars$company[which(cars$company=="maxda")]<-"mazda"
cars$company[which(cars$company=="porcshce")]<-"porsche"
cars$company[which(cars$company%in%c("vokswagen","vw"))]<-"volkswagen"
unique(cars$company)
cars$company<-str_trim(cars$company,side="both")

# numeric datapoint summary---------------------------------
col_names<-c('wheelbase','carlength','carwidth','carheight','curbweight',
             'enginesize','boreratio','stroke','compressionratio','horsepower',
             'peakrpm','citympg','highwaympg','price')

numeric_var_summary<-do.call(cbind, lapply(cars[,col_names], summary))
numeric_var_summary<-t(numeric_var_summary)
numeric_var_summary
#                      Min. 1st Qu.   Median         Mean  3rd Qu.     Max.
# wheelbase          86.60   94.50    97.00    98.756585   102.40   120.90
# carlength         141.10  166.30   173.20   174.049268   183.10   208.10
# carwidth           60.30   64.10    65.50    65.907805    66.90    72.30
# carheight          47.80   52.00    54.10    53.724878    55.50    59.80
# curbweight       1488.00 2145.00  2414.00  2555.565854  2935.00  4066.00
# enginesize         61.00   97.00   120.00   126.907317   141.00   326.00
# boreratio           2.54    3.15     3.31     3.329756     3.58     3.94
# stroke              2.07    3.11     3.29     3.255415     3.41     4.17
# compressionratio    7.00    8.60     9.00    10.142537     9.40    23.00
# horsepower         48.00   70.00    95.00   104.117073   116.00   288.00
# peakrpm          4150.00 4800.00  5200.00  5125.121951  5500.00  6600.00
# citympg            13.00   19.00    24.00    25.219512    30.00    49.00
# highwaympg         16.00   25.00    30.00    30.751220    34.00    54.00
# price            5118.00 7788.00 10295.00 13276.710571 16503.00 45400.00
# 
#Looking at median and mean -- enginesize, compressionratio, price have higher 
#mean than median-- they have some outliers
#Major difference is in price  3rd Qu 16,503 and max 45,400 

# outliers tratment------------------------------------------------------------

ggplot(cars,aes(x=price)) + geom_histogram(binwidth = 5000)

quantile(cars$price,probs = seq(0,1,0.01)) #sudden jump from 92 to 93%
cars$price[which(cars$price>27336.32)]<-27336.32


# most of the car priced in range of  5,000 to 15,000
boxplot(cars$enginesize)
quantile(cars$enginesize,probs = seq(0,1,0.01)) #sudden jump from 96 to 97%
cars$enginesize[which(cars$enginesize>209)]<-209
#
boxplot(cars$compressionratio)
quantile(cars$compressionratio,probs = seq(0,1,0.01)) #sudden jump from 90 to 91%
cars$compressionratio[which(cars$compressionratio>10.94)]<-10.94

#Categorical variables exploration----------------------------------------------
#Convert categorical variables into factors
col_names<-c("symboling","fueltype","aspiration","doornumber","carbody",
             "drivewheel","enginelocation","enginetype","cylindernumber",
             "fuelsystem","company")
columns <- cars[,col_names]
cars[,col_names] <- lapply(columns,factor)
summary(cars[,col_names])
# symboling : -2,-1,0,1,2,3
# fueltype : diesel,gas
# doornumber: std,turbo
# carbody : convertible,hardtop,hatchback,sedan,wagon
# drivewheel : 4wd, fwd, rwd
# enginelocation: front, rear
# enginetype  dohc,dohcv, l,ohc,ohcf,ohcv,rotor
# cylindernumber : eight, four,six,three, twelve,two
# fuelsystem  8 levels mpfi, 2bbl,idi,..
# company 25 levels "alfa","audi",..
# 
car_1 <- cars
cars<-car_1
ggplot(cars,aes(x=symboling))+geom_bar(stat="count")
#we can consider (-1,-2) as safe (0,1) neutral (2,3) unsafe
cars$symboling<-as.numeric(levels(cars$symboling)[cars$symboling])
cars$symboling[which(cars$symboling%in%c(-2,-1))]<-"safe"
cars$symboling[which(cars$symboling%in%c(0,1))]<-"neutral"
cars$symboling[which(cars$symboling%in%c(2,3))]<-"unsafe"
summary(as.factor(cars$symboling))

ggplot(cars,aes(x=fueltype))+geom_bar(stat="count")
#most cars have gas fuel type
ggplot(cars,aes(x=doornumber))+geom_bar(stat="count")
#no of cars distributed euallly amonth two and four door types
ggplot(cars,aes(x=carbody))+geom_bar(stat="count")
table(cars$carbody)
#combining hardtop wagon,hardtop and convertible in catagory "other"
cars$carbody<-as.character(levels(cars$carbody)[cars$carbody])
cars$carbody[which(cars$carbody%in%c("convertible","hardtop","wagon"))] <-"other"

ggplot(cars,aes(x=drivewheel))+geom_bar(stat="count")+
        geom_text(stat="count",aes(label=..count..))
#only 9 cars from 205 has "4wd" 
ggplot(cars,aes(x=enginelocation))+geom_bar(stat="count")+ geom_text(stat="count",aes(label=..count..))
#very less rear engine location, no dummy variable is needed 

cars%>%group_by(enginetype)%>%summarise(cnt=n())%>%
        ggplot(aes(x=reorder(enginetype,-cnt),y=cnt))+
        geom_bar(stat="identity") +geom_text(stat="identity",aes(label=cnt))
#ohc is most common enginetype 
#lets convert this into 3 categories ohc, ohc_variant, other
cars$enginetype<-as.character(levels(cars$enginetype)[cars$enginetype])
cars$enginetype[which(cars$enginetype%in%c("ohcf","ohcv","dohc","dohcv"))] <-"ohc_variant"
cars$enginetype[which(cars$enginetype%in%c("l","rotor"))] <-"other"
table(cars$enginetype)

cars%>%group_by(cylindernumber)%>%summarise(cnt=n())%>%
        ggplot(aes(x=reorder(cylindernumber,-cnt),y=cnt))+
        geom_bar(stat="identity") +geom_text(stat="identity",aes(label=cnt))
#categorized it again into 3 categories two_to_four, five_six,eight_twelve
cars$cylindernumber<-as.character(levels(cars$cylindernumber)[cars$cylindernumber])
cars$cylindernumber[which(cars$cylindernumber%in%c("two","three","four"))] <-"two_to_four"
cars$cylindernumber[which(cars$cylindernumber%in%c("five","six"))] <-"five_six"
cars$cylindernumber[which(cars$cylindernumber%in%c("eight","twelve"))] <-"eight_twelve"
table(cars$cylindernumber)

cars%>%group_by(fueltype)%>%summarise(cnt=n())%>%
        ggplot(aes(x=reorder(fueltype,-cnt),y=cnt))+
        geom_bar(stat="identity") +geom_text(stat="identity",aes(label=cnt))

cars%>%group_by(fuelsystem)%>%summarise(cnt=n())%>%
        ggplot(aes(x=reorder(fuelsystem,-cnt),y=cnt))+
        geom_bar(stat="identity") +geom_text(stat="identity",aes(label=cnt))
cars$fuelsystem<-as.character(levels(cars$fuelsystem)[cars$fuelsystem])
cars$fuelsystem[which(cars$fuelsystem%in%c("mpfi","mfi","spfi"))] <-"mfi"
cars$fuelsystem[which(cars$fuelsystem%in%c("spdi","idi"))] <-"di"
cars$fuelsystem[which(cars$fuelsystem%in%c("4bbl","2bbl","1bbl"))] <-"bbl"

cars%>%group_by(company)%>%summarise(cnt=n())%>%
        ggplot(aes(x=reorder(company,-cnt),y=cnt))+
        geom_bar(stat="identity") +geom_text(stat="identity",aes(label=cnt))
# making 5 categories of companies based on frequency
company_freq<-function(freq){
        return( case_when(
                between(freq,0,5) ~"very_low",
                between(freq,6,10) ~"low",
                between(freq,11,15) ~"medium",
                between(freq,16,20) ~"high",
                between(freq,21,35) ~ "very_high",
                TRUE ~ "NA"))
}
cars<-group_by(cars,company)%>%
        mutate(company.freq=company_freq(n()))

cars%>%group_by(company.freq)%>%summarise(cnt=n())%>%
        ggplot(aes(x=reorder(company.freq,-cnt),y=cnt))+
        geom_bar(stat="identity") +geom_text(stat="identity",aes(label=cnt))

#Convert categorical variables into factors
col_names<-c("symboling","carbody",
             "enginetype","cylindernumber",
             "fuelsystem","company.freq")
columns <- cars[,col_names]
cars[,col_names] <- lapply(columns,factor)


# Check values against price  segment univariate analysis---------------------

#numerical values-----------------------------------
col_names<-c("symboling","fueltype","aspiration","doornumber","carbody",
             "drivewheel","enginelocation","enginetype","cylindernumber",
             "fuelsystem","company","company.freq")
cor_var<-cars[,-which(colnames(cars)%in%col_names)]
c<-round(cor(cor_var,use="complete.obs"),2)

write.csv(c,"car_price_correlation.csv")
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(c, method="color",
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

### highly correlated variables
### citympg = highwaympg
### enginsize = curbweight
### curbweight = carlength
### carlength = wheelbase


#peakrpm,stroke,compressionratio are not significantly correlated with price 
#we can remove them
cars1<-cars[,-which(colnames(cars) == "company")]
cars<-cars1

dev.off()
library(GGally)
#ggpairs(data.frame(c))
#categorical values ---------------------------------

ggplot(cars,aes(x=reorder(company.freq,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(symboling,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(aspiration,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(doornumber,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(carbody,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(drivewheel,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(enginelocation,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(enginetype,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(cylindernumber,-price),y=price))+geom_boxplot()
ggplot(cars,aes(x=reorder(fuelsystem,-price),y=price))+geom_boxplot()


## Company.freq: from low to high freq  price is less distributed and decrement in price
##symboling : safe has high price range and neutral has less
##aspiration : turbo has high price than std
##doornumber : price distribition is same for both categories, four door has little high price 
##carbody : sedan has higher prices than hatchback
##drivewheel : rwd has higher prices, 4wd and fwd prices are less distributed and low
##enginelocation : rear has very high price thant front, price is distributed well for front
##enginetype : ohc has price outliers - price is distributed from low to very high
##cylindernumber : price increases from low to high cylinder numbers

# Create dummy variables for model generation -----------------------------

# #converting 2 level ordered categorical variables in to numeric variables
levels(cars$fueltype)<-c(0,1)
levels(cars$aspiration)<-c(0,1)
levels(cars$doornumber)<-c(0,1)
levels(cars$enginelocation)<-c(0,1)

convert_factor_to_numeric<-function(x){return(as.numeric(levels(x)[x]))}
col_names<-c("fueltype","aspiration","doornumber","enginelocation")
columns <- cars[,col_names]
cars[,col_names] <- lapply(columns,convert_factor_to_numeric)

dummy_1 <- data.frame(model.matrix( ~carbody, data = cars))[,-1]
cars_1 <- cbind(cars[,-which(colnames(cars)=="carbody")], dummy_1)

dummy_2 <- data.frame(model.matrix( ~drivewheel, data = cars_1))[,-1]
cars_2 <- cbind(cars_1[,-which(colnames(cars_1)=="drivewheel")], dummy_2)

dummy_3 <- data.frame(model.matrix( ~enginetype, data = cars_2))[,-1]
cars_3 <- cbind(cars_2[,-which(colnames(cars_2)=="enginetype")], dummy_3)

dummy_4 <- data.frame(model.matrix( ~fuelsystem, data = cars_3))[,-1]
cars_4 <- cbind(cars_3[,-which(colnames(cars_3)=="fuelsystem")], dummy_4)

dummy_5 <- data.frame(model.matrix( ~company.freq, data = cars_4))[,-1]
cars_5 <- cbind(cars_4[,-which(colnames(cars_4)=="company.freq")], dummy_5)

dummy_6 <- data.frame(model.matrix( ~symboling, data = cars_5))[,-1]
cars_6 <- cbind(cars_5[,-which(colnames(cars_5)=="symboling")], dummy_6)

dummy_7 <- data.frame(model.matrix( ~cylindernumber, data = cars_6))[,-1]
cars_7 <- cbind(cars_6[,-which(colnames(cars_6)=="cylindernumber")], dummy_7)

# Model building --------------------------------------------------------------
r_sqr <- function(predict){return(cor(test$price,predict)^2)}
# separate training and testing data
set.seed(100)
trainindices<- sample(1:nrow(cars_7), 0.7*nrow(cars_7))
train <- cars_7[trainindices,]
test <- cars_7[-trainindices,]


model_1<-lm(price~.,data=train)
summary(model_1)
vif(model_1)
        
step <- stepAIC(model_1, direction="both")
step 

model_2 <- lm(formula = price ~ aspiration + enginelocation + carlength + 
                      carwidth + curbweight + stroke + compressionratio + horsepower + 
                      citympg + carbodysedan + drivewheelfwd + drivewheelrwd + 
                      enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                      company.freqlow + company.freqvery_high + company.freqvery_low + 
                      cylindernumberfive_six + cylindernumbertwo_to_four, data = train)

summary(model_2)
sort(vif(model_2))
r_sqr( predict(model_2,test[,-which(colnames(test)=="price")]))

# removing high vif variable cylindernumbertwo_to_four vif 18.5 p-value =0.002
model_3<-  lm(formula = price ~ aspiration + enginelocation + carlength + 
                      carwidth + curbweight + stroke + compressionratio + horsepower + 
                      citympg + carbodysedan + drivewheelfwd + drivewheelrwd + 
                      enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                      company.freqlow + company.freqvery_high + company.freqvery_low + 
                      cylindernumberfive_six , data = train)

summary(model_3)
sort(vif(model_3))
r_sqr( predict(model_3,test[,-which(colnames(test)=="price")]))
# curbweight has high vif = 16  although it IS VERY SIGNIFICANT
# removing for now 
model_4<-   lm(formula = price ~ aspiration + enginelocation + carlength + 
                       carwidth  + stroke + compressionratio + horsepower + 
                       citympg + carbodysedan + drivewheelfwd + drivewheelrwd + 
                       enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                       company.freqlow + company.freqvery_high + company.freqvery_low + 
                       cylindernumberfive_six , data = train)
summary(model_4)
sort(vif(model_4))
r_sqr( predict(model_4,test[,-which(colnames(test)=="price")]))

# removing horsepower has vif 8.3
model_5<- lm(formula = price ~ aspiration + enginelocation + carlength + 
                     carwidth  + stroke + compressionratio  + 
                     citympg + carbodysedan + curbweight + drivewheelrwd + 
                     enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                     company.freqlow + company.freqvery_high + company.freqvery_low + 
                     cylindernumberfive_six , data = train)

summary(model_5)
sort(vif(model_5))
r_sqr( predict(model_5,test[,-which(colnames(test)=="price")]))

# removing carlength high vif and high p-value
model_6<- lm(formula = price ~ aspiration + enginelocation + carlength + 
                     carwidth  + stroke + compressionratio  + 
                     citympg + carbodysedan + drivewheelfwd  + 
                     enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                     company.freqlow + company.freqvery_high + company.freqvery_low + 
                     cylindernumberfive_six , data = train)

summary(model_6)
vif(model_6)

# removing citympg very high p-value .9
model_7<- lm(formula = price ~ aspiration + enginelocation + carlength + 
                     carwidth  + stroke + compressionratio  +
                      carbodysedan + drivewheelfwd  + 
                     enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                     company.freqlow + company.freqvery_high + company.freqvery_low + 
                     cylindernumberfive_six , data = train)
summary(model_7)
vif(model_7)
r_sqr( predict(model_7,test[,-which(colnames(test)=="price")]))

#removing carwidth  higher vif
model_8<-   lm(formula = price ~ aspiration + enginelocation + carlength + 
                        stroke   + compressionratio  + 
                       carbodysedan + drivewheelfwd  + 
                       enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                       company.freqlow + company.freqvery_high + company.freqvery_low + 
                       cylindernumberfive_six , data = train)

summary(model_8)
vif(model_8)
# now all VIF values are around 2
# lets see p-values
# car length has least p-value in this model but in revious models it was not 
# in model 4 we have removed curbweight which had loest p-value
cor(train$carlength,train$curbweight) #0.8683094
#lets replce  curbweight  with carlength
model_9<-lm(formula = price ~ aspiration + enginelocation  + 
                    stroke   + compressionratio  + curbweight +
                    carbodysedan + drivewheelfwd  + 
                    enginetypeohc_variant + enginetypeother + fuelsystemdi + 
                    company.freqlow + company.freqvery_high + company.freqvery_low + 
                    cylindernumberfive_six , data = train)

summary(model_9)
vif(model_9)
r_sqr( predict(model_9,test[,-which(colnames(test)=="price")]))

# enginetypeohc_variant has higher p value  0.57- remove it
model_10<- lm(formula = price ~ aspiration + enginelocation  + 
                    stroke   + compressionratio  + curbweight +
                    carbodysedan + drivewheelfwd  + 
                     enginetypeother + fuelsystemdi + 
                    company.freqlow + company.freqvery_high + company.freqvery_low + 
                    cylindernumberfive_six , data = train)

summary(model_10)
vif(model_10)
r_sqr( predict(model_10,test[,-which(colnames(test)=="price")]))
# looking at higher p-values
# removinh strok p-value 0.14
model_11<- lm(formula = price ~ aspiration + enginelocation  + 
                      compressionratio  + curbweight +
                      carbodysedan + drivewheelfwd  + 
                      enginetypeother + fuelsystemdi + 
                      company.freqlow + company.freqvery_high + company.freqvery_low + 
                      cylindernumberfive_six , data = train)
summary(model_11)
vif(model_11)
r_sqr( predict(model_11,test[,-which(colnames(test)=="price")]))

#removing carbodysedan p-value 0.16
model_12<- lm(formula = price ~ aspiration + enginelocation  + 
                      compressionratio  + curbweight +
                      drivewheelfwd  + 
                      enginetypeother + fuelsystemdi + 
                      company.freqlow + company.freqvery_high + company.freqvery_low + 
                      cylindernumberfive_six , data = train)
summary(model_12)
vif(model_12)
r_sqr( predict(model_12,test[,-which(colnames(test)=="price")]))

#remove company.freqvery_low as it is less significant
model_13<-lm(formula = price ~ aspiration + enginelocation  + 
                     compressionratio  + curbweight +
                     drivewheelfwd  + 
                     enginetypeother + fuelsystemdi + 
                     company.freqlow + company.freqvery_high + 
                     cylindernumberfive_six , data = train)
summary(model_13)
vif(model_13)
r_sqr( predict(model_13,test[,-which(colnames(test)=="price")]))

cor(train$aspiration,train$price) # 0.3064824
cor(train$drivewheelfwd,train$price) # -0.6539354

#drivewheelfwd having -ve coefficients 
#removing aspiration
model_14<- lm(formula = price ~  enginelocation  + 
                      compressionratio  + curbweight +
                      drivewheelfwd  + 
                      enginetypeother + fuelsystemdi + 
                      company.freqlow + company.freqvery_high + 
                      cylindernumberfive_six , data = train)
summary(model_14)
vif(model_14)
r_sqr( predict(model_14,test[,-which(colnames(test)=="price")]))


# lets remove fuelsystemdi which has comparatively big p-value
model_15<- lm(formula = price ~  enginelocation  + 
                      compressionratio  + curbweight +
                      drivewheelfwd  + 
                      enginetypeother  + 
                      company.freqlow + company.freqvery_high + 
                      cylindernumberfive_six , data = train)
summary(model_15)
vif(model_15)
r_sqr( predict(model_15,test[,-which(colnames(test)=="price")]))

# all p-values are less than 0.05
#compressionratio is least significant 
cor(train$compressionratio,train$price) #-0.03637058
model_16<- lm(formula = price ~  enginelocation  + 
                      curbweight +
                      drivewheelfwd  + 
                      enginetypeother  + 
                      company.freqlow + company.freqvery_high + 
                      cylindernumberfive_six , data = train)
summary(model_16)
vif(model_16)
r_sqr( predict(model_16,test[,-which(colnames(test)=="price")]))


cor(train$price,train$drivewheelfwd) #-0.6539354
cor(train$price,train$drivewheelrwd) #0.662393
#lets use drivewheelrwd instead of drivewheelfwd 
model_17<- lm(formula = price ~  enginelocation  + 
                      curbweight +
                      drivewheelrwd  + 
                      enginetypeother  + 
                      company.freqlow + company.freqvery_high + 
                      cylindernumberfive_six , data = train)
summary(model_17)
vif(model_17)
r_sqr( predict(model_17,test[,-which(colnames(test)=="price")]))

cor(train$price,train$enginetypeother) #0.03003212
#lets remove enginetypeother relatively high p-value
model_18<- lm(formula = price ~  enginelocation  + 
                      curbweight +
                      drivewheelrwd  + 
                      company.freqlow + company.freqvery_high + 
                      cylindernumberfive_six , data = train)
summary(model_18)
vif(model_18)
r_sqr( predict(model_18,test[,-which(colnames(test)=="price")]))


#lets remove -ve coefficient value by removing company.freqvery_high
model_19<- lm(formula = price ~  enginelocation  + curbweight + drivewheelrwd  + 
                      company.freqlow  + cylindernumberfive_six , data = train)
summary(model_19)
vif(model_19)
r_sqr( predict(model_19,test[,-which(colnames(test)=="price")]))

#since company.freqlow has very low p-value 
# add company.freqvwry_low
model_20<- lm(formula = price ~  enginelocation  + curbweight + drivewheelrwd  + 
                      company.freqlow + company.freqvery_low  + 
                      cylindernumberfive_six , data = train)
summary(model_20)
vif(model_20)
r_sqr( predict(model_20,test[,-which(colnames(test)=="price")]))

summary(model_20)$coefficients
#all coefficients are positive 
#increment in value will also increase car price.
# car price is dependent on 5 variables :

# 1) curb weight 
# 2) engine location 
# 3) company's no of car models 
# 4) Drive wheels 
# 5) no of cylenders 

