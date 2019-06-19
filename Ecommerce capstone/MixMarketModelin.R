################################################################################
#
# E-Commerce Capstone
# Group E-Comm
# 
# Business Understanding
# 
# ElecKart is an e-commerce firm specialising in eletctronic products. Over the 
# last one year, they had spent a significant amount of money in marketing. 
# Occasionally, they had also offered big-ticket promotions (similar to the Big 
# Billion Day). They are about to create a marketing budget for the next year 
# which includes spending on commercials, online campaigns, and pricing & 
# promotion strategies. The CFO feels that the money spent over last 12 months 
# on marketing was not sufficiently impactful, and, that they can either cut on 
# the budget or reallocate it optimally across marketing levers to improve the 
# revenue response.
#  
# Capstone Objective
# 
# Develop a market mix model by analyzing the data from 2015 thru 2016 which 
# will optimize the spend on advertising via different channels and increase the 
# revenue

################################################################################

# Import libraries and set working directory -----------------------------------
rm(list=ls())

list.of.packages <- c( "ggpubr","ggpmisc","gdata","data.table","stringr",
                       "dplyr","dplyr","lubridate","readxl","gridExtra","grid",
                       "corrplot","ggplot2","lattice","zoo","Hmisc","tidyr",
                       "MASS","car","caret","DataCombine","PerformanceAnalytics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(item in list.of.packages) library(item,character.only = TRUE)

f_count_na <- function(x) {return(sum(is.na(x)))}
f_count_blank <-function(x) {return(length(which(str_trim(x, side = "both") == "")))}
f_tolower<-function(x){return(ifelse(is.character(x),tolower(x),x))}
'%!in%' <- function(x,y)!('%in%'(x,y))
f_freq_dist<-function(x,br){ 
        ranges = paste(head(br,-1), br[-1], sep=" - ")
        freq   = hist(x, breaks=br, include.lowest=TRUE, plot=FALSE)
        return(data.frame(range = ranges, frequency = freq$counts))}
f_remove_outliers<-function(x,prc){ q<-quantile(x, prc)
x[which(x> q)]<-q
return(x)}
# Data import ------------------------------------------------------------------
raw_data <- data.table(read.csv("ConsumerElectronics.csv", stringsAsFactors = FALSE))
# ------------------------------------------------------------------------------
#   Data exploration
#-------------------------------------------------------------------------------
ecart_data<-as.data.frame(raw_data)

dim(ecart_data)   #566067     20
str(ecart_data)
summary(ecart_data)

#1 check for blank values -----
sapply(ecart_data,f_count_blank, simplify = "array")
#2.check for null values------
sapply(ecart_data,f_count_na, simplify = "array") #4904

dim(ecart_data[which(is.na(ecart_data$gmv)&
                             is.na(ecart_data$cust_id)&is.na(ecart_data$pincode)),
               c("gmv","cust_id","pincode")])
#remove na data
ecart_data<-ecart_data[-which(is.na(ecart_data$cust_id)),]

#3.check distinct values in each columns ------------
sapply(ecart_data,function(x) length(unique(x)), simplify = "array") 
# fsn_id                      order_date                            Year 
# 21216                         1152699                               2 
# Month                        order_id                   order_item_id 
# 12                         1497298                         1477119 
# gmv                           units                   deliverybdays 
# 12523                              27                             143 
# deliverycdays      s1_fact.order_payment_type                             sla 
# 171                               2                              60 
# cust_id                         pincode product_analytic_super_category 
# 1201089                            7564                               1 
# product_analytic_category   product_analytic_sub_category       product_analytic_vertical 
# 5                              14                              74 
# product_mrp         product_procurement_sla 
# 1929                              17 
options(scipen = '999')
table(ecart_data$units)
table(ecart_data$deliverybdays)
table(ecart_data$deliverycdays)
table(ecart_data$s1_fact.order_payment_type)
table(ecart_data$sla)
table(ecart_data$product_procurement_sla)
f_freq_dist(ecart_data$gmv,seq(0,250000,by=20000))
f_freq_dist(ecart_data$product_mrp,seq(min(ecart_data$product_mrp),max(ecart_data$product_mrp)+1000,by=5000))

#3.order_id, order_item_id, cust_id and pincode ------
summary(ecart_data[,c("cust_id","order_id","order_item_id","pincode")])

#converting -ve to +ve numbers and conerting them into char
ecart_data$cust_id<-as.character(abs(ecart_data$cust_id))
ecart_data$pincode<-as.character(abs(ecart_data$pincode))
ecart_data$order_id<-as.character(ecart_data$order_id)
ecart_data$order_id<-as.character(ecart_data$order_item_id)

#4 deliverybdays,sla,product_procurement_sla --------

# deliverybdays -- delay in dispatching the product
# deliverycdays -- delay in delivering to customer
# sla -- estimated delivery days
# product_procurement_sla --  estimated time taken to procure
table(ecart_data$deliverybdays)
table(ecart_data$deliverycdays)
table(ecart_data$sla)
table(ecart_data$product_procurement_sla)

#most of the records has \\N as delay -- Assuming no delay 
#replacing '\\N' = 0
#Assumption : if delay is negative then there is no delay 
ecart_data$deliverybdays[which(ecart_data$deliverybdays=="\\N")]<-0
ecart_data$deliverycdays[which(ecart_data$deliverycdays=="\\N")]<-0
ecart_data$sla<-as.numeric(ecart_data$sla)
ecart_data$product_procurement_sla<-as.numeric(ecart_data$product_procurement_sla)
ecart_data$deliverybdays<-as.numeric(ecart_data$deliverybdays)
ecart_data$deliverycdays<-as.numeric(ecart_data$deliverycdays)
#5 gmv, mrp, units ------
sum(ecart_data$gmv <=0 ) #1349
sum(ecart_data$product_mrp <=0) #5290
sum(ecart_data$units <=0) #0
dim(ecart_data[which(ecart_data$gmv <=0 | ecart_data$product_mrp <=0),])
# # removing all 0 or-ve records of gmv and mrp
ecart_data<-ecart_data[-which(ecart_data$gmv <=0 | ecart_data$product_mrp <=0),] 

# Other categorical Variables----------
old.par <- par(mfcol=c(1,1),mar = c(0, 0, 0, 0))
par(mfcol=c(2,2),mar=c(2,1,2,1), oma=c(1,1,1,1))
barplot(table(ecart_data$s1_fact.order_payment_type),main="Payment type",xlab="Days",ylab="No of records")
barplot(table(ecart_data$product_analytic_category),main="Category",xlab="Days",ylab="No of records")
barplot(table(ecart_data$product_analytic_sub_category) ,main="Sub Category",xlab="Days",ylab="No of records")
barplot(table(ecart_data$product_analytic_vertical),main="Vertical Category",xlab="Days",ylab="No of records")
par<-old.par

#6 Filter data as required in project def -----------------------
#only need one year of data july 2015 to jun 2016
ecart_data$order_date<-as.Date(ecart_data$order_date)
ecart_data <- filter(ecart_data,order_date > '2015-6-30' & order_date < '2016-7-1')

#filter product sub category in c('CameraAccessory', 'HomeAudio', 'GamingAccessory')
table(as.factor(ecart_data$product_analytic_sub_category))
ecart_data <- ecart_data[which(ecart_data$product_analytic_sub_category %in% c('CameraAccessory', 
                                                                               'HomeAudio', 'GamingAccessory')),]
#summary -----------------
summary(ecart_data)
dim(ecart_data)
ecart_clean_data<-data.frame(ecart_data)

#--- Derived variables -----------------
theme_set(theme_bw() +theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1),legend.title = element_blank()))
#1.Delivery_days delay... a customer service KPI -----
# No of days to deliver =  time  taken to procure the product + 
#                         time taken to despating the product + 
#                         time taken to delivering the product
# delay = estimated delivery days (sla) - actual days to deliver
ecart_data$delay_days<-ecart_data$sla-ecart_data$product_procurement_sla + ecart_data$deliverybdays + ecart_data$deliverycdays 

#2.Gmv, unit, mrp  related  -----------------
summary(ecart_data[,c("gmv","product_mrp","units")])
# gmv          product_mrp         units       
# Min.   :    10   Min.   :    63   Min.   : 1.000  
# 1st Qu.:   299   1st Qu.:   700   1st Qu.: 1.000  
# Median :   556   Median :  1200   Median : 1.000  
# Mean   :  1289   Mean   :  2484   Mean   : 1.026  
# 3rd Qu.:  1499   3rd Qu.:  2800   3rd Qu.: 1.000  
# Max.   :148050   Max.   :180000   Max.   :39.000  

# clearly unit has outliers
quantile(ecart_data$units,probs = seq(0, 1, 0.01))
ecart_data[which(ecart_data$units>=2 & ecart_data$gmv > ecart_data$product_mrp),]$gmv<-
        ecart_data[which(ecart_data$units>=2 & ecart_data$gmv > ecart_data$product_mrp),]$gmv/ 
        ecart_data[which(ecart_data$units>=2 & ecart_data$gmv > ecart_data$product_mrp),]$units

ecart_data$units[which(ecart_data$units>=2)]<-1
#unit = 1 in whole data

#  gmv Vs mrp ----------
ecart_data[which(ecart_data$gmv>ecart_data$product_mrp),c("gmv","product_mrp")]%>%
        ggplot(aes(x = gmv, y = product_mrp)) +
        geom_point() +
        geom_smooth(method="lm", formula=y~x) +
        stat_poly_eq(parse=T, aes(label = ..eq.label..), formula=y~x)
#replace mrp to gmv where gmv > mrp
ecart_data[which(ecart_data$gmv>ecart_data$product_mrp),]$gmv<-ecart_data[which(ecart_data$gmv>ecart_data$product_mrp),]$product_mrp
# Grouped Scatter plot with marginal density plots

ggplot(ecart_data,aes(x =gmv, y = product_mrp)) + 
        geom_smooth(method=lm) + xlab("gmv") + ylab("mrp") +
        ggtitle("GMV vs MRP")+theme()
#This plot shows relationship between mrp and gmv which is linear
cor(ecart_data$gmv,ecart_data$product_mrp) #0.68

#3.List price ----
sum(ecart_data$listprice != ecart_data$gmv)
# gmv = list_price when units = 1
# 
#4 Order data kpi ----
ecart_data<-ecart_data%>%group_by(order_id)%>%dplyr::mutate(units_per_order=sum(units))%>%ungroup()
table(ecart_data$units_per_order)
f_freq_dist(ecart_data$units_per_order,seq(0,30,by=5))
ecart_data$No_of_items_in_cart<- case_when(
        between(ecart_data$units_per_order,1,5) ~ "0 - 5",
        between(ecart_data$units_per_order,6,10) ~ "5 - 10",
        between(ecart_data$units_per_order,11,15) ~ "10 - 15",
        between(ecart_data$units_per_order,16,20) ~ "15 - 20",
        between(ecart_data$units_per_order,21,25) ~ "20 - 25",
        between(ecart_data$units_per_order,26,30) ~ "25 - 30",
        TRUE                    ~  "other")
ecart_data%>%group_by(No_of_items_in_cart)%>%summarise(tot_gmv=sum(gmv))%>%
        ggplot(aes(x=No_of_items_in_cart,y=tot_gmv))+geom_bar(stat="identity")+
        xlab("No of units per cart")+ylab("Total Sale")+
        ggtitle("Sale Vs No of units per Order")
#7 calculating week- -----
startDate <- min(ecart_data$order_date)
ecart_data$week <-floor(as.numeric(difftime(ecart_data$order_date, startDate, units = "weeks"))+1)
table(ecart_data$week)

#setting month order from 1:12 instead of (7:12,1:6)
ecart_data$Month_ordered<- ifelse(ecart_data$Month < 7,
                                  ecart_data$Month + 6,
                                  ecart_data$Month - 6)
table(ecart_data$Month_ordered)


#-------------------------------------------------------------------------------
#   sale calender
#-------------------------------------------------------------------------------
Special_sale_calender <- read_excel("Media data and other information.xlsx", sheet = 3)
name<-as.factor(unlist(lapply(Special_sale_calender$`Sales Calendar`,function(x) unlist(strsplit(x,"\\(")[[1]][1]))))
name<-trim(tolower(name),side="both")
start_day<-as.Date(c("2015-07-18","2015-08-15","2015-08-28","2015-10-15","2015-11-07","2015-12-25","2016-01-20","2016-02-01","2016-02-14","2016-02-20","2016-03-07","2016-05-25"))
end_day<-as.Date(c("2015-07-19","2015-08-17","2015-08-30","2015-10-17","2015-11-14","2016-01-03","2016-01-22","2016-02-02","2016-02-15","2016-02-21","2016-03-09","2016-05-27"))
Special_sale<-data.frame(name,start_day,end_day)

Special_sale$start_week<-floor(as.numeric(difftime(Special_sale$start_day, startDate, units = "weeks"))+1)
Special_sale$end_week<-floor(as.numeric(difftime(Special_sale$end_day, startDate, units = "weeks"))+1)
Special_sale$start_day<-NULL
Special_sale$end_day<-NULL

special_sale_long <- data.frame(gather ( Special_sale , type , week , 2:3))
special_sale_long$type <- NULL
special_sale_long<-special_sale_long[-which(duplicated(special_sale_long,by=ids,fromLast=T)),]
levels(special_sale_long$name)

#-------------------------------------------------------------------------------
#   Media investment 
#-------------------------------------------------------------------------------
media_investment <- read_excel("Media data and other information.xlsx",
                               sheet = 2,skip = 2)
#replace NA with 0 = no investment
media_investment<-data.frame(media_investment)
media_investment[is.na(media_investment)]<-0
# media investment data -investment is in Crs converting it in to normal
media_investment[,3:12]<-media_investment[,3:12] * 10000000

#-------------------------------------------------------------------------------
#   nps
#-------------------------------------------------------------------------------
nps <-read_excel("Media data and other information.xlsx",
                 sheet = 4,skip = 1,
                 range = "B3:M3",col_names = FALSE)
## Audience reached × Frequency of advertisement = Gross rating points (GRP)
month <- c(7, 8, 9, 10, 11, 12, 1, 2, 3, 4, 5, 6)
year <- c(rep(2015:2016, each=6))
nps <- rbind(nps, month, year)
nps <- setNames(data.frame(t(nps)), c("nps", "Month", "Year"))
rownames(nps) <- NULL
nps$nps <- round(nps$nps, 2)
#-------------------------------------------------------------------------------
#   convert ecart_data to weekly data and then merge other data
#-------------------------------------------------------------------------------
ecart_weekly<-ecart_data%>%
        group_by(Year,Month,Month_ordered,week,
                 product_analytic_sub_category,
                 product_analytic_vertical,
                 s1_fact.order_payment_type)%>%
        dplyr::summarise(no_products=n_distinct(fsn_id),
                         no_orders=n_distinct(order_id),
                         no_customers=n_distinct(cust_id),
                         no_units=sum(units),
                         listprice=mean(gmv), # gvm = listprice when unit = 1
                         tot_gmv=sum(gmv),
                         delay_days=round(mean(delay_days),0),
                         mrp=round(mean(product_mrp),2),
                         units_per_order=round(mean(units_per_order),0))%>%ungroup()

colnames(ecart_weekly)[5:7]<-c("category","sub_category","payment_type")
#   Data merging -----------------------------------------------------------------
ecart<-merge(ecart_weekly,nps,by=c("Year","Month"),all.x=TRUE)
ecart<-merge(ecart,media_investment,by=c("Year","Month"),all.x=TRUE)
ecart<-merge(ecart,special_sale_long[,1:2],by="week",all.x=TRUE)
ecart<-as.data.frame(ecart)


ecart[,c("Year","Month")]<-NULL
ecart$Total.Investment<-NULL
colnames(ecart)[colnames(ecart)=="name"] <- "special_sale_name"

#52 week year 
#1 month = 4.3 approx 
str(ecart)
col_media<-c("TV","Digital","Sponsorship","Content.Marketing","Online.marketing",
             "Affiliates","SEM","Radio","Other")
ecart<-ecart%>%group_by(week)%>%mutate(rows_per_week=n() * 4.33)%>%ungroup()
ecart[,col_media]<-ecart[,col_media]/ecart$rows_per_week
ecart$rows_per_week<-NULL

ecart$is_special_sale_week<-ifelse(is.na(ecart$special_sale_name),FALSE,TRUE)
ecart$special_sale_name<-as.character(ecart$special_sale_name)
ecart$special_sale_name[which(is.na(ecart$special_sale_name))]<-"No_Sale"



#outlier treatment ---------
boxplot(ecart$tot_gmv) #
boxplot(ecart$mrp) #
boxplot(ecart$listprice) #
boxplot(ecart$TV)
boxplot(ecart$Digital) #
boxplot(ecart$Sponsorship) #
boxplot(ecart$Content.Marketing) #
boxplot(ecart$SEM) #
boxplot(ecart$Radio) #
boxplot(ecart$Other) #

ecart$tot_gmv<-f_remove_outliers(ecart$tot_gmv,.95)
ecart$mrp<-f_remove_outliers(ecart$mrp,.95)
ecart$listprice<-f_remove_outliers(ecart$listprice,.95)
ecart$TV<-f_remove_outliers(ecart$TV,.95)
ecart$Sponsorship<-f_remove_outliers(ecart$Sponsorship,.95)
ecart$Content.Marketing<-f_remove_outliers(ecart$Content.Marketing,.90)
ecart$SEM<-f_remove_outliers(ecart$SEM,.90)
ecart$Radio<-f_remove_outliers(ecart$Radio,.95)
ecart$Other<-f_remove_outliers(ecart$Other,.90)
# KPI generated ---------------
# 1.Delay days bucket ----------
table(ecart$delay_days)
f_freq_dist(ecart$delay_days,seq(min(ecart$delay_days),max(ecart$delay_days)+5,2))
ecart$delivery_kpi<- case_when(
        ecart$delay_days==0 ~ "Same_day_delivery",
        ecart$delay_days>0 & ecart$delay_days<7~"Delay_by_aweek",
        ecart$delay_days>=7 ~"Long_Delay",
        ecart$delay_days<0 ~ "Early",
        TRUE                    ~  "other")
# converting -ve delay days to 0
par(mfcol=c(1,1),mar=c(2,1,2,1), oma=c(1,1,1,1))
barplot(table(ecart$delay_days),xlab="Days",ylab="Frequency",main="Delivery Delay")
barplot(table(ecart$delivery_kpi),main="Delivery Delay",xlab="Days",ylab="No of records")

ecart%>%group_by(delivery_kpi)%>%summarise(tot_gmv=sum(tot_gmv)/10000000)%>%
        ggplot(aes(x=reorder(delivery_kpi,tot_gmv),y=tot_gmv))+
        geom_bar(stat="identity", aes(fill=tot_gmv))+
        xlab("Delivery Type")+ylab("Total Sale in Cr")+
        ggtitle("Sale Vs No of Delivery")+
        guides(fill="none")

# 2.Discount ----------------
ecart$discount<-round((ecart$mrp-ecart$listprice)/ecart$mrp *100,0)
ggplot(f_freq_dist(ecart$discount,seq(0,100,10)),aes(x=range,y=frequency))+
        geom_bar(stat="identity",aes(fill=frequency))+xlab("Discount% range") + 
        ylab("Frequency")+guides(fill="none")+
        ggtitle("Frquency of the discount given")
ecart$discount_kpi<- case_when(
        between(ecart$discount,0,20) ~ "0-20% Discount",
        between(ecart$discount,21,50) ~ "21%-50% Discount",
        ecart$discount >50 ~ ">50% Discount")

ecart%>%group_by(discount_kpi)%>%summarise(tot_gmv=sum(tot_gmv)/10000000)%>%
        ggplot(aes(x=reorder(discount_kpi,tot_gmv),y=tot_gmv))+
        geom_bar(stat="identity",aes(fill=tot_gmv)) +guides(fill="none")+
        labs(x="Discount",y="Sale in Cr.",title="Discount and  Sale")


# 3.Cart streath bucket -----------
table(ecart$units_per_order)
f_freq_dist(ecart$units_per_order,seq(0,16,by=2))
ecart$units_in_cart_kpi<- case_when(
        between(ecart$units_per_order,0,2) ~ "1-2",
        between(ecart$units_per_order,3,8) ~ "3-8",
        ecart$units_per_order >9 ~ ">9")

ecart%>%group_by(units_in_cart_kpi,tot_gmv=sum(tot_gmv)/10000000)%>%
        ggplot(aes(x=reorder(units_in_cart_kpi,tot_gmv),y=tot_gmv))+
        geom_bar(stat="identity", aes(fill=tot_gmv)) + guides(fill="none")+
        xlab("Units per Order")+ylab("Total Sale in Cr.")+
        ggtitle("Units per order and Average Sale")

# 4 Product type
sub_cat<-ecart%>%group_by(sub_category)%>%
        dplyr::summarise(avg_mrp = mean(mrp),units_sold = sum(no_units),tot_gmv=sum(tot_gmv)/10000000)%>%
        arrange(desc(avg_mrp),desc(units_sold))
ggplot(sub_cat,aes(x=avg_mrp,y=units_sold))+
        geom_point()+
        theme(axis.text.x = element_text(angle = 30, hjust = 1),legend.title = element_text())+
        labs(x="MRP",y="Units sold",title="MRP and Units sold")
# creating 3 different categories of sub category products
# total units sold >=20,000 "Mass Market"
# total units sold < 20,000  and mrp < 2000 = "aspiring"
# total units sold < 20,000 and mrp >= 2000  = "premium"
ecart<-ecart%>%group_by(sub_category)%>%
        dplyr::mutate(product_type= ifelse(sum(no_units)>=20000,"MassMarket",
                                           ifelse(sum(no_units)<20000 & mean(mrp)<2000,"Aspiring","Premium")))

ecart%>%group_by(category,product_type,total_gmv=sum(tot_gmv))%>%
        ggplot(aes(x=product_type,y=total_gmv))+geom_bar(stat="identity", aes(fill=total_gmv))+
        labs(x="Product type",y="Total Sale",title="Sale by Product Type")+guides(fill="none")+
        facet_grid(category~.)
# # Listprice infletion effect ----
f_list_price_inflation_week<-function(df){
        #fetching data
        weekly_list_price<-df%>%
                group_by(week)%>%
                dplyr::summarise(list_price=mean(listprice))
        weekly_list_price$weekly_listprice_infl<-NA
        for(i in 2:nrow(weekly_list_price)){
                weekly_list_price$weekly_listprice_infl[i]= (weekly_list_price$list_price[i]-weekly_list_price$list_price[i-1])/weekly_list_price$list_price[i-1] * 100
        }
        return(weekly_list_price[,-2])
}
ecart<-merge(ecart,f_list_price_inflation_week(ecart),by="week",all.x=TRUE)

ecart%>%group_by(week)%>%summarise(tot_gmv=sum(tot_gmv)/1000000,
                                   avg_infl = round(mean(weekly_listprice_infl,rm.na=TRUE),1))%>%
        ggplot(aes(x=week,y=avg_infl))+
        geom_line()+
        labs(x="Week",y="Weekly Listprice Inflation",title="Weekly Listprice Inflation")
# 
f_list_price_infaltion_month<-function(df){
        
        monthly_list_price<-df%>%
                group_by(Month_ordered)%>%
                dplyr::summarise(list_price=mean(listprice))
        
        monthly_list_price$monthly_listprice_infl<-NA
        for(i in 2:nrow(monthly_list_price)){
                monthly_list_price$monthly_listprice_infl[i]= (monthly_list_price$list_price[i]-monthly_list_price$list_price[i-1])/-monthly_list_price$list_price[i-1] *100
        }
        return(monthly_list_price[,-2])
}
ecart<-merge(ecart,f_list_price_infaltion_month(ecart),by="Month_ordered",all.x=TRUE)
ecart%>%group_by(Month_ordered)%>%summarise(tot_gmv=sum(tot_gmv)/1000000,
                                            avg_infl = round(mean(monthly_listprice_infl),1))%>%
        filter(is.na(avg_infl)==FALSE)%>%
        ggplot(aes(x=Month_ordered,y=avg_infl))+
        geom_line()+
        labs(x="Week",y="Monthly Listprice Inflation",title="Monthly Listprice Inflation")
#
# boxplot(ecart$weekly_listprice_infl)
# boxplot(ecart$monthly_listprice_infl)
# ecart$weekly_listprice_infl<-f_remove_outliers(ecart$weekly_listprice_infl,.95)

#-------------------------------------------------------------------------------
#   General EDA bivariate
# #-------------------------------------------------------------------------------
theme_set(theme_bw() +theme(legend.position = "top", axis.text.x = element_text(angle = 30, hjust = 1),legend.title = element_blank()))
#Weekly Sale
ecart%>%group_by(week)%>%summarise(tot_gmv=sum(tot_gmv)/100000)%>%
        ggplot(aes(x=as.factor(week), y= tot_gmv))+guides(fill="none")+
        geom_bar(stat="identity", aes(fill=tot_gmv))+
        xlab("Week")+ylab("Sale in lacs")+ ggtitle("Sale per week")

# Weekly Revenue by Category  and Product Type-----
ecart%>%group_by(week,category,product_type)%>%summarise(tot_gmv=sum(tot_gmv))%>%
        ggplot(aes(x=as.factor(week),y=tot_gmv,group=category, shape=category, colour=category))+
        geom_line(aes(linetype=category), size=1) +
        geom_point(size=2, fill="white") +
        scale_colour_hue(name="Category",l=30)  +
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        scale_linetype_discrete(name="Category") +
        xlab("Week") + ylab("Total Revenue") +
        ggtitle("Weekly Revenue by Category")+
        facet_grid(product_type~.)

# from plot we can see weekly pattern in revenue for most of the year except
# start and ending months.
total_gmv<-sum(ecart$tot_gmv)
ecart%>%group_by(category)%>% dplyr::summarise(tot_gmv_prc = round(sum(tot_gmv)/total_gmv * 100,1))%>%
        ggplot(aes(x=category, y=tot_gmv_prc))+
        geom_bar(stat="identity",aes(fill=tot_gmv_prc)) +
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        xlab("Category") + ylab("Revenue %") +guides(fill="none")+
        ggtitle(" Revenue % by Category")

ecart%>%group_by(category,product_type)%>% dplyr::summarise(avg_list_price = round(mean(listprice),1))%>%
        ggplot(aes(x=category, y=avg_list_price,fill=product_type))+
        geom_bar(stat="identity",position=position_dodge()) +
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        xlab("Category") + ylab("Average List Price") +
        ggtitle("Average List Price by Category")

#Camera accessory and home audio togather responsible for 75% of revenue
#Average list price for Camera Accessory is less then Home Audio
# we can say  list_price i s inversly propostional to revenue

# Weekly Promotion by Category --------
ecart%>%group_by(week,category)%>%summarise(discount=round(mean(discount),0))%>%
        ggplot(aes(x=as.factor(week),y=discount,group=category, shape=category, colour=category))+
        geom_line(aes(linetype=category), size=1) +
        geom_point(size=1, fill="white") +
        scale_colour_hue(name="Category",l=30)  +
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        scale_linetype_discrete(name="Category") +
        xlab("Week") + ylab("Discount %") +
        ggtitle("Weekly Discount by Category")
# # we see a big dip in promotion at 6th week for camera accessory products promos
# # from weekly revenue plot from 6,7,8 weeks revenue is almost 0..
ecart %>% group_by(category,discount_kpi) %>%
        summarise(avg_gmv = mean(tot_gmv))%>%
        ggplot(aes(x=discount_kpi,y=avg_gmv,group=category, shape=category, colour=category))+
        geom_line(aes(linetype=category), size=1) +
        geom_point(size=3, fill="white") +
        scale_colour_hue(name="Category",l=30)  +
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        scale_linetype_discrete(name="Category") +
        xlab("") + ylab("Average Sale") +
        ggtitle("Average sale by Discount ")

ecart %>% filter(special_sale_name!="No_Sale")%>%group_by (category,special_sale_name) %>%
        summarise( avg_gmv = sum(tot_gmv)) %>%
        ggplot(aes(x=special_sale_name,y=avg_gmv,group=category, shape=category, colour=category))+
        geom_line(aes(linetype=category), size=1) +
        geom_point(size=3, fill="white") +
        scale_colour_hue(name="Category",l=30)  +
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        scale_linetype_discrete(name="Category") +
        xlab("") + ylab("Average Sale") +
        ggtitle("Average sale by Special sale ")

ecart %>% filter(special_sale_name!="No_Sale")%>% group_by (category,special_sale_name) %>%
        summarise( avg_discount = mean(discount)) %>%
        ggplot(aes(x=special_sale_name,y=avg_discount,group=category, shape=category, colour=category))+
        geom_line(aes(linetype=category), size=1) +
        geom_point(size=3, fill="white") +
        scale_colour_hue(name="Category",l=30)  +
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        scale_linetype_discrete(name="Category") +
        xlab("") + ylab("Average discount") +
        ggtitle("Average discount by Special sale ")

# Payment Type -------
ecart%>%
        group_by(week,category,payment_type)%>%
        dplyr::summarise(tot_gmv_prc = round(sum(tot_gmv)/total_gmv * 100,1))%>%
        ggplot(aes(x=as.factor(week), y=tot_gmv_prc,fill=payment_type)) +
        geom_bar(stat="identity") +
        scale_shape_manual(name="Payment Type",values=c(22,21)) +
        xlab("Week") + ylab("Total Revenue %.") +
        ggtitle(" Revenue % by Payment Type") +
        facet_grid(category~.,scales = "free")

#plot shows same payment type pattern of revenue for all three category
#in week 41,42,43 you can see the payment type is only prepaid and revenue % is
#gradually decreasing... and sudden jump in revenue in 44 th week
#week 6,7,8 doen't have any activity..

# Revenue  per delivery type ---------
ecart%>%
        group_by(week,category,delivery_kpi)%>%
        dplyr::summarise(tot_gmv_prc = round(sum(tot_gmv)/total_gmv * 100,1))%>%
        ggplot(aes(x=as.factor(week), y=tot_gmv_prc,fill=delivery_kpi)) +
        geom_bar(stat="identity") +
        scale_shape_manual(name="Delivery Type",values=c(22,21)) +
        xlab("Week") + ylab("Total Revenue %.") +
        ggtitle(" Revenue % by Delivery Type") +
        facet_grid(category~.,scales = "free")

# from the plot we can see most revenue generation orders get delayes compared to on time and early
# this pattern is same for all three categories

# List price , MRP, no of units per order and revenue -----

columns<-c("tot_gmv","no_products","no_orders","no_units","listprice","mrp","discount",col_media)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(ecart[,columns]), type="lower")

#this plot takes too much time to load ....
# library("PerformanceAnalytics")
# chart.Correlation(ecart[,columns], histogram = T, pch= 19)


# Delay days -------------
ecart%>%
        group_by(week)%>%
        summarise(delay_day=round(mean(delay_days),0))%>%
        ggplot(aes(x=as.factor(week), y=delay_day)) +
        geom_bar(stat="identity",aes(fill=delay_day)) + guides(fill="none")+
        xlab("Week") + ylab("Average delivery delay in days") +
        ggtitle("Weekly average Delivery delay in days")
#high delay in delivery in 7 th week may be because of high promo offered and
#rush to purchase product was high
#from 42- 53rd week we can see steady increment in delay -- due to lack of other
#resources..
# Nps --------
ecart%>%
        group_by(Month_ordered)%>%
        summarise(nps=round(mean(nps),0))%>%
        ggplot(aes(x=as.factor(Month_ordered), y=nps)) +
        geom_bar(stat="identity") +
        xlab("Month") + ylab("nps") +
        ggtitle("Weekly NPS")

# Weekly Media investment ----------
#
media<-ecart[,c("week","category","tot_gmv",col_media)]%>%
        group_by(week,category)%>%
        dplyr::summarise( revenue=sum(tot_gmv),
                          TV=sum(TV),
                          Digital=sum(Digital),
                          Sponsorship=sum(Sponsorship),
                          Content.Marketing=sum(Content.Marketing),
                          Online.marketing=sum(Online.marketing),
                          Affiliates=sum(Affiliates),
                          SEM=sum(SEM),
                          Radio=sum(Radio),
                          Other=sum(Other))%>%ungroup()


media<-data.frame(media)

media.long<-tidyr::gather(media,Type, Investment,col_media)
str(media.long)

ggplot(media.long,aes(x=Investment, y=revenue)) +
        geom_line( size=1.2) +
        scale_x_continuous(expand = expand_scale(add = c(10, 10))) +
        xlab("Media Expenditure") + ylab("Revenue") +
        ggtitle("Media Expenditure and Revenue by Product Categories") +
        facet_grid(Type~category,scales="free")+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
table(media.long$Type)
#media investment vs return...
ggplot(media.long,aes(x=Investment, y=revenue)) +
        geom_line( size=1.2) +
        scale_x_continuous(expand = expand_scale(add = c(10, 10))) +
        xlab("Media Expenditure") + ylab("Revenue") +
        ggtitle("Media Expenditure and Revenue by Product Categories") +
        facet_wrap(Type~.,scales="free")+
        theme_bw() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
# Assumption: media investment per category is in same proportion.
media.long%>%group_by(week,category,Type)%>%summarise(Investment = sum(Investment))%>%
        ggplot(aes(x=week,y=Investment,group=category, shape=category, colour=category))+
        geom_line(aes(linetype=category), size=0.5) +
        geom_point(size=1, fill="white") +
        scale_colour_hue(name="category",l=30)  +
        scale_shape_manual(name="category",values=c(22,21,20)) +
        scale_linetype_discrete(name="category") +
        xlab("Week") + ylab("Investment") +
        ggtitle("Weekly Media Expenditure") +
        facet_grid(Type~.)


# # special day -------------------
ecart %>%  group_by (category,special_sale_name) %>%
        summarise( Avg_sale = mean(tot_gmv)) %>%
        ggplot( aes ( x=special_sale_name, y =Avg_sale ))+
        geom_bar(stat="identity",aes(fill=Avg_sale)) + guides(fill="none")+
        scale_shape_manual(name="Category",values=c(22,21,20)) +
        facet_grid(category ~.)+
        xlab("Category") + ylab("Average Revenue") +
        ggtitle("Average Revenew by Category in Special day sale")


write.csv(ecart,file="ecart.csv")

ecart.final1<-ecart
ecart<-ecart.final1
ecart_data<-NULL
raw_data<-NULL
ecart_clean_data<-NULL
#-------------------------------------------------------------------------------
# Data cleanup for Linear modeling 
#-------------------------------------------------------------------------------
colnames(ecart)
cor_col<-c("tot_gmv","no_products","no_orders","no_customers","no_units")
cor(ecart[,cor_col])
#               tot_gmv   no_products no_orders no_customers  no_units
# tot_gmv      1.0000000   0.7780672 0.7364540    0.7396811 0.7554230
# no_products  0.7780672   1.0000000 0.7459362    0.7472535 0.7387371
# no_orders    0.7364540   0.7459362 1.0000000    0.9997438 0.9749021
# no_customers 0.7396811   0.7472535 0.9997438    1.0000000 0.9768197
# no_units     0.7554230   0.7387371 0.9749021    0.9768197 1.0000000
table(ecart$units_in_cart_kpi)
table(ecart$units_per_order)
table(ecart$payment_type)

col_remove<-c("no_products","no_orders","no_customers","no_units","units_in_cart_kpi")
ecart[,col_remove]<-NULL


str(ecart)
col_categorical<-c("sub_category","special_sale_name","delivery_kpi","discount_kpi","product_type")
col_logical<-c("is_special_sale_week","payment_type")

# #categorical variables with 2 levels
ecart$is_special_sale_week<-as.numeric(ecart$is_special_sale_week)
ecart$payment_type<-as.numeric(ifelse(ecart$payment_type=="COD",1,0))

# #categorical variables with more than 2 levels
ecart[,col_categorical]<-lapply(ecart[,col_categorical], factor)

# #Creating dummy variables for all factors with more than 3 levels
dummies<-data.frame(sapply(ecart[,col_categorical],
                           function(x) data.frame(model.matrix(~x,data = ecart[,col_categorical]))[,-1]))
ecart<-cbind(ecart[,-which(colnames(ecart) %in% col_categorical)],dummies)

ecart<-ecart[complete.cases(ecart),]

str(ecart)

#rearrangement of variables
ecart_numeric<-ecart[,c(1:3,5:19,21:23)]
ecart_categorical<-ecart[,-c(1:3,5:19,21:23)]
ecart<-cbind(ecart_numeric,ecart_categorical)
str(ecart)

ecart.final2<-ecart

# Create Product sub category subsets -------------------------------------------
cam_acc<-dplyr::filter(ecart,category == 'CameraAccessory')
gam_acc<-dplyr::filter(ecart,category == 'GamingAccessory')
hm_Audio<-dplyr::filter(ecart,category == 'HomeAudio')

dim(cam_acc)
dim(gam_acc)
dim(hm_Audio)
#-------------------------------------------------------------------------------
# Creating data for lag distribution models-------------------------------------
ecart_dist<-ecart.final1
ecart_dist[,col_remove]<-NULL
#ecart_dist1<-ecart_dist[,c("week","category","sub_category","product_type","tot_gmv","discount","listprice",col_media)]
str(ecart_dist)
ecart_dist1<-ecart_dist[,c(3,4,21,23,25,26,5,22)]
ecart_dist2<-ecart_dist[,-c(3,4,21,23,25,26,5,22)]
ecart_dist<-cbind(ecart_dist1,ecart_dist2)
str(ecart_dist)
# Lag sale  
# #  i.e. the past value of tot_gmv (will be useful for Koyck and distributed lag models)
ecart_dist<-ecart_dist%>%group_by(week,category,sub_category)%>% arrange(week)%>%
        mutate(tot_gmv_lag1 = lag(tot_gmv,1),
               discount_lag1 = lag(discount,1),
               listprice_lag1 = lag(listprice,1))%>%
        mutate(tot_gmv_lag1_prc_change = ifelse(tot_gmv==0,0,(tot_gmv-tot_gmv_lag1)/tot_gmv * 100),
               discount_lag1_prc_change = ifelse(discount==0,0,(discount-discount_lag1)/discount * 100),
               listprice_lag1_prc_change = ifelse(listprice==0,0,(listprice-listprice_lag1)/listprice * 100))%>%ungroup()

ecart_dist<-ecart_dist%>%group_by(week,category,sub_category)%>% arrange(week)%>%
        mutate(TV_adstock = TV+ 
                       if_else(is.na(lag(TV, 1)*.6),0, lag(TV, 1)*.6)+
                       if_else(is.na(lag(TV, 2)*.36),0, lag(TV, 2)*.36)+
                       if_else(is.na(lag(TV, 3)*.22),0, lag(TV, 3)*.22)+
                       if_else(is.na(lag(TV, 4)*.13),0, lag(TV, 4)*.13)+
                       if_else(is.na(lag(TV, 5)*.8),0, lag(TV, 5)*.8),
               Digital_adstock= Digital + 
                       if_else(is.na(lag(Digital, 1)*.5),0, lag(Digital, 1)*.5) +
                       if_else(is.na(lag(Digital, 2)*.25),0, lag(Digital, 2)*.25)+
                       if_else(is.na(lag(Digital, 3)*.10),0, lag(Digital, 3)*.10),
               Sponsorship_adstock = Sponsorship +
                       if_else(is.na(lag(Sponsorship, 1)*.6),0, lag(Sponsorship, 1)*.6) +
                       if_else(is.na(lag(Sponsorship, 2)*.36),0, lag(Sponsorship, 2)*.36)+
                       if_else(is.na(lag(Sponsorship, 3)*.22),0, lag(Sponsorship, 3)*.22)+
                       if_else(is.na(lag(Sponsorship, 4)*.13),0, lag(Sponsorship, 4)*.13)+
                       if_else(is.na(lag(Sponsorship, 5)*.8),0, lag(Sponsorship, 5)*.8),
               Content.Marketing_adstock= Content.Marketing +
                       if_else(is.na(lag(Content.Marketing, 1)*.4),0, lag(Content.Marketing,1)*.4)+
                       if_else(is.na(lag(Content.Marketing, 2)*.2),0, lag(Content.Marketing,2)*.2)+
                       if_else(is.na(lag(Content.Marketing, 1)*.1),0, lag(Content.Marketing,1)*.1),
               Online.marketing_adstock = Online.marketing + 
                       if_else(is.na(lag(Online.marketing, 1)*.4),0, lag(Online.marketing, 1)*.4)+
                       if_else(is.na(lag(Online.marketing, 2)*.2),0, lag(Online.marketing, 2)*.2)+
                       if_else(is.na(lag(Online.marketing, 3)*.1),0, lag(Online.marketing, 3)*.1),
               Affiliates_adstock= Affiliates +
                       if_else(is.na(lag(Affiliates, 1)*.4),0, lag(Affiliates, 1)*.4) +
                       if_else(is.na(lag(Affiliates, 2)*.2),0, lag(Affiliates, 2)*.2)+
                       if_else(is.na(lag(Affiliates, 3)*.1),0, lag(Affiliates, 3)*.1),
               SEM_adstock = SEM + 
                       if_else(is.na(lag(SEM, 1)*.4),0, lag(SEM, 1)*.4)+
                       if_else(is.na(lag(SEM, 2)*.2),0, lag(SEM, 2)*.2)+
                       if_else(is.na(lag(SEM, 3)*.1),0, lag(SEM, 3)*.1),
               Radio_adstock = Radio + 
                       if_else(is.na(lag(Radio, 1)*.4),0, lag(Radio, 1)*.4)+
                       if_else(is.na(lag(Radio, 2)*.2),0, lag(Radio, 2)*.2)+
                       if_else(is.na(lag(Radio, 3)*.1),0, lag(Radio, 3)*.1),
               Other_adstock = Other + 
                       if_else(is.na(lag(Other, 1)*.4),0, lag(Other, 1)*.4)+
                       if_else(is.na(lag(Other, 2)*.2),0, lag(Other, 2)*.2)+
                       if_else(is.na(lag(Other, 3)*.1),0, lag(Other, 3)*.1)) %>% ungroup()
str(ecart_dist)

# #categorical variables with more than 2 levels
ecart_dist[,1:6]<-lapply(ecart_dist[,1:6], factor)
# #categorical variables with 2 levels
ecart_dist$is_special_sale_week<-as.numeric(ecart_dist$is_special_sale_week)
ecart_dist$payment_type<-as.numeric(ifelse(ecart_dist$payment_type=="COD",1,0))

# #Creating dummy variables for all factors with more than 3 levels
dummies<-data.frame(sapply(ecart_dist[,2:6],
                           function(x) data.frame(model.matrix(~x,data = ecart_dist[,2:6]))[,-1]))
ecart_dist<-cbind(ecart_dist[,c(1,7:ncol(ecart_dist))],dummies)

ecart_dist<-ecart_dist[complete.cases(ecart_dist),]
sum((sapply(ecart_dist,function(x) sum(is.nan(x)))>0))
sum((sapply(ecart_dist,function(x) sum(is.na(x)))>0))
sum((sapply(ecart_dist,function(x) sum(is.infinite(x)))>0))


# Create sub data set for each sub category
cam_acc.dist<-dplyr::filter(ecart_dist,category == 'CameraAccessory')
gam_acc.dist<-dplyr::filter(ecart_dist,category == 'GamingAccessory')
hm_Audio.dist<-dplyr::filter(ecart_dist,category == 'HomeAudio')

#  final_linear_model -------------
plot_elasticity<-function(model,Title){
        model_elsticity<-data.frame(model$coefficients)
        variables<-rownames(model_elsticity)
        model_elsticity<-cbind(variables,model_elsticity)
        colnames(model_elsticity)<-c("Variable","Elasticity")
        model_elsticity$Elasticity<-round(model_elsticity$Elasticity,1)
        
        model_elsticity<-model_elsticity%>%dplyr::arrange(desc(Elasticity))
        p<-ggplot(model_elsticity,aes(x=reorder(Variable,Elasticity),y=Elasticity))+
                geom_bar(stat = "identity", aes(fill=Elasticity))+
                geom_text(aes(label=Elasticity),color="orange",size=3,hjust=0)+
                coord_flip()+guides(fill="none")+
                xlab("")+ylab("Elsticity value")+ggtitle(title)
        return(p)
}
