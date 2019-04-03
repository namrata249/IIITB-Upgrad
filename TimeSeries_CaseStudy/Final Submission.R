# Group Members :
# Manish Jha
# Namrata Khatri
# Prashant Agrawal
# Rahul Shukla



library(dplyr)
library(zoo) 
library(forecast)
library(tseries)
require(graphics)
library(ggplot2)
#loading data
gsData <- read.csv('Global Superstore.csv',sep=',',header = T,stringsAsFactors = F)

#data structure and dimention
str(gsData)
dim(gsData) #[1] 51290    24

#check for duplicate records
sum(duplicated(gsData)) #0

#check unique values per column 
sapply(gsData,function(x){return(length(unique(x)))})

#check for null 
sapply(gsData,function(x)sum(is.na(x)),simplify = T)
#Postal.Code  41296 -- NULL values

#change Order.Date type 
gsData$Order.Date <-  zoo::as.yearmon(as.Date(gsData$Order.Date,format=("%d-%m-%Y")))

## Data Preparation------------------------------------------------------------
#first segment the whole dataset into the 21 subsets based on the market and 
#the customer segment level

# 1.combine  segment and market column into market.segment 
gsData <- within(gsData, Market.Segment <- paste(Market,Segment, sep="_"))
View(gsData)
gsData$Market.Segment<-as.factor(gsData$Market.Segment)

# 2.aggregate the 3 attributes  - Sales, Quantity & Profit, over the Order Date to 
#arrive at monthly values for these attributes

Agg.gsData <- gsData%>% 
  group_by(Market.Segment,Order.Date)%>%
  summarise(TotalSales = sum(Sales),
            TotalQuantity = sum(Quantity),
            TotalProfit = sum(Profit))%>%
  arrange(Order.Date)

#coefficient of variation of the Profit for all 21 market segments.
#The CV or RSD is widely used to express the precision and repeatability of 
#an assay. 
#Less CV means less standard deviation and more mean -- more repeatative values 
#are near mean -- meaning more consistent
##need to find out 2 most profitable (and consistent) segments 
Agg.gsData %>%
  group_by(Market.Segment) %>%
  summarise(NetProfit=sum(TotalProfit),
            AvgProfit=mean(TotalProfit),
            Coff.Var = sd(TotalProfit, na.rm=TRUE)/
              mean(TotalProfit, na.rm=TRUE))%>%
  arrange(desc(NetProfit,AvgProfit,Coff.Var))
# Top Two segments
# Market.Segment      NetProfit   AvgProfit    Coff.Var
# 1 APAC_Consumer      222818.     4642.       0.632
# 2 EU_Consumer        188688.     3931.       0.624  


finalData.APAC_Consumer<- filter(Agg.gsData,Market.Segment =='APAC_Consumer')%>%
  ungroup()%>%
  select(Order.Date,TotalSales,TotalQuantity)

finalData.EU_Consumer<- filter(Agg.gsData,Market.Segment=='EU_Consumer')%>%
  ungroup()%>%
  select(Order.Date,TotalSales,TotalQuantity)

APAC_Consumer.indt <-finalData.APAC_Consumer[1:42,]
APAC_Consumer.outdt <-finalData.APAC_Consumer[43:48,]

EU_Consumer.indt <-finalData.EU_Consumer[1:42,]
EU_Consumer.outdt <-finalData.EU_Consumer[43:48,]

#Model building ----------------------------------------------
#forecast the sales and quantity for the next 6 months.
#------------------------------------------------------
MinDate<-min(APAC_Consumer.indt$Order.Date)
#smoothening of series
Smoothning<-function(weight,timesr){
  w <-weight
  smoothedseries <- stats::filter(timesr, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
  #Smoothing left end of the time series
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  #Smoothing right end of the time series
  
  n <- length(timesr)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  return(smoothedseries)
}

####_______________________________________________________________________________________________________________________________####
#                                 APAC_Consumer sales time series
####_______________________________________________________________________________________________________________________________####

Segment1.sale.ts<-ts(APAC_Consumer.indt$TotalSales)

#Decompose the ts to understand the constituents
## the series has a increasing linear trend and a sinosodial seasonal curve
plot(decompose(ts(APAC_Consumer.indt[,2],frequency=12))) 

plot(Segment1.sale.ts,main="APAC-Consumer monthly sales",ylab="Sales",type='o')
S1.sale.smoothts<-Smoothning(1,Segment1.sale.ts)
lines(S1.sale.smoothts, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
time_val <-rank(APAC_Consumer.indt$Order.Date)
s1.saleddf <- as.data.frame(cbind(time_val, as.data.frame(S1.sale.smoothts)))
colnames(s1.saleddf) <- c('Month', 'Sales_APAC')

dim(s1.saleddf)
#-----classical decomposition
#let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

## With trial and error, poly 1 gave the best fit : lower MAPE value , and comparable log likelohoods than poly 2 & 3. The MAPE value improved from 31 to 26.896
lmfit <- lm(Sales_APAC ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=s1.saleddf)
global_pred <- predict(lmfit, Month=time_val)
summary(global_pred)


plot(Segment1.sale.ts,main="APAC-Consumer monthly sales",ylab="Sales",type='o')
lines(S1.sale.smoothts, col="blue", lwd=2)
lines(time_val, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- Segment1.sale.ts-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 95714751:  log likelihood=-445.51
#AIC=893.02   AICc=893.12   BIC=894.76

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
# Dickey-Fuller = -5.6979, Lag order = 3, p-value = 0.01
kpss.test(resi)
# KPSS Level = 0.025419, Truncation lag parameter = 3, p-value = 0.1
# adf and kpss tests - proves resi is stationary series

# #Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- rank(finalData.APAC_Consumer$Order.Date)[43:48]
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out
plot(timevals_out,fcast,type='o')
fcast
#fcast values
#       1        2        3        4        5        6 
#57348.97 62302.56 64491.84 63383.45 59306.50 53400.06 

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,APAC_Consumer.outdt$TotalSales)[5]
MAPE_class_dec #[1] 26.89676

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(finalData.APAC_Consumer$TotalSales), col = "black")
lines(class_dec_pred, col = "red")

# ARIMA fit--------------------
autoarima <- auto.arima(Segment1.sale.ts)
autoarima
# Series: Segment1.sale.ts 
# ARIMA(0,1,1) 
# Coefficients:      ma1    -0.7559   s.e.   0.1381
# sigma^2 estimated as 174361555:  log likelihood=-447.11
# AIC=898.23   AICc=898.55   BIC=901.66
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- Segment1.sale.ts - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
# Dickey-Fuller = -4.2563, Lag order = 3, p-value = 0.01
kpss.test(resi_auto_arima)
# KPSS Level = 0.043931, Truncation lag parameter = 3, p-value = 0.1
# adf and kpss tests - proves resi_auto_arima is stationary series

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer.outdt$TotalSales)[5]
MAPE_auto_arima #[1] 27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts(finalData.APAC_Consumer$TotalSales), col = "black")
lines(auto_arima_pred, col = "red")

##CONCLUSION:
# The classical decomposition model with poly 1 is a better fit than the Auto 
# Arima Model

next_6months_forecast <-predict(lmfit,data.frame(Month = c(1:54)))[49:54]
#      49       50       51       52       53       54 
# 47341.95 42923.43 41577.41 43983.86 49859.71 57995.43 

#Plot next 6 months prediction
x<-49:54
plot(type = "o", col = "red",x,next_6months_forecast)

####________________________________________________________________________####
#                 APAC_Consumer Demand time series
####________________________________________________________________________####


Segment1.demand.ts<-ts(APAC_Consumer.indt$TotalQuantity)

#Decompose the ts to understand the constituents
plot(decompose(ts(APAC_Consumer.indt$TotalQuantity,frequency=12))) 
## the series has a increasing linear trend and a sinosodial seasonal curve


plot(Segment1.demand.ts,main="APAC-Consumer monthly sales",ylab="Demand",type='o')
S1.demand.smoothts<-Smoothning(1,Segment1.demand.ts)
lines(S1.demand.smoothts, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
time_val <-rank(APAC_Consumer.indt$Order.Date)
s1.demanddf <- as.data.frame(cbind(time_val, as.data.frame(S1.demand.smoothts)))
colnames(s1.demanddf) <- c('Month', 'Demand_APAC')

dim(s1.demanddf)
#-----classical decomposition
#let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
## With trial and error, poly 1 gave the best fit : lower MAPE value , and comparable log likelohoods than poly 2 & 3. The MAPE value improved from 66 to 29.56586

lmfit <- lm(Demand_APAC ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=s1.demanddf)
global_pred <- predict(lmfit, Month=time_val)
summary(global_pred)

plot(Segment1.demand.ts,main="APAC-Consumer monthly demand",ylab="demand",type='o')
lines(S1.demand.smoothts, col="blue", lwd=2)
lines(time_val, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- Segment1.demand.ts-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 11192:  log likelihood=-255.38
#AIC=512.76   AICc=512.86   BIC=514.49

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
# Dickey-Fuller = -6.762, Lag order = 3, p-value = 0.01
kpss.test(resi)
# KPSS Level = 0.041584, Truncation lag parameter = 3, p-value = 0.1

# #Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- rank(finalData.APAC_Consumer$Order.Date)[43:48]
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out
fcast
#fcast predictions
#       1        2        3        4        5        6 
#697.9473 747.4322 757.7105 725.9758 660.6288 579.5369 

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,APAC_Consumer.outdt$TotalQuantity)[5]
MAPE_class_dec #[1] 29.56586

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(finalData.APAC_Consumer$TotalQuantity), col = "black")
lines(class_dec_pred, col = "red")

# ARIMA fit--------------------
autoarima <- auto.arima(Segment1.demand.ts)
autoarima
# Series: Segment1.demand.ts 
# ARIMA(0,1,0) 
# 
# sigma^2 estimated as 25366:  log likelihood=-266.07
# AIC=534.14   AICc=534.24   BIC=535.85
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- Segment1.demand.ts - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
# Dickey-Fuller = -4.3326, Lag order = 3, p-value = 0.01
kpss.test(resi_auto_arima)
# KPSS Level = 0.031535, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer.outdt$TotalQuantity)[5]
MAPE_auto_arima #[1] 26.24458

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts(finalData.APAC_Consumer$TotalQuantity), col = "black")
lines(auto_arima_pred, col = "red")
# fcast_auto_arima [1] 159.2675 225.2382 275.8593 318.5349 356.1329 390.1240

# Conclusion:
# Auto ARIMA is better than Classical Decomposition with poly 1 as it has better MAPE value
# Prediction for next 6 months.
next_6months_forecast <-predict(lmfit,data.frame(Month = c(1:54)))[49:54]
# 49       50       51       52       53       54 
# 505.6714 461.1905 461.4866 510.7856 600.5297 711.0826 

#Plot next 6 months prediction
x<-49:54
plot(type = "o", col = "red",x,next_6months_forecast)

####_______________________________________________________________________________________________________________________________####
#                                 EU Consumer sale time series
####_______________________________________________________________________________________________________________________________####

MinDate<-min(EU_Consumer.indt$Order.Date)

#Segment2.sale.ts<-ts(EU_Consumer.indt$TotalSales,start=c(MinDate,1),frequency = 12)
Segment2.sale.ts<-ts(EU_Consumer.indt$TotalSales)

#Decompose the ts to understand the constituents
plot(decompose(ts(EU_Consumer.indt$TotalSales,frequency=12))) ## the series has a increasing linear trend and a sinosodial seasonal curve


plot(Segment2.sale.ts,main="EU-Consumer monthly sales",ylab="Sale",type='o')
S2.sale.smoothts<-Smoothning(1,Segment2.sale.ts)
lines(S2.sale.smoothts, col="blue", lwd=2)
#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
time_val <-rank(EU_Consumer.indt$Order.Date)
s2.saleddf <- as.data.frame(cbind(time_val, as.data.frame(S2.sale.smoothts)))
colnames(s2.saleddf) <- c('Month', 'Sales_EU')
View(s2.saleddf)
dim(s2.saleddf)
#-----classical decomposition
#let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

## With trial and error, poly 1 gave the best fit : lower MAPE value , and comparable log likelohoods than poly 2 & 3. The MAPE value improved from 92 to 29


lmfit <- lm(Sales_EU ~ sin(0.5*Month) * poly(Month,1) + cos(0.5*Month) * poly(Month,1)
            + Month, data=s2.saleddf)
global_pred <- predict(lmfit, Month=time_val)
summary(global_pred)
accuracy(lmfit)

plot(Segment2.sale.ts,main="EU-Consumer monthly sales",ylab="Sales",type='o')
lines(S2.sale.smoothts, col="blue", lwd=2)
lines(time_val, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- Segment2.sale.ts-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
#Series: local_pred 
#ARIMA(0,0,0) with zero mean 

#sigma^2 estimated as 1.03e+08:  log likelihood=-447.05
#AIC=896.1   AICc=896.2   BIC=897.84
#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
# Dickey-Fuller = -4.2904, Lag order = 3, p-value = 0.01
kpss.test(resi)
# KPSS Level = 0.12078, Truncation lag parameter = 3, p-value = 0.1

# #Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- rank(finalData.EU_Consumer$Order.Date)[43:48]
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,EU_Consumer.outdt$TotalSales)[5]
MAPE_class_dec #[1]  29.09711

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(finalData.EU_Consumer$TotalSales), col = "black")
lines(class_dec_pred, col = "red")

# ARIMA fit--------------------
autoarima <- auto.arima(Segment2.sale.ts)
autoarima
# Series: Segment2.sale.ts 
# ARIMA(2,1,0) 
# 
# Coefficients:
#   ar1      ar2
# -0.5796  -0.4906
# s.e.   0.1346   0.1310
# 
# sigma^2 estimated as 168564623:  log likelihood=-445.84
# AIC=897.67   AICc=898.32   BIC=902.81
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- Segment2.sale.ts - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
# Dickey-Fuller = -4.3522, Lag order = 3, p-value = 0.01
kpss.test(resi_auto_arima)
# KPSS Level = 0.067962, Truncation lag parameter = 3, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer.outdt$TotalSales)[5]
MAPE_auto_arima #[1] 28.9226

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts(finalData.EU_Consumer$TotalSales), col = "black")
lines(auto_arima_pred, col = "red")


# The loglikelihood, AIC, & MAPE values of both classical decomposition and Auto ARIMA is almost comparable. So, finalizing the classical decomposition model.
# Prediction for next 6 months.
next_6months_forecast <-predict(lmfit,data.frame(Month = c(1:54)))[49:54]

#      49       50       51       52       53       54 
#37767.15 36501.97 37005.61 39310.69 42956.32 47103.55

#Plot next 6 months prediction
x<-49:54
plot(type = "o", col = "red",x,next_6months_forecast)


####_______________________________________________________________________####
#                       EU Consumer Demand time serries
####_______________________________________________________________________####

Segment2.demand.ts<-ts(EU_Consumer.indt$TotalQuantity)


#Decompose the ts to understand the constituents
plot(decompose(ts(EU_Consumer.indt$TotalQuantity,frequency=12))) 
## the series has a increasing linear trend and a sinosodial seasonal curve

plot(Segment2.demand.ts,main="EU-Consumer monthly sales",ylab="Demand",type='o')
S2.demand.smoothts<-Smoothning(1,Segment2.demand.ts)
lines(S2.demand.smoothts, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
time_val <-rank(EU_Consumer.indt$Order.Date)
s2.demanddf <- as.data.frame(cbind(time_val, as.data.frame(S2.demand.smoothts)))
colnames(s2.demanddf) <- c('Month', 'Demand_EU')

dim(s2.demanddf)
#-----classical decomposition
#let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#With poly 3, MAPE is 30 while with poly 1 MAPE is 29. But Dickey-Fuller 
#p-value = 0.25 for poly1. Hence prefering poly 3
lmfit <- lm(Demand_EU ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=s2.demanddf)
global_pred <- predict(lmfit, Month=time_val)
summary(global_pred)

plot(Segment2.demand.ts,main="EU-Consumer monthly demand",ylab="demand",type='o')
lines(S2.demand.smoothts, col="blue", lwd=2)
lines(time_val, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series
local_pred <- Segment2.demand.ts-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit
# Series: local_pred 
# ARIMA(2,0,0) with zero mean 
# 
# Coefficients:
#   ar1      ar2
# -0.6341  -0.6158
# s.e.   0.1173   0.1131
# 
# sigma^2 estimated as 7284:  log likelihood=-245.89
# AIC=497.79   AICc=498.42   BIC=503

#We'll check if the residual series is white noise
resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary")
# Dickey-Fuller = -6.6825, Lag order = 3, p-value = 0.01
kpss.test(resi)
# KPSS Level = 0.023531, Truncation lag parameter = 1, p-value = 0.1

# #Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- rank(finalData.EU_Consumer$Order.Date)[43:48]
global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
MAPE_class_dec <- accuracy(fcast,EU_Consumer.outdt$TotalQuantity)[5]
MAPE_class_dec #[1] 30.39741

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(finalData.EU_Consumer$TotalQuantity), col = "black")
lines(class_dec_pred, col = "red")

# ARIMA fit--------------------
autoarima <- auto.arima(Segment2.demand.ts)
autoarima
# Series: Segment2.demand.ts 
# ARIMA(2,1,0) 
# 
# Coefficients:
#   ar1      ar2
# -0.7359  -0.5879
# s.e.   0.1224   0.1185
# 
# sigma^2 estimated as 21185:  log likelihood=-261.9
# AIC=529.8   AICc=530.44   BIC=534.94
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- Segment2.demand.ts - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
# Dickey-Fuller = -3.5969, Lag order = 3, p-value = 0.04521
kpss.test(resi_auto_arima)
# KPSS Level = 0.047939, Truncation lag parameter = 1, p-value = 0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer.outdt$TotalQuantity)[5]
MAPE_auto_arima #[1] 30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts(finalData.EU_Consumer$TotalQuantity), col = "black")
lines(auto_arima_pred, col = "red")

# The loglikelihood, AIC, & MAPE values of both classical decomposition and Auto ARIMA is almost comparable but very slightly better for classical decomposition model.
next_6months_forecast <-predict(lmfit,data.frame(Month = c(1:54)))[49:54]

#      49       50       51       52       53       54 
#590.6191 540.0785 532.8207 587.1292 707.3636 880.7869 

#Plot next 6 months prediction
x<-49:54
plot(type = "o", col = "red",x,next_6months_forecast)


