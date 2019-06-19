#home Audio -- linear model ------------------
hm_Audio.linear<-hm_Audio
hm_Audio.linear$category<-NULL
hm_Audio.linear$week<-NULL
hm_Audio.linear$Month_ordered<-NULL

# Feature standardisation -- scaling
hm_Audio.linear1<-scale(hm_Audio.linear[,1:18])
hm_Audio.linear2<-hm_Audio.linear[,19:ncol(hm_Audio.linear)]
hm_Audio.linear<-cbind(hm_Audio.linear1,hm_Audio.linear2)

# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(hm_Audio.linear), 0.7*nrow(hm_Audio.linear))
train_l = hm_Audio.linear[trainindices,]
test_l = hm_Audio.linear[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_l)
summary(model_1)
#Multiple R-squared:  0.8349,	Adjusted R-squared:  0.8212 

step <- stepAIC(model_1, direction="both")
step
model_2 <- lm(formula = tot_gmv ~ listprice + delay_days + mrp + units_per_order + 
                      nps + TV + Sponsorship + Content.Marketing + Online.marketing + 
                      Affiliates + SEM + Radio + payment_type + sub_category.xBoomBox + 
                      sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                      sub_category.xFMRadio + sub_category.xHomeAudioSpeaker + 
                      sub_category.xSlingBox + sub_category.xSoundMixer + special_sale_name.xbsd.5 + 
                      special_sale_name.xdaussera.sale + special_sale_name.xrakshabandhan.sale, 
              data = train_l)
summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.8309,	Adjusted R-squared:  0.8234 

#removed one by one  because of high VIF
#Online.marketing
#mrp
#SEM
#Sponsorship
#listprice
#Content.Marketing
model_3 <-lm(formula = tot_gmv ~   delay_days  + units_per_order + 
                     nps + TV   + 
                     Affiliates  + Radio + payment_type + sub_category.xBoomBox + 
                     sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                     sub_category.xFMRadio + sub_category.xHomeAudioSpeaker + 
                     sub_category.xSlingBox + sub_category.xSoundMixer + special_sale_name.xbsd.5 + 
                     special_sale_name.xdaussera.sale + special_sale_name.xrakshabandhan.sale, 
             data = train_l)

summary(model_3)
#Multiple R-squared:  0.816,	Adjusted R-squared:  0.8099 
sort(vif(model_3))
# remove one by one high pvalue
# TV
# sub_category.xBoomBox
# Radio
# special_sale_name.xbsd.5 
# sub_category.xSlingBox
# nps
# delay_days
# sub_category.xSoundMixer
# sub_category.xDJController
model_4 <-lm(formula = tot_gmv ~  units_per_order + 
                     Affiliates   + payment_type  + 
                     sub_category.xDock + sub_category.xDockingStation + 
                     sub_category.xFMRadio + sub_category.xHomeAudioSpeaker + 
                     special_sale_name.xdaussera.sale + special_sale_name.xrakshabandhan.sale, 
             data = train_l)

summary(model_4)
sort(vif(model_4))
#MMultiple R-squared:  0.8071,	Adjusted R-squared:  0.804 
final_linear_model <- model_4
# Model Evalution -----------

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_l)
data.frame( R2 = R2(predictions, test_l$tot_gmv),
            RMSE = RMSE(predictions, test_l$tot_gmv),
            MAE = MAE(predictions, test_l$tot_gmv))
# R2            RMSE       MAE
# 1 0.8204268   0.4647148   0.3170481

#considering model 3 a final model
final_linear_model <- model_4

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~  units_per_order + 
                       Affiliates   + payment_type  + 
                       sub_category.xDock + sub_category.xDockingStation + 
                       sub_category.xFMRadio + sub_category.xHomeAudioSpeaker + 
                       special_sale_name.xdaussera.sale + special_sale_name.xrakshabandhan.sale, data = hm_Audio.linear, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.4333315  0.8107299  0.2859763

#elasticity ---------------
#-------------------------------------------------------------------------------
# Data preparation for  for multiplicative modeling 
#-------------------------------------------------------------------------------

hm_Audio.multi<-hm_Audio
hm_Audio.multi$category<-NULL
hm_Audio.multi$week <- NULL
hm_Audio.multi$Month_ordered<-NULL
str(hm_Audio.multi)
col<-c(1:18)
sapply(hm_Audio.multi[,1:18],function(x) sum(x==0),simplify = TRUE)

# loge of 0 is -Inf -- convert 0 to some small num
# loge of -ve produces NaNs -- convert -ne in to +ve

hm_Audio.multi$delay_days[which(hm_Audio.multi$delay_days==0)]<-0.01
hm_Audio.multi$Radio[which(hm_Audio.multi$Radio==0)]<-0.01
hm_Audio.multi$Other[which(hm_Audio.multi$Other==0)]<-0.01
hm_Audio.multi$discount[which(hm_Audio.multi$discount==0)]<-0.01

hm_Audio.multi1<-sign(hm_Audio.multi[,col])*log(abs(hm_Audio.multi[,col]),base=exp(1))
hm_Audio.multi<-cbind(hm_Audio.multi1,hm_Audio.multi[,-col])

# Model-----
set.seed(123)
trainindices= sample(1:nrow(hm_Audio.multi), 0.7*nrow(hm_Audio.multi))
train_m = hm_Audio.multi[trainindices,]
test_m = hm_Audio.multi[-trainindices,]

model_1 <-lm(tot_gmv~.,data=train_m)
summary(model_1)
#Multiple R-squared:  0.8327,	Adjusted R-squared:  0.8189 
#
step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ listprice + delay_days + mrp + units_per_order + 
                    TV + Digital + Sponsorship + Online.marketing + Radio + Other + 
                    discount + weekly_listprice_infl + monthly_listprice_infl + 
                    payment_type + is_special_sale_week + sub_category.xBoomBox + 
                    sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                    sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                    sub_category.xKaraokePlayer + sub_category.xSlingBox + sub_category.xSoundMixer + 
                    special_sale_name.xbsd.5 + special_sale_name.xdaussera.sale + 
                    special_sale_name.xpacman + delivery_kpi.xLong_Delay + delivery_kpi.xSame_day_delivery + 
                    discount_kpi.x0.20..Discount + discount_kpi.x21..50..Discount, 
            data = train_m)

summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.8309,	Adjusted R-squared:  0.8207 

#remove one by one high VIF
#Radio
#listprice
#Sponsorship
#delay_days
#mrp
#discount_kpi.x0.20..Discount 
#TV
model_3<-lm(formula = tot_gmv ~   units_per_order + 
                    Digital  + Online.marketing  + Other + 
                    discount + weekly_listprice_infl + monthly_listprice_infl + 
                    payment_type + is_special_sale_week + sub_category.xBoomBox + 
                    sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                    sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                    sub_category.xKaraokePlayer + sub_category.xSlingBox + sub_category.xSoundMixer + 
                    special_sale_name.xbsd.5 + special_sale_name.xdaussera.sale + 
                    special_sale_name.xpacman + delivery_kpi.xLong_Delay + delivery_kpi.xSame_day_delivery + 
                    discount_kpi.x21..50..Discount, 
            data = train_m)

summary(model_3)
sort(vif(model_3))
#Multiple R-squared:  0.7992,	Adjusted R-squared:  0.7899 

#remove one by one high p value
#special_sale_name.xpacman
#delivery_kpi.xSame_day_delivery
#weekly_listprice_infl
#Digital
#sub_category.xBoomBox
#is_special_sale_week
#discount_kpi.x21..50..Discount 
#sub_category.xKaraokePlayer
#monthly_listprice_infl
#delivery_kpi.xLong_Delay
#special_sale_name.xdaussera.sale
#special_sale_name.xbsd.5
#Other
model_4<-lm(formula = tot_gmv ~   units_per_order + 
                     Online.marketing   + 
                    payment_type  + 
                    sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                    sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                    sub_category.xSlingBox + sub_category.xSoundMixer     , 
            data = train_m)

summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.7839,	Adjusted R-squared:  0.7792 

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_m)
data.frame( R2 = R2(predictions, test_m$tot_gmv),
            RMSE = RMSE(predictions, test_m$tot_gmv),
            MAE = MAE(predictions, test_m$tot_gmv))
#         R2     RMSE       MAE
#1 0.7335872 0.913672 0.6865441
#k- fold cross validation
final.multi.model<-model_4
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~   units_per_order + Online.marketing   + payment_type  + 
                       sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                       sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                       sub_category.xSlingBox + sub_category.xSoundMixer  , data = hm_Audio.multi, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.8755156  0.7589961  0.6516586

#-------------------------------------------------------------------------------
# Kyock
# -------------------------------------------------------------------------
hm_Audio.koyck <- hm_Audio
hm_Audio.koyck$category<-NULL
hm_Audio.koyck$week <- NULL
hm_Audio.koyck$Month_ordered<-NULL

# Creating Lag variable 
hm_Audio.koyck <- slide(hm_Audio.koyck, Var = "tot_gmv",slideBy = -1)
str(hm_Audio.koyck)
ncol(hm_Audio.koyck)
col<-c(2:18,90)
hm_Audio.koyck <- na.omit(hm_Audio.koyck)
hm_Audio.koyck1 <- data.frame(scale(hm_Audio.koyck[,col]))
hm_Audio.koyck2 <- data.frame(hm_Audio.koyck[,-col])

hm_Audio.koyck <- cbind(hm_Audio.koyck1, hm_Audio.koyck2)
str(hm_Audio.koyck)

set.seed(123)
trainindices= sample(1:nrow(hm_Audio.koyck), 0.7*nrow(hm_Audio.koyck))
train_c = hm_Audio.koyck[trainindices,]
test_c = hm_Audio.koyck[-trainindices,]


## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_c)
summary(Koyck_model1)
#Multiple R-squared:  0.8559,	Adjusted R-squared:  0.8437 

step <- stepAIC(Koyck_model1, direction="both")
step

model_2 <- lm(formula = tot_gmv ~ delay_days + mrp + units_per_order + nps + 
                      TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                      Affiliates + SEM + Radio + Other + weekly_listprice_infl + 
                      tot_gmv.1 + listprice + payment_type + is_special_sale_week + 
                      sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                      sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                      sub_category.xSlingBox + sub_category.xSoundMixer + special_sale_name.xbig.diwali.sale + 
                      special_sale_name.xbsd.5 + special_sale_name.xdaussera.sale + 
                      special_sale_name.xrakshabandhan.sale, data = train_c)
summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.8529,	Adjusted R-squared:  0.8446 

#remove one by one  high VIF
#Online.marketing
#special_sale_name.xrakshabandhan.sale
#SEM
#Other
#listprice
#Sponsorship
#mrp
#Content.Marketing
model_3 <- lm(formula = tot_gmv ~ delay_days  + units_per_order + nps + 
                      TV + Digital    + 
                      Affiliates  + Radio  + weekly_listprice_infl + 
                      tot_gmv.1  + payment_type + is_special_sale_week + 
                      sub_category.xDJController + sub_category.xDock + sub_category.xDockingStation + 
                      sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                      sub_category.xSlingBox + sub_category.xSoundMixer + special_sale_name.xbig.diwali.sale + 
                      special_sale_name.xbsd.5 + special_sale_name.xdaussera.sale , data = train_c)
summary(model_3)
sort(vif(model_3))
#Multiple R-squared:  0.8394,	Adjusted R-squared:  0.8328 

#removing one by one 
#TV
#Digital
#Radio
#special_sale_name.xbig.diwali.sale
#is_special_sale_week
#sub_category.xSlingBox
#special_sale_name.xbsd.5 
#tot_gmv.1 
#nps
#delay_days
#sub_category.xHiFiSystem
#sub_category.xSoundMixer
#special_sale_name.xdaussera.sale
#sub_category.xDJController
#weekly_listprice_infl
model_4 <- lm(formula = tot_gmv ~  units_per_order   + 
                      Affiliates    + weekly_listprice_infl + payment_type  + 
                      sub_category.xDock + sub_category.xDockingStation + 
                      sub_category.xFMRadio  + sub_category.xHomeAudioSpeaker , data = train_c)
summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.8242,	Adjusted R-squared:  0.8217 
# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_c)
data.frame( R2 = R2(predictions, test_c$tot_gmv),
            RMSE = RMSE(predictions, test_c$tot_gmv),
            MAE = MAE(predictions, test_c$tot_gmv))
# R2     RMSE       MAE
# 1 0.7926778 0.437138 0.2773625

#k- fold cross validation
final.kyock.model<-model_4
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~  units_per_order   + 
                       Affiliates    + weekly_listprice_infl + payment_type  + 
                       sub_category.xDock + sub_category.xDockingStation + 
                       sub_category.xFMRadio  + sub_category.xHomeAudioSpeaker, data = hm_Audio.koyck, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.4289312  0.8128724  0.2829492

#--------------------------------------------------------------------------
#distributed lag model
#---------------------------------------------------------------------
#-data preparation for Home Audio----------------------
str(hm_Audio.dist)
hm_Audio.dist$category<-NULL
hm_Audio.dist$Month_ordered<-NULL
hm_Audio.dist$week<-NULL

# for lag multiplicative model --
hm_Audio.dist.multi<-hm_Audio.dist
hm_Audio.dist[,3:35]<-scale(hm_Audio.dist[,3:35])
# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(hm_Audio.dist), 0.7*nrow(hm_Audio.dist))
train_d = hm_Audio.dist[trainindices,]
test_d = hm_Audio.dist[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_d)
summary(model_1)
#Multiple R-squared:  0.9056,	Adjusted R-squared:  0.8765 
step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = tot_gmv ~ payment_type + listprice + delay_days + 
                      mrp + nps + TV + Digital + Sponsorship + Content.Marketing + 
                      Online.marketing + Other + monthly_listprice_infl + tot_gmv_lag1 + 
                      discount_lag1 + tot_gmv_lag1_prc_change + listprice_lag1_prc_change + 
                      Sponsorship_adstock + Online.marketing_adstock + sub_category.xBoomBox + 
                      sub_category.xDJController + sub_category.xFMRadio + sub_category.xHomeAudioSpeaker + 
                      sub_category.xSoundMixer + special_sale_name.xchristmas...new.year.sale + 
                      special_sale_name.xrakshabandhan.sale + discount_kpi.x0.20..Discount + 
                      discount_kpi.x21..50..Discount, data = train_d)

summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.8912,	Adjusted R-squared:  0.8803 

#removing one by one -- high VIF values
#Online.marketing
#Sponsorship
#mrp
#Sponsorship_adstock
#tot_gmv_lag1
#Content.Marketing
# discount_kpi.x0.20..Discount 
# listprice
# nps
model_3 <- lm(formula = tot_gmv ~ payment_type  + delay_days + 
                         TV + Digital  + 
                       Other + monthly_listprice_infl  + 
                      discount_lag1 + tot_gmv_lag1_prc_change + listprice_lag1_prc_change + 
                       Online.marketing_adstock + sub_category.xBoomBox + 
                      sub_category.xDJController + sub_category.xFMRadio + sub_category.xHomeAudioSpeaker + 
                      sub_category.xSoundMixer + special_sale_name.xchristmas...new.year.sale + 
                      special_sale_name.xrakshabandhan.sale + 
                      discount_kpi.x21..50..Discount, data = train_d)

summary(model_3)
sort(vif(model_3))
#Multiple R-squared:  0.8621,	Adjusted R-squared:  0.8516 

#removing one by one  high p-value
#Digital
#sub_category.xDJController
#sub_category.xSoundMixer
#Other
#special_sale_name.xchristmas...new.year.sale
#listprice_lag1_prc_change
#listprice_lag1_prc_change
#discount_lag1
#discount_kpi.x21..50..Discount 
#special_sale_name.xrakshabandhan.sale
#sub_category.xBoomBox
#delay_days
#TV
#payment_type
model_4 <- lm(formula = tot_gmv ~    Online.marketing_adstock  + 
                       sub_category.xFMRadio + sub_category.xHomeAudioSpeaker , data = train_d)

summary(model_4)
sort(vif(model_4))

#Multiple R-squared:  0.8205,	Adjusted R-squared:  0.8186

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_d)
data.frame( R2 = R2(predictions, test_d$tot_gmv),
            RMSE = RMSE(predictions, test_d$tot_gmv),
            MAE = MAE(predictions, test_d$tot_gmv))
# R2      RMSE       MAE
# 1 0.7452013 0.4503729 0.2965419
#k- fold cross validation
final.model.dist<-model_4
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~      Online.marketing_adstock  + 
                       sub_category.xFMRadio + sub_category.xHomeAudioSpeaker , data = hm_Audio.dist, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.4401166  0.7920258  0.2949553

#-------------------------------------------------------------------------------
# Distributed lag multiplicative model
# ------------------------------------------------------------------------------

# Data preparation -----------
str(hm_Audio.dist.multi[3:35])
hm_Audio.dist.multi2<-hm_Audio.dist.multi[,c(1,2,50:ncol(hm_Audio.dist.multi))]

hm_Audio.dist.multi1  <- data.frame(lapply (hm_Audio.dist.multi[,3:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) ))
hm_Audio.dist.multi <- cbind(hm_Audio.dist.multi1,hm_Audio.dist.multi2)
hm_Audio.dist.multi<-hm_Audio.dist.multi[complete.cases(hm_Audio.dist.multi),]

# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(hm_Audio.dist.multi), 0.7*nrow(hm_Audio.dist.multi))
train_dm = hm_Audio.dist.multi[trainindices,]
test_dm = hm_Audio.dist.multi[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_dm)
summary(model_1)
#Multiple R-squared:  0.9589,	Adjusted R-squared:  0.9494 
step <- stepAIC(model_1, direction="both")
step


model_2 <- lm(formula = tot_gmv ~ listprice + delay_days + mrp + units_per_order + 
                      nps + TV + Digital + Sponsorship + Online.marketing + SEM + 
                      discount + tot_gmv_lag1 + discount_lag1 + tot_gmv_lag1_prc_change + 
                      listprice_lag1_prc_change + Digital_adstock + Content.Marketing_adstock + 
                      Online.marketing_adstock + Affiliates_adstock + SEM_adstock + 
                      payment_type + sub_category.xDJController + sub_category.xDockingStation + 
                      sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                      sub_category.xSoundMixer + discount_kpi.x0.20..Discount + 
                      discount_kpi.x21..50..Discount, data = train_dm)
summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.9573,	Adjusted R-squared:  0.9526 

#remove one by one by looking at high VIF
#Online.marketing_adstock
#SEM
#listprice
#Online.marketing
#Digital_adstock
#Affiliates_adstock
#discount_kpi.x0.20..Discount
#SEM_adstock
#tot_gmv_lag1
#Content.Marketing_adstock
#Sponsorship
#mrp
model_3 <- lm(formula = tot_gmv ~  delay_days  + units_per_order + 
                      nps + TV + Digital    + mrp +
                      discount  + discount_lag1 + tot_gmv_lag1_prc_change + 
                      listprice_lag1_prc_change    + 
                      payment_type + sub_category.xDJController + sub_category.xDockingStation + 
                      sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                      sub_category.xSoundMixer +
                      discount_kpi.x21..50..Discount, data = train_dm)

summary(model_3)
sort(vif(model_3))
#Multiple R-squared:  0.8487,	Adjusted R-squared:  0.8389 

#further remove looking at high p-value
#units_per_order
#payment_type
#Digital
#nps
#delay_days
#discount_kpi.x21..50..Discount
model_4 <- lm(formula = tot_gmv ~  TV   + discount  + 
                       listprice_lag1 + tot_gmv_lag1_prc_change + 
                       listprice_lag1_prc_change   +
                       sub_category.xDJController + sub_category.xDockingStation + 
                       sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                       sub_category.xSoundMixer,data = train_dm)

summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.8579,	Adjusted R-squared:  0.8525 

# model Evalution
final.model.multi.dist <- model_4
# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_dm)
data.frame( R2 = R2(predictions, test_dm$tot_gmv),
            RMSE = RMSE(predictions, test_dm$tot_gmv),
            MAE = MAE(predictions, test_dm$tot_gmv))
# R2      RMSE       MAE
# 1 0.8290398 0.7434998 0.5746039
#k- fold cross validation

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~ TV   + discount  + 
                       listprice_lag1 + tot_gmv_lag1_prc_change + 
                       listprice_lag1_prc_change   +
                       sub_category.xDJController + sub_category.xDockingStation + 
                       sub_category.xFMRadio + sub_category.xHiFiSystem + sub_category.xHomeAudioSpeaker + 
                       sub_category.xSoundMixer , data = hm_Audio.dist.multi, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.7083548  0.8422587  0.5348213

#------Visulisation --------
# 
# 
title<-"Camera Accessory Linear model elasticity"
plot_elasticity(final_linear_model,title)
title<-"Camera Accessory multiplicative elasticity"
plot_elasticity(final.multi.model,title)
title<-"Camera Accessory kyock elasticity"
plot_elasticity(final.kyock.model,title)
title<-"Camera Accessory distributed elasticity"
plot_elasticity(final.model.dist,title)
title<-"Camera Accessory multiplicative distributed elasticity"
plot_elasticity(final.model.multi.dist,title)
