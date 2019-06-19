
#game Accessory -- linear model ------------------
gam_acc.linear<-gam_acc
gam_acc.linear$category<-NULL
gam_acc.linear$week<-NULL
gam_acc.linear$Month_ordered<-NULL

# str(gam_acc.linear)
# col_numeric<-c("listprice","tot_gmv","delay_days","mrp","nps","discount","weekly_listprice_infl","monthly_listprice_infl",col_media)
# str(gam_acc.linear)

# Feature standardisation -- scaling
gam_acc.linear1<-scale(gam_acc.linear[,1:18])
gam_acc.linear2<-gam_acc.linear[,19:ncol(gam_acc.linear)]
gam_acc.linear<-cbind(gam_acc.linear1,gam_acc.linear2)

# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(gam_acc.linear), 0.7*nrow(gam_acc.linear))
train_l = gam_acc.linear[trainindices,]
test_l = gam_acc.linear[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_l)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step

model_2 <- lm(formula = tot_gmv ~ listprice + delay_days + mrp + units_per_order + 
                nps + Sponsorship + Content.Marketing + Online.marketing + 
                Affiliates + SEM + Radio + discount + weekly_listprice_infl + 
                payment_type + sub_category.xCoolingPad + sub_category.xGamePad + 
                sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                sub_category.xGamingGun + sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                sub_category.xGamingMouse + sub_category.xGamingSpeaker + 
                sub_category.xMotionController + special_sale_name.xdaussera.sale + 
                special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
                delivery_kpi.xLong_Delay + discount_kpi.x0.20..Discount + 
                discount_kpi.x21..50..Discount, data = train_l)
summary(model_2)
sort(vif(model_2))

# Remove insignificant variables units_per_order, sub_category.xMotionController, nps, 
# sub_category.xGamingAdapter, special_sale_name.xpacman, listprice, mrp, discount
# discount_kpi.x0.20..Discount, discount_kpi.x21..50..Discount, weekly_listprice_infl
# sub_category.xGamingGun, delivery_kpi.xLong_Delay, sub_category.xCoolingPad, 
# sub_category.xGamingChargingStation, special_sale_name.xdaussera.sale

model_3 <- lm(formula = tot_gmv ~ delay_days +
                Sponsorship + Content.Marketing + Online.marketing + 
                Affiliates + SEM + Radio + 
                payment_type + sub_category.xGamePad + 
                sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                sub_category.xGamingMouse + sub_category.xGamingSpeaker + 
                special_sale_name.xrakshabandhan.sale
                , data = train_l)
summary(model_3)
sort(vif(model_3))

# Residual standard error: 0.5927 on 883 degrees of freedom
# Multiple R-squared:  0.6343,	Adjusted R-squared:  0.6285 
# F-statistic: 109.4 on 14 and 883 DF,  p-value: < 0.00000000000000022

# Remove High Collinearity
# Online.marketing
#Content.Marketing
#Radio
#sub_category.xGamingSpeaker
model_4 <- lm(formula = tot_gmv ~ delay_days +
                Sponsorship  +  
                Affiliates + SEM  + 
                payment_type + sub_category.xGamePad + 
                sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                sub_category.xGamingMouse  + 
                special_sale_name.xrakshabandhan.sale
              , data = train_l)
summary(model_4)
sort(vif(model_4))

# Residual standard error: 0.6 on 884 degrees of freedom
# Multiple R-squared:  0.6248,	Adjusted R-squared:  0.6193 
# F-statistic: 113.2 on 13 and 884 DF,  p-value: < 0.00000000000000022
final_linear_model <- model_3
# Model Evalution -----------

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_l)
data.frame( R2 = R2(predictions, test_l$tot_gmv),
            RMSE = RMSE(predictions, test_l$tot_gmv),
            MAE = MAE(predictions, test_l$tot_gmv))
#         R2      RMSE      MAE
# 1 0.697923 0.6118748 0.4076187

final_linear_model <- model_4

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~ delay_days +
                       Sponsorship + Content.Marketing + Online.marketing + 
                       Affiliates + SEM + Radio + 
                       payment_type + sub_category.xGamePad + 
                       sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                       sub_category.xGamingMouse + sub_category.xGamingSpeaker + 
                       special_sale_name.xrakshabandhan.sale
               , data = train_l, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE     Rsquared   MAE     
# 0.59857  0.6243386  0.396331

#elasticity ---------------
#-------------------------------------------------------------------------------
# Data preparation for  for multiplicative modeling 
#-------------------------------------------------------------------------------

gam_acc.multi<-gam_acc
gam_acc.multi$category<-NULL
gam_acc.multi$week <- NULL
gam_acc.multi$Month_ordered<-NULL
str(gam_acc.multi)
#col<-c(2:15,17:19)
col<-c(1:18)
sapply(gam_acc.multi[,1:18],function(x) sum(x==0),simplify = TRUE)

# loge of 0 is -Inf -- convert 0 to some small num
# loge of -ve produces NaNs -- convert -ne in to +ve

gam_acc.multi$delay_days[which(gam_acc.multi$delay_days==0)]<-0.01
gam_acc.multi$Radio[which(gam_acc.multi$Radio==0)]<-0.01
gam_acc.multi$Other[which(gam_acc.multi$Other==0)]<-0.01
gam_acc.multi$discount[which(gam_acc.multi$discount==0)]<-0.01

gam_acc.multi1<-sign(gam_acc.multi[,col])*log(abs(gam_acc.multi[,col]),base=exp(1))
gam_acc.multi<-cbind(gam_acc.multi1,gam_acc.multi[,-col])

# Model-----
set.seed(123)
trainindices= sample(1:nrow(gam_acc.multi), 0.7*nrow(gam_acc.multi))
train_m = gam_acc.multi[trainindices,]
test_m = gam_acc.multi[-trainindices,]

model_1 <-lm(tot_gmv~.,data=train_m)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ listprice + delay_days + units_per_order + 
                    nps + TV + Sponsorship + Content.Marketing + SEM + Radio + 
                    Other + discount + weekly_listprice_infl + monthly_listprice_infl + 
                    payment_type + is_special_sale_week + sub_category.xCoolingPad + 
                    sub_category.xGamePad + sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                    sub_category.xGamingGun + sub_category.xGamingHeadset + sub_category.xGamingMemoryCard + 
                    sub_category.xGamingMouse + sub_category.xGamingMousePad + 
                    sub_category.xGamingSpeaker + sub_category.xJoystickGamingWheel + 
                    sub_category.xMotionController + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
                    special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xvalentine.s.day + delivery_kpi.xEarly + 
                    delivery_kpi.xLong_Delay + delivery_kpi.xSame_day_delivery + 
                    discount_kpi.x21..50..Discount, data = train_m)

summary(model_2)
sort(vif(model_2))

# Residual standard error: 0.7763 on 858 degrees of freedom
# Multiple R-squared:  0.8189,	Adjusted R-squared:  0.8106 
# F-statistic: 99.45 on 39 and 858 DF,  p-value: < 0.00000000000000022


#remove one by one high VIF

##Radio
#Sponsorship
#delay_days
#Content.Marketing
#monthly_listprice_infl
#is_special_sale_week
model_3<-lm(formula = tot_gmv ~ listprice  + units_per_order + nps + TV + 
                    SEM  + Other + discount + 
                    weekly_listprice_infl  + payment_type + 
                    sub_category.xCoolingPad + sub_category.xGamePad + 
                    sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                    sub_category.xGamingGun + sub_category.xGamingHeadset + sub_category.xGamingMemoryCard + 
                    sub_category.xGamingMouse + sub_category.xGamingMousePad + 
                    sub_category.xGamingSpeaker + sub_category.xJoystickGamingWheel + 
                    sub_category.xMotionController + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
                    special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xvalentine.s.day + delivery_kpi.xEarly + 
                    delivery_kpi.xLong_Delay + delivery_kpi.xSame_day_delivery + 
                    discount_kpi.x21..50..Discount, 
            data = train_m)
summary(model_3)
sort(vif(model_3))

# Residual standard error: 0.9568 on 866 degrees of freedom
# Multiple R-squared:  0.7867,	Adjusted R-squared:  0.7786 
# F-statistic: 72.65 on 31 and 866 DF,  p-value: < 0.00000000000000022

# Removing one by one for high pvalue
# SEM, Other, sub_category.xMotionController, special_sale_name.xbig.diwali.sale 
# special_sale_name.xbsd.5, delivery_kpi.xSame_day_delivery, special_sale_name.xchristmas...new.year.sale
# special_sale_name.xpacman, units_per_order, discount, sub_category.xGamingMemoryCard
# sub_category.xJoystickGamingWheel, sub_category.xGamingMousePad, special_sale_name.xdaussera.sale



#delivery_kpi.xSame_day_delivery
#Other
#special_sale_name.xbsd.5 
#pecial_sale_name.xchristmas...new.year.sale 
#special_sale_name.xbig.diwali.sale 
#special_sale_name.xpacman 
#sub_category.xGamingMousePad
#sub_category.xGamingGun
#special_sale_name.xdaussera.sale
#discount_kpi.x21..50..Discount 
#special_sale_name.xindependence.sale 
#sub_category.xGamingMemoryCard  
model_4<-lm(formula = tot_gmv ~ listprice  + units_per_order + nps + TV + 
                    SEM   + discount + 
                    weekly_listprice_infl  + payment_type + 
                    sub_category.xCoolingPad + sub_category.xGamePad + 
                    sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                      sub_category.xGamingHeadset  + 
                    sub_category.xGamingMouse  + 
                    sub_category.xGamingSpeaker + sub_category.xJoystickGamingWheel + 
                    sub_category.xMotionController + 
                     special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xvalentine.s.day + delivery_kpi.xEarly + 
                    delivery_kpi.xLong_Delay , 
            data = train_m)

summary(model_4)
sort(vif(model_4))

#Multiple R-squared:  0.772,	Adjusted R-squared:  0.7666 


# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_m)
data.frame( R2 = R2(predictions, test_m$tot_gmv),
            RMSE = RMSE(predictions, test_m$tot_gmv),
            MAE = MAE(predictions, test_m$tot_gmv))
# R2      RMSE       MAE
# 1 0.7034267 0.9222334 0.6930455
final.multi.model<-model_4
#k- fold cross validation

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
# Using model_4 as the final model
model <- train(tot_gmv ~ listprice  + units_per_order + nps + TV + 
                       SEM   + discount + 
                       weekly_listprice_infl  + payment_type + 
                       sub_category.xCoolingPad + sub_category.xGamePad + 
                       sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                       sub_category.xGamingHeadset  + 
                       sub_category.xGamingMouse  + 
                       sub_category.xGamingSpeaker + sub_category.xJoystickGamingWheel + 
                       sub_category.xMotionController + 
                       special_sale_name.xrakshabandhan.sale + 
                       special_sale_name.xvalentine.s.day + delivery_kpi.xEarly + 
                       delivery_kpi.xLong_Delay, data = gam_acc.multi, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# #  RMSE       Rsquared   MAE      
# 0.8873187  0.7483385  0.6700083

#-------------------------------------------------------------------------------
# Kyock
# -------------------------------------------------------------------------
gam_acc.koyck <- gam_acc
gam_acc.koyck$category<-NULL
gam_acc.koyck$week <- NULL
gam_acc.koyck$Month_ordered<-NULL

# Creating Lag variable 
gam_acc.koyck <- slide(gam_acc.koyck, Var = "tot_gmv",slideBy = -1)
str(gam_acc.koyck)
ncol(gam_acc.koyck)
col<-c(2:18,90)
gam_acc.koyck <- na.omit(gam_acc.koyck)
gam_acc.koyck1 <- data.frame(scale(gam_acc.koyck[,col]))
gam_acc.koyck2 <- data.frame(gam_acc.koyck[,-col])

gam_acc.koyck <- cbind(gam_acc.koyck1, gam_acc.koyck2)
str(gam_acc.koyck)

set.seed(123)
trainindices= sample(1:nrow(gam_acc.koyck), 0.7*nrow(gam_acc.koyck))
train_c = gam_acc.koyck[trainindices,]
test_c = gam_acc.koyck[-trainindices,]


## building first overall model
Koyck_model1 <- lm(tot_gmv~.,train_c)
summary(Koyck_model1)
#Multiple R-squared:  0.6857,	Adjusted R-squared:  0.6676 
step <- stepAIC(Koyck_model1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ delay_days + mrp + units_per_order + nps + 
              Content.Marketing + Online.marketing + Affiliates + SEM + 
              Radio + Other + discount + tot_gmv.1 + listprice + payment_type + 
              sub_category.xCoolingPad + sub_category.xGamePad + sub_category.xGamingAccessoryKit + 
              sub_category.xGamingChargingStation + sub_category.xGamingGun + 
              sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
              sub_category.xGamingMemoryCard + sub_category.xGamingMouse + 
              sub_category.xGamingMousePad + sub_category.xGamingSpeaker + 
              sub_category.xJoystickGamingWheel + special_sale_name.xbsd.5 + 
              special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
              special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
              special_sale_name.xrepublic.day + delivery_kpi.xLong_Delay + 
              discount_kpi.x0.20..Discount + discount_kpi.x21..50..Discount, 
            data = train_c)
summary(model_2)
sort(vif(model_2))
# Residual standard error: 0.5698 on 862 degrees of freedom
# Multiple R-squared:  0.6817,	Adjusted R-squared:  0.6687 
# F-statistic: 52.74 on 35 and 862 DF,  p-value: < 0.00000000000000022



#remove one by one  high VIF
#Online.marketing, listprice, Radio, delay_days, discount, Content.Marketing, discount_kpi.x21..50..Discount
#SEM

model_3<-lm(formula = tot_gmv ~  mrp + units_per_order + nps + 
              Affiliates +
              Other + tot_gmv.1 + payment_type + 
              sub_category.xCoolingPad + sub_category.xGamePad + sub_category.xGamingAccessoryKit + 
              sub_category.xGamingChargingStation + sub_category.xGamingGun + 
              sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
              sub_category.xGamingMemoryCard + sub_category.xGamingMouse + 
              sub_category.xGamingMousePad + sub_category.xGamingSpeaker + 
              sub_category.xJoystickGamingWheel + special_sale_name.xbsd.5 + 
              special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
              special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
              special_sale_name.xrepublic.day + delivery_kpi.xLong_Delay + 
              discount_kpi.x0.20..Discount , 
            data = train_c)
summary(model_3)
sort(vif(model_3))

# Residual standard error: 0.5991 on 870 degrees of freedom
# Multiple R-squared:  0.6448,	Adjusted R-squared:  0.6338 
# F-statistic:  58.5 on 27 and 870 DF,  p-value: < 0.00000000000000022

#remove one by one high p value
# mrp, nps, Other , sub_category.xGamingMemoryCard, sub_category.xGamingMousePad 
# sub_category.xJoystickGamingWheel, sub_category.xGamingAccessoryKit, special_sale_name.xpacman
# discount_kpi.x0.20..Discount, special_sale_name.xbsd.5,sub_category.xCoolingPad
# special_sale_name.xrepublic.day, sub_category.xGamingChargingStation, sub_category.xGamingGun 
# special_sale_name.xdaussera.sale, special_sale_name.xindependence.sale
# sub_category.xGamingSpeaker

model_4<-lm(formula = tot_gmv ~  units_per_order + 
              Affiliates +
              tot_gmv.1 + payment_type + 
              sub_category.xGamePad + 
              sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
              sub_category.xGamingMouse + 
              special_sale_name.xrakshabandhan.sale + 
              delivery_kpi.xLong_Delay 
              , 
            data = train_c)
summary(model_4)
sort(vif(model_4))

# Residual standard error: 0.6122 on 887 degrees of freedom
# Multiple R-squared:  0.6219,	Adjusted R-squared:  0.6177 
# F-statistic: 145.9 on 10 and 887 DF,  p-value: < 0.00000000000000022


# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_c)
data.frame( R2 = R2(predictions, test_c$tot_gmv),
            RMSE = RMSE(predictions, test_c$tot_gmv),
            MAE = MAE(predictions, test_c$tot_gmv))

#         R2      RMSE       MAE
#1 0.6885198 0.5803555 0.3923696
final.kyock.model<-model_4
#k- fold cross validation

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model - final model as model_4
model <- train(tot_gmv ~   units_per_order + 
                 Affiliates +
                 tot_gmv.1 + payment_type + 
                 sub_category.xGamePad + 
                 sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                 sub_category.xGamingMouse + 
                 special_sale_name.xrakshabandhan.sale + 
                 delivery_kpi.xLong_Delay , data = gam_acc.koyck, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
#RMSE       Rsquared   MAE      
#0.5967212  0.6476401  0.4068498

#--------------------------------------------------------------------------
#distributed lag model
#---------------------------------------------------------------------
#-data preparation for game accessory----------------------

str(gam_acc.dist)
gam_acc.dist$category<-NULL
gam_acc.dist$Month_ordered<-NULL
gam_acc.dist$week<-NULL

# for lag multiplicative model --
gam_acc.dist.multi<-gam_acc.dist
# 
gam_acc.dist[,3:35]<-scale(gam_acc.dist[,3:35])
# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(gam_acc.dist), 0.7*nrow(gam_acc.dist))
train_d = gam_acc.dist[trainindices,]
test_d = gam_acc.dist[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_d)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ payment_type + is_special_sale_week + 
                    listprice + delay_days + mrp + nps + TV + Sponsorship + Online.marketing + 
                    Affiliates + SEM + Radio + Other + discount + weekly_listprice_infl + 
                    monthly_listprice_infl + tot_gmv_lag1_prc_change + listprice_lag1_prc_change + 
                    TV_adstock + Online.marketing_adstock + Affiliates_adstock + 
                    SEM_adstock + Radio_adstock + Other_adstock + sub_category.xGamePad + 
                    sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                    sub_category.xGamingMouse + special_sale_name.xbsd.5 + special_sale_name.xdaussera.sale + 
                    special_sale_name.xrakshabandhan.sale + delivery_kpi.xEarly + 
                    discount_kpi.x0.20..Discount + discount_kpi.x21..50..Discount, 
            data = train_d)
summary(model_2)
sort(vif(model_2))

# Residual standard error: 0.5692 on 437 degrees of freedom
# Multiple R-squared:  0.7307,	Adjusted R-squared:  0.6975 
# F-statistic: 21.96 on 54 and 437 DF,  p-value: < 0.00000000000000022

# Removing one by one --
#Online.marketing
#Radio
#Affiliates_adstock
#Affiliates
#Other_adstock
#TV
#weekly_listprice_infl
#SEM
#Sponsorship
#discount
#Other
#listprice
#SEM_adstock
#discount_kpi.x21..50..Discount
model_3<-lm(formula = tot_gmv ~ payment_type + is_special_sale_week + 
                     delay_days + mrp + nps       + 
                    monthly_listprice_infl + tot_gmv_lag1_prc_change + listprice_lag1_prc_change + 
                    TV_adstock + Online.marketing_adstock  + 
                     Radio_adstock  + sub_category.xGamePad + 
                    sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                    sub_category.xGamingMouse + special_sale_name.xbsd.5 + special_sale_name.xdaussera.sale + 
                    special_sale_name.xrakshabandhan.sale + delivery_kpi.xEarly + 
                    discount_kpi.x0.20..Discount, 
            data = train_d)
summary(model_3)
sort(vif(model_3))

# Residual standard error: 0.6039 on 465 degrees of freedom
# Multiple R-squared:  0.6774,	Adjusted R-squared:  0.6594 
# F-statistic: 37.56 on 26 and 465 DF,  p-value: < 0.00000000000000022

# Remove high p-value variables
#discount_kpi.x0.20..Discount 
#Radio_adstock
#mrp
#delivery_kpi.xEarly
#special_sale_name.xbsd.5 
#listprice_lag1_prc_change
#TV_adstock
#is_special_sale_week
#special_sale_name.xrakshabandhan.sale
#monthly_listprice_infl
model_4<-lm(formula = tot_gmv ~ payment_type  + 
                    delay_days  + nps       + 
                      tot_gmv_lag1_prc_change  + 
                      Online.marketing_adstock  + 
                      sub_category.xGamePad + 
                    sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                    sub_category.xGamingMouse + special_sale_name.xdaussera.sale  , 
            data = train_d)
summary(model_4)
sort(vif(model_4))

# Residual standard error: 0.6196 on 481 degrees of freedom
#Multiple R-squared:  0.6446,	Adjusted R-squared:  0.6373 
# F-statistic: 88.88 on 10 and 481 DF,  p-value: < 0.00000000000000022

final.model.dist<-model_4
# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_d)
data.frame( R2 = R2(predictions, test_d$tot_gmv),
            RMSE = RMSE(predictions, test_d$tot_gmv),
            MAE = MAE(predictions, test_d$tot_gmv))

#         R2      RMSE       MAE
#1 0.5858182 0.5918418 0.3835193

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~ payment_type  + 
                       delay_days  + nps       + 
                       tot_gmv_lag1_prc_change  + 
                       Online.marketing_adstock  + 
                       sub_category.xGamePad + 
                       sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                       sub_category.xGamingMouse + special_sale_name.xdaussera.sale , data = gam_acc.dist, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE      Rsquared   MAE      
# 0.611059  0.6260288  0.3795828

#
#-------------------------------------------------------------------------------
# Distributed lag multiplicative model
# ------------------------------------------------------------------------------

# Data preparation -----------
str(gam_acc.dist.multi[3:35])
gam_acc.dist.multi2<-gam_acc.dist.multi[,c(1,2,50:ncol(gam_acc.dist.multi))]

gam_acc.dist.multi1  <- lapply (gam_acc.dist.multi[,3:35] ,function(x) ifelse ( x==0 , 0, log(abs(x)))*sign(x) )
gam_acc.dist.multi <- cbind(gam_acc.dist.multi1,gam_acc.dist.multi2)
gam_acc.dist.multi<-gam_acc.dist.multi[complete.cases(gam_acc.dist.multi),]

# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(gam_acc.dist.multi), 0.7*nrow(gam_acc.dist.multi))
train_dm = gam_acc.dist.multi[trainindices,]
test_dm = gam_acc.dist.multi[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_dm)
summary(model_1)

# Residual standard error: 0.396 on 418 degrees of freedom
# Multiple R-squared:  0.9447,	Adjusted R-squared:  0.9368 
# F-statistic: 106.7 on 73 and 418 DF,  p-value: < 0.00000000000000022

step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ listprice + delay_days + mrp + nps + Digital + 
                    Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                    SEM + Radio + monthly_listprice_infl + tot_gmv_lag1 + listprice_lag1 + 
                    tot_gmv_lag1_prc_change + TV_adstock + Digital_adstock + 
                    Online.marketing_adstock + Affiliates_adstock + SEM_adstock + 
                    Radio_adstock + Other_adstock + payment_type + sub_category.xCoolingPad + 
                    sub_category.xGamePad + sub_category.xGamingAccessoryKit + 
                    sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                    sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                    sub_category.xGamingMemoryCard + sub_category.xGamingMouse + 
                    sub_category.xGamingSpeaker + sub_category.xJoystickGamingWheel + 
                    sub_category.xMotionController + special_sale_name.xdaussera.sale + 
                    special_sale_name.xrakshabandhan.sale, data = train_dm)

summary(model_2)
sort(vif(model_2))

#Multiple R-squared:  0.9436,	Adjusted R-squared:  0.939 

#remove one by one by looking at high VIF 
#Online.marketing
#Radio_adstock
#Affiliates_adstock
#SEM
#Affiliates
#Sponsorship
#Digital_adstock
#listprice
#Content.Marketing
#SEM_adstock
#Radio
#tot_gmv_lag1
#sub_category.xMotionController
#nps
model_3<-lm(formula = tot_gmv ~   delay_days + mrp  + Digital + 
                       monthly_listprice_infl  + listprice_lag1 + 
                    tot_gmv_lag1_prc_change + TV_adstock  + 
                    Online.marketing_adstock   + 
                     Other_adstock + payment_type + sub_category.xCoolingPad + 
                    sub_category.xGamePad + sub_category.xGamingAccessoryKit + 
                    sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                    sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                    sub_category.xGamingMemoryCard + sub_category.xGamingMouse + 
                    sub_category.xGamingSpeaker + sub_category.xJoystickGamingWheel + 
                     special_sale_name.xdaussera.sale + 
                    special_sale_name.xrakshabandhan.sale, data = train_dm)
summary(model_3)
sort(vif(model_3))

# Remove high pvalues
#payment_type
#sub_category.xGamingMemoryCard
#monthly_listprice_infl
#listprice_lag1
#TV_adstock
#Other_adstock
#Digital
#sub_category.xJoystickGamingWheel
#special_sale_name.xdaussera.sale
model_4<-lm(formula = tot_gmv ~   delay_days + mrp   + 
                    tot_gmv_lag1_prc_change   + 
                    Online.marketing_adstock   + 
                     sub_category.xCoolingPad + 
                    sub_category.xGamePad + sub_category.xGamingAccessoryKit + 
                    sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                    sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                    sub_category.xGamingMouse + 
                    sub_category.xGamingSpeaker   + 
                    special_sale_name.xrakshabandhan.sale, data = train_dm)
summary(model_4)
sort(vif(model_4))

#Multiple R-squared:  0.8207,	Adjusted R-squared:  0.8155 
final.model.multi.dist <- model_4

# model Evalution

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_dm)
data.frame( R2 = R2(predictions, test_dm$tot_gmv),
            RMSE = RMSE(predictions, test_dm$tot_gmv),
            MAE = MAE(predictions, test_dm$tot_gmv))

# R2      RMSE       MAE
# 1 0.7520676 0.8234425 0.6406894
#k- fold cross validation

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~   delay_days + mrp   + 
                       tot_gmv_lag1_prc_change   + 
                       Online.marketing_adstock   + 
                       sub_category.xCoolingPad + 
                       sub_category.xGamePad + sub_category.xGamingAccessoryKit + 
                       sub_category.xGamingAdapter + sub_category.xGamingChargingStation + 
                       sub_category.xGamingHeadset + sub_category.xGamingKeyboard + 
                       sub_category.xGamingMouse + 
                       sub_category.xGamingSpeaker   + 
                       special_sale_name.xrakshabandhan.sale , data = gam_acc.dist.multi, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
#RMSE       Rsquared   MAE      
#0.7789021  0.7756296  0.5920253

#  Visulisation of models -------------------------


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
# ----------------------------------------------------------

