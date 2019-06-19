#-------------------------------------------------------------------------------
#camera Accessory -- linear model 
#-------------------------------------------------------------------------------
cam_acc.linear<-cam_acc
cam_acc.linear$category<-NULL
cam_acc.linear$week<-NULL
cam_acc.linear$Month_ordered<-NULL

# str(cam_acc.linear)
# col_numeric<-c("listprice","tot_gmv","delay_days","mrp","nps","discount","weekly_listprice_infl","monthly_listprice_infl",col_media)
# str(cam_acc.linear)

# Feature standardisation -- scaling
cam_acc.linear1<-scale(cam_acc.linear[,1:18])
cam_acc.linear2<-cam_acc.linear[,19:ncol(cam_acc.linear)]
cam_acc.linear<-cbind(cam_acc.linear1,cam_acc.linear2)

# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(cam_acc.linear), 0.7*nrow(cam_acc.linear))
train_l = cam_acc.linear[trainindices,]
test_l = cam_acc.linear[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_l)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step
model_2<-lm(formula = tot_gmv ~ listprice + delay_days + units_per_order + 
           TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
           Affiliates + SEM + Radio + Other + discount + weekly_listprice_infl + 
           payment_type + sub_category.xCameraAccessory + sub_category.xCameraBag + 
           sub_category.xCameraBattery + sub_category.xCameraBatteryCharger + 
           sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
           sub_category.xCameraFilmRolls + sub_category.xCameraHousing + 
           sub_category.xCameraLEDLight + sub_category.xCameraMicrophone + 
           sub_category.xCameraMount + sub_category.xCameraRemoteControl + 
           sub_category.xCameraTripod + sub_category.xExtensionTube + 
           sub_category.xFilter + sub_category.xFlash + sub_category.xFlashShoeAdapter + 
           sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
           sub_category.xStrap + sub_category.xTeleconverter + sub_category.xTelescope + 
           special_sale_name.xbig.diwali.sale + special_sale_name.xdaussera.sale + 
           special_sale_name.xfhsd + special_sale_name.xindependence.sale + 
           special_sale_name.xrakshabandhan.sale + delivery_kpi.xLong_Delay + 
           discount_kpi.x0.20..Discount + discount_kpi.x21..50..Discount, 
   data = train_l)
summary(model_2)
sort(vif(model_2))

#removed one by one  because of high VIF
#Online.marketing
#special_sale_name.xrakshabandhan.sale
#SEM
#Radio
#discount_kpi.x0.20..Discount
#Sponsorship
#delay_days
#listprice
#Content.Marketing
#discount
model_3<-lm(formula = tot_gmv ~   units_per_order + 
                     TV + Digital   + 
                     Affiliates   + Other  + weekly_listprice_infl + 
                     payment_type + sub_category.xCameraAccessory + sub_category.xCameraBag + 
                     sub_category.xCameraBattery + sub_category.xCameraBatteryCharger + 
                     sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
                     sub_category.xCameraFilmRolls + sub_category.xCameraHousing + 
                     sub_category.xCameraLEDLight + sub_category.xCameraMicrophone + 
                     sub_category.xCameraMount + sub_category.xCameraRemoteControl + 
                     sub_category.xCameraTripod + sub_category.xExtensionTube + 
                     sub_category.xFilter + sub_category.xFlash + sub_category.xFlashShoeAdapter + 
                     sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
                     sub_category.xStrap + sub_category.xTeleconverter + sub_category.xTelescope + 
                     special_sale_name.xbig.diwali.sale + special_sale_name.xdaussera.sale + 
                     special_sale_name.xfhsd + special_sale_name.xindependence.sale + 
                      delivery_kpi.xLong_Delay  + discount_kpi.x21..50..Discount, 
             data = train_l)
summary(model_3)
sort(vif(model_3))

#remove one by one because of high p value
#Other
#special_sale_name.xbig.diwali.sale
#special_sale_name.xfhsd
#Digital
#TV
#discount_kpi.x21..50..Discount 
#sub_category.xTeleconverter
#sub_category.xCameraTripod
#sub_category.xCameraBag
#sub_category.xCameraBattery
#sub_category.xFlash
#special_sale_name.xindependence.sale
#sub_category.xFlashShoeAdapter
#sub_category.xCameraLEDLight
model_4<-lm(formula = tot_gmv ~   units_per_order + 
                    Affiliates     + weekly_listprice_infl + 
                    payment_type + sub_category.xCameraAccessory  + 
                     sub_category.xCameraBatteryCharger + 
                    sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
                    sub_category.xCameraFilmRolls + sub_category.xCameraHousing + 
                    sub_category.xCameraMicrophone + 
                    sub_category.xCameraMount + sub_category.xCameraRemoteControl + 
                     sub_category.xExtensionTube + 
                    sub_category.xFilter   + 
                    sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
                    sub_category.xStrap  + sub_category.xTelescope + 
                    special_sale_name.xdaussera.sale  + 
                    delivery_kpi.xLong_Delay  , 
            data = train_l)
summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7357 
# Model Evalution -----------

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_l)
data.frame( R2 = R2(predictions, test_l$tot_gmv),
            RMSE = RMSE(predictions, test_l$tot_gmv),
            MAE = MAE(predictions, test_l$tot_gmv))
# R2            RMSE            MAE
# 0.6595309  0.5352941     0.3457189

final_linear_model <- model_4
summary(final_linear_model)
# Define training control

train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~   units_per_order + 
                       Affiliates     + weekly_listprice_infl + 
                       payment_type + sub_category.xCameraAccessory  + 
                       sub_category.xCameraBatteryCharger + 
                       sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
                       sub_category.xCameraFilmRolls + sub_category.xCameraHousing + 
                       sub_category.xCameraMicrophone + 
                       sub_category.xCameraMount + sub_category.xCameraRemoteControl + 
                       sub_category.xExtensionTube + 
                       sub_category.xFilter   + 
                       sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
                       sub_category.xStrap  + sub_category.xTelescope + 
                       special_sale_name.xdaussera.sale  + 
                       delivery_kpi.xLong_Delay  ,
               data=cam_acc.linear, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.5298187  0.7180194  0.3496057


#-------------------------------------------------------------------------------
# Data preparation for  for multiplicative modeling 
#-------------------------------------------------------------------------------

cam_acc.multi<-cam_acc
cam_acc.multi$category<-NULL
cam_acc.multi$week <- NULL
cam_acc.multi$Month_ordered<-NULL
str(cam_acc.multi)
#col<-c(2:15,17:19)
col<-c(1:18)
sapply(cam_acc.multi[,1:18],function(x) sum(x==0),simplify = TRUE)

# loge of 0 is -Inf -- convert 0 to some small num
# loge of -ve produces NaNs -- convert -ne in to +ve

cam_acc.multi$delay_days[which(cam_acc.multi$delay_days==0)]<-0.01
cam_acc.multi$Radio[which(cam_acc.multi$Radio==0)]<-0.01
cam_acc.multi$Other[which(cam_acc.multi$Other==0)]<-0.01
cam_acc.multi$discount[which(cam_acc.multi$discount==0)]<-0.01

cam_acc.multi1<-sign(cam_acc.multi[,col])*log(abs(cam_acc.multi[,col]),base=exp(1))
cam_acc.multi<-cbind(cam_acc.multi1,cam_acc.multi[,-col])

# Model-----
set.seed(123)
trainindices= sample(1:nrow(cam_acc.multi), 0.7*nrow(cam_acc.multi))
train_m = cam_acc.multi[trainindices,]
test_m = cam_acc.multi[-trainindices,]

model_1 <-lm(tot_gmv~.,data=train_m)
summary(model_1)

step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ delay_days + mrp + units_per_order + nps + 
                    TV + Digital + Sponsorship + Content.Marketing + Online.marketing + 
                    Affiliates + Radio + Other + discount + weekly_listprice_infl + 
                    monthly_listprice_infl + payment_type + is_special_sale_week + 
                    sub_category.xCameraAccessory + sub_category.xCameraBag + 
                    sub_category.xCameraBattery + sub_category.xCameraBatteryGrip + 
                    sub_category.xCameraEyeCup + sub_category.xCameraFilmRolls + 
                    sub_category.xCameraHousing + sub_category.xCameraLEDLight + 
                    sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                    sub_category.xCameraRemoteControl + sub_category.xCameraTripod + 
                    sub_category.xExtensionTube + sub_category.xFilter + sub_category.xFlash + 
                    sub_category.xFlashShoeAdapter + sub_category.xLens + sub_category.xReflectorUmbrella + 
                    sub_category.xSoftbox + sub_category.xStrap + sub_category.xTeleconverter + 
                    sub_category.xTelescope + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
                    special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xrepublic.day + special_sale_name.xvalentine.s.day + 
                    delivery_kpi.xEarly + delivery_kpi.xLong_Delay + delivery_kpi.xSame_day_delivery + 
                    discount_kpi.x0.20..Discount + discount_kpi.x21..50..Discount, 
            data = train_m)

summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.8628,	Adjusted R-squared:  0.8563 
#
#remove one by one high VIF
#Radio
#Online.marketing
#Sponsorship
#Content.Marketing
#delay_days
#discount_kpi.x0.20..Discount 
#is_special_sale_week
#nps
#Affiliates

model_3<-lm(formula = tot_gmv ~   mrp + units_per_order  + 
                    TV + Digital +  Other + discount + weekly_listprice_infl + 
                    monthly_listprice_infl + payment_type  + 
                    sub_category.xCameraAccessory + sub_category.xCameraBag + 
                    sub_category.xCameraBattery + sub_category.xCameraBatteryGrip + 
                    sub_category.xCameraEyeCup + sub_category.xCameraFilmRolls + 
                    sub_category.xCameraHousing + sub_category.xCameraLEDLight + 
                    sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                    sub_category.xCameraRemoteControl + sub_category.xCameraTripod + 
                    sub_category.xExtensionTube + sub_category.xFilter + sub_category.xFlash + 
                    sub_category.xFlashShoeAdapter + sub_category.xLens + sub_category.xReflectorUmbrella + 
                    sub_category.xSoftbox + sub_category.xStrap + sub_category.xTeleconverter + 
                    sub_category.xTelescope + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
                    special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xrepublic.day + special_sale_name.xvalentine.s.day + 
                    delivery_kpi.xEarly + delivery_kpi.xLong_Delay + delivery_kpi.xSame_day_delivery + 
                     discount_kpi.x21..50..Discount, 
            data = train_m)

summary(model_3)
sort(vif(model_3))
#Multiple R-squared:  0.8396,	Adjusted R-squared:  0.8334 

#removing one by one because of high pvalue
#delivery_kpi.xEarly 
#special_sale_name.xchristmas...new.year.sale
#Digital
#monthly_listprice_infl
#delivery_kpi.xSame_day_delivery
#special_sale_name.xrepublic.day
#special_sale_name.xbsd.5  
#special_sale_name.xpacman
#sub_category.xFilter
#special_sale_name.xbig.diwali.sale
#Other
#discount
#sub_category.xCameraBag
#sub_category.xCameraTripod
#discount_kpi.x21..50..Discount 
#sub_category.xFlash
model_4<-lm(formula = tot_gmv ~   mrp + units_per_order  + 
                    TV   + weekly_listprice_infl + 
                    payment_type  + 
                    sub_category.xCameraAccessory  + 
                    sub_category.xCameraBattery + sub_category.xCameraBatteryGrip + 
                    sub_category.xCameraEyeCup + sub_category.xCameraFilmRolls + 
                    sub_category.xCameraHousing + sub_category.xCameraLEDLight + 
                    sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                    sub_category.xCameraRemoteControl  + 
                    sub_category.xExtensionTube  + 
                    sub_category.xFlashShoeAdapter  + sub_category.xReflectorUmbrella + 
                    sub_category.xSoftbox + sub_category.xStrap + sub_category.xTeleconverter + 
                    sub_category.xTelescope  +
                    special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
                     special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xvalentine.s.day + 
                    delivery_kpi.xLong_Delay  , 
            data = train_m)


summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.827,	Adjusted R-squared:  0.823
# Model evalution -----------------

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_m)
data.frame( R2 = R2(predictions, test_m$tot_gmv),
            RMSE = RMSE(predictions, test_m$tot_gmv),
            MAE = MAE(predictions, test_m$tot_gmv))
# R2            RMSE            MAE
# 1 0.7707387   0.9027428       0.6689656
#k- fold cross validation

final.multi.model<-model_4
summary(final.multi.model)
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~   mrp + units_per_order  + 
                       TV   + weekly_listprice_infl + 
                       payment_type  + 
                       sub_category.xCameraAccessory  + 
                       sub_category.xCameraBattery + sub_category.xCameraBatteryGrip + 
                       sub_category.xCameraEyeCup + sub_category.xCameraFilmRolls + 
                       sub_category.xCameraHousing + sub_category.xCameraLEDLight + 
                       sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                       sub_category.xCameraRemoteControl  + 
                       sub_category.xExtensionTube  + 
                       sub_category.xFlashShoeAdapter  + sub_category.xReflectorUmbrella + 
                       sub_category.xSoftbox + sub_category.xStrap + sub_category.xTeleconverter + 
                       sub_category.xTelescope  +
                       special_sale_name.xdaussera.sale + special_sale_name.xindependence.sale + 
                       special_sale_name.xrakshabandhan.sale + 
                       special_sale_name.xvalentine.s.day + 
                       delivery_kpi.xLong_Delay , data = cam_acc.multi, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE     
# 0.8543597  0.8012706  0.611671
#-------------------------------------------------------------------------------
# Kyock
# ------------------------------------------------------------------------------
# -------------------------------------------------------------------------
cam_acc.koyck <- cam_acc
cam_acc.koyck$category<-NULL
cam_acc.koyck$week <- NULL
cam_acc.koyck$Month_ordered<-NULL

# Creating Lag variable 
cam_acc.koyck <- slide(cam_acc.koyck, Var = "tot_gmv",slideBy = -1)
str(cam_acc.koyck)
ncol(cam_acc.koyck)
col<-c(2:18,90)
cam_acc.koyck <- na.omit(cam_acc.koyck)
cam_acc.koyck1 <- data.frame(scale(cam_acc.koyck[,col]))
cam_acc.koyck2 <- data.frame(cam_acc.koyck[,-col])

cam_acc.koyck <- cbind(cam_acc.koyck1, cam_acc.koyck2)
str(cam_acc.koyck)

#Modeling --------------------------
set.seed(123)
trainindices= sample(1:nrow(cam_acc.koyck), 0.7*nrow(cam_acc.koyck))
train_c = cam_acc.koyck[trainindices,]
test_c = cam_acc.koyck[-trainindices,]

Koyck_model1 <- lm(tot_gmv~.,train_c)
#Multiple R-squared:  0.787,	Adjusted R-squared:  0.7764
summary(Koyck_model1)

step <- stepAIC(Koyck_model1, direction="both")
step


model_2<-lm(formula = tot_gmv ~ delay_days + units_per_order + TV + Digital + 
                    Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                    SEM + Radio + Other + discount + weekly_listprice_infl + 
                    tot_gmv.1 + listprice + payment_type + is_special_sale_week + 
                    sub_category.xCameraAccessory + sub_category.xCameraBag + 
                    sub_category.xCameraBattery + sub_category.xCameraBatteryCharger + 
                    sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
                    sub_category.xCameraFilmRolls + sub_category.xCameraHousing + 
                    sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                    sub_category.xCameraRemoteControl + sub_category.xCameraTripod + 
                    sub_category.xExtensionTube + sub_category.xFilter + sub_category.xFlash + 
                    sub_category.xFlashShoeAdapter + sub_category.xLens + sub_category.xReflectorUmbrella + 
                    sub_category.xSoftbox + sub_category.xStrap + sub_category.xTeleconverter + 
                    sub_category.xTelescope + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xfhsd + 
                    special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xrepublic.day + delivery_kpi.xEarly + delivery_kpi.xLong_Delay + 
                    discount_kpi.x0.20..Discount, data = train_c)
summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.7862,	Adjusted R-squared:  0.7767 


#remove one by one  high VIF
#Online.marketing
#special_sale_name.xrakshabandhan.sale
#Radio
#SEM
#Sponsorship
#delay_days
#is_special_sale_week
#discount
#sub_category.xCameraFilmRolls
#Content.Marketing
#listprice
model_3<-lm(formula = tot_gmv ~   units_per_order + TV + Digital + 
                       Affiliates + 
                     Other  + weekly_listprice_infl + 
                    tot_gmv.1  + payment_type  + 
                    sub_category.xCameraAccessory + sub_category.xCameraBag + 
                    sub_category.xCameraBattery + sub_category.xCameraBatteryCharger + 
                    sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
                    sub_category.xCameraHousing + 
                    sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                    sub_category.xCameraRemoteControl + sub_category.xCameraTripod + 
                    sub_category.xExtensionTube + sub_category.xFilter + sub_category.xFlash + 
                    sub_category.xFlashShoeAdapter + sub_category.xLens + sub_category.xReflectorUmbrella + 
                    sub_category.xSoftbox + sub_category.xStrap + sub_category.xTeleconverter + 
                    sub_category.xTelescope + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xfhsd + 
                    special_sale_name.xpacman  + 
                    special_sale_name.xrepublic.day + delivery_kpi.xEarly + delivery_kpi.xLong_Delay + 
                    discount_kpi.x0.20..Discount, data = train_c)
summary(model_3)
sort(vif(model_3))
#remove one by one high p value
#Digital
#special_sale_name.xbig.diwali.sale
#special_sale_name.xpacman
#special_sale_name.xfhsd
#tot_gmv.1  
#delivery_kpi.xEarly
#special_sale_name.xchristmas...new.year.sale
#TV
#sub_category.xFlash
#sub_category.xCameraTripod
#sub_category.xCameraBattery
#sub_category.xCameraBatteryCharger
#Other
#sub_category.xTeleconverter
#special_sale_name.xrepublic.day
#special_sale_name.xbsd.5 
#sub_category.xFlashShoeAdapter
#sub_category.xCameraBag
#sub_category.xReflectorUmbrella
model_4<-lm(formula = tot_gmv ~   units_per_order   + 
                    Affiliates + 
                      weekly_listprice_infl + 
                     payment_type  + 
                    sub_category.xCameraAccessory  + 
                    sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
                    sub_category.xCameraHousing + 
                    sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                    sub_category.xCameraRemoteControl  + 
                    sub_category.xExtensionTube + sub_category.xFilter  + 
                    sub_category.xLens  + 
                    sub_category.xSoftbox + sub_category.xStrap  + 
                    sub_category.xTelescope  + 
                    special_sale_name.xdaussera.sale  + 
                    delivery_kpi.xLong_Delay + 
                    discount_kpi.x0.20..Discount, data = train_c)
summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.7104,	Adjusted R-squared:  0.7053 

#model evalution ---------------------------

# Make predictions and compute the R2, RMSE and MAE
predictions <- model%>% predict(test_c)
data.frame( R2 = R2(predictions, test_c$tot_gmv),
            RMSE = RMSE(predictions, test_c$tot_gmv),
            MAE = MAE(predictions, test_c$tot_gmv))

# R2     RMSE      MAE
# 1 0.4901605 3.147638 2.881681

final.kyock.model<-model_4
summary(final.kyock.model)
#k- fold cross validation

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~   units_per_order   + 
                       Affiliates + 
                       weekly_listprice_infl + 
                       payment_type  + 
                       sub_category.xCameraAccessory  + 
                       sub_category.xCameraBatteryGrip + sub_category.xCameraEyeCup + 
                       sub_category.xCameraHousing + 
                       sub_category.xCameraMicrophone + sub_category.xCameraMount + 
                       sub_category.xCameraRemoteControl  + 
                       sub_category.xExtensionTube + sub_category.xFilter  + 
                       sub_category.xLens  + 
                       sub_category.xSoftbox + sub_category.xStrap  + 
                       sub_category.xTelescope  + 
                       special_sale_name.xdaussera.sale  + 
                       delivery_kpi.xLong_Delay + 
                       discount_kpi.x0.20..Discount , data = cam_acc.koyck, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.5717762  0.6721222  0.3881277
#-------------------------------------------------------------------------------
#distributed lag model
#--------------------------------------------------------------------------------

#-data preparation for camera accessory----------------------

str(cam_acc.dist)
cam_acc.dist$category<-NULL
cam_acc.dist$Month_ordered<-NULL
cam_acc.dist$week<-NULL
str(cam_acc.dist[3:35])
# for lag multiplicative model --
cam_acc.dist.multi<-cam_acc.dist
# 
cam_acc.dist[,3:35]<-scale(cam_acc.dist[,3:35])
# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(cam_acc.dist), 0.7*nrow(cam_acc.dist))
train_d = cam_acc.dist[trainindices,]
test_d = cam_acc.dist[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_d)
summary(model_1)
# Multiple R-squared:  0.8293,	Adjusted R-squared:  0.8088

step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ payment_type + is_special_sale_week + 
                    listprice + nps + TV + Online.marketing + Affiliates + SEM + 
                    Radio + Other + discount + weekly_listprice_infl + monthly_listprice_infl + 
                    tot_gmv_lag1 + discount_lag1 + listprice_lag1 + tot_gmv_lag1_prc_change + 
                    listprice_lag1_prc_change + TV_adstock + Sponsorship_adstock + 
                    Content.Marketing_adstock + Online.marketing_adstock + Affiliates_adstock + 
                    SEM_adstock + Radio_adstock + Other_adstock + sub_category.xCameraAccessory + 
                    sub_category.xCameraBatteryCharger + sub_category.xCameraBatteryGrip + 
                    sub_category.xCameraEyeCup + sub_category.xCameraFilmRolls + 
                    sub_category.xCameraHousing + sub_category.xCameraMicrophone + 
                    sub_category.xCameraMount + sub_category.xCameraRemoteControl + 
                    sub_category.xExtensionTube + sub_category.xFilter + sub_category.xFlash + 
                    sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
                    sub_category.xStrap + sub_category.xTelescope + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xfhsd + special_sale_name.xpacman + special_sale_name.xrakshabandhan.sale + 
                    special_sale_name.xrepublic.day + special_sale_name.xvalentine.s.day + 
                    discount_kpi.x0.20..Discount, data = train_d)
summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.8266,	Adjusted R-squared:  0.8111 
#
#removing one by one --
#Online.marketing_adstock
#Radio
#Affiliates
#Online.marketing
#Other_adstock
#SEM_adstock
#special_sale_name.xrakshabandhan.sale
#TV
#Radio_adstock
#SEM
#Sponsorship_adstock
#Content.Marketing_adstock
#listprice
#listprice_lag1
#sub_category.xCameraFilmRolls
#is_special_sale_week
#discount

model_3<-lm(formula = tot_gmv ~ payment_type  + 
                      nps    + 
                      Other  + weekly_listprice_infl + monthly_listprice_infl + 
                    tot_gmv_lag1 + discount_lag1  + tot_gmv_lag1_prc_change + 
                    listprice_lag1_prc_change + TV_adstock  + 
                     Affiliates_adstock + 
                sub_category.xCameraAccessory + 
                    sub_category.xCameraBatteryCharger + sub_category.xCameraBatteryGrip + 
                    sub_category.xCameraEyeCup  + 
                    sub_category.xCameraHousing + sub_category.xCameraMicrophone + 
                    sub_category.xCameraMount + sub_category.xCameraRemoteControl + 
                    sub_category.xExtensionTube + sub_category.xFilter + sub_category.xFlash + 
                    sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
                    sub_category.xStrap + sub_category.xTelescope + special_sale_name.xbig.diwali.sale + 
                    special_sale_name.xbsd.5 + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xfhsd + special_sale_name.xpacman  + 
                    special_sale_name.xrepublic.day + special_sale_name.xvalentine.s.day + 
                    discount_kpi.x0.20..Discount, data = train_d)
summary(model_3)
#Multiple R-squared:  0.7649,	Adjusted R-squared:  0.7512 
sort(vif(model_3))

#removing one by one  high p-value
#listprice_lag1_prc_change
#special_sale_name.xbsd.5
#special_sale_name.xvalentine.s.day  
#sub_category.xSoftbox 
#sub_category.xReflectorUmbrella
#discount_lag1
#special_sale_name.xbig.diwali.sale  
#special_sale_name.xrepublic.day 
#weekly_listprice_infl
#sub_category.xCameraHousing
#special_sale_name.xpacman
#special_sale_name.xfhsd
#sub_category.xFlash
#monthly_listprice_infl
#sub_category.xCameraEyeCup
#special_sale_name.xchristmas...new.year.sale
#sub_category.xExtensionTube
#sub_category.xCameraMicrophone
#sub_category.xFilter
#discount_kpi.x0.20..Discount
#sub_category.xCameraBatteryCharger
#sub_category.xCameraBatteryGrip
#sub_category.xCameraRemoteControl
#sub_category.xCameraMount
#sub_category.xTelescope
#sub_category.xCameraAccessory
#sub_category.xStrap
#Affiliates_adstock
model_4<-lm(formula = tot_gmv ~ payment_type  +nps + 
                    Other + tot_gmv_lag1   + tot_gmv_lag1_prc_change + TV_adstock +   
                    sub_category.xLens     , data = train_d)
summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.721,	Adjusted R-squared:  0.7179 

# Model evalution ---------------------------
# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_d)
data.frame( R2 = R2(predictions, test_d$tot_gmv),
            RMSE = RMSE(predictions, test_d$tot_gmv),
            MAE = MAE(predictions, test_d$tot_gmv))

# R2       RMSE       MAE
# 0.702899 0.5544175 0.3254765


final.model.dist<-model_4
summary(final.model.dist)

#k- fold cross validation

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~ payment_type  +nps + 
                       Other + tot_gmv_lag1   + tot_gmv_lag1_prc_change + TV_adstock +   
                       sub_category.xLens   , data = cam_acc.dist, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.5317605  0.7161322  0.3162638
#
#-------------------------------------------------------------------------------
# Distributed lag multiplicative model
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Data preparation -----------
str(cam_acc.dist.multi[3:35])
cam_acc.dist.multi2<-cam_acc.dist.multi[,c(1,2,50:ncol(cam_acc.dist.multi))]

cam_acc.dist.multi1  <- lapply (cam_acc.dist.multi[,3:49] ,function(x) ifelse ( x==0 , 0, log(abs(x),base=exp(1)))*sign(x) )
cam_acc.dist.multi <- cbind(cam_acc.dist.multi1,cam_acc.dist.multi2)
cam_acc.dist.multi<-cam_acc.dist.multi[complete.cases(cam_acc.dist.multi),]

# Model ---------------
set.seed(123)
trainindices= sample(1:nrow(cam_acc.dist.multi), 0.7*nrow(cam_acc.dist.multi))
train_dm = cam_acc.dist.multi[trainindices,]
test_dm = cam_acc.dist.multi[-trainindices,]


model_1 <-lm(tot_gmv~.,data=train_dm)
summary(model_1)
#Multiple R-squared:  0.9327,	Adjusted R-squared:  0.9259 
step <- stepAIC(model_1, direction="both")
step

model_2<-lm(formula = tot_gmv ~ listprice + delay_days + mrp + nps + Digital + 
                    Sponsorship + Content.Marketing + Online.marketing + Affiliates + 
                    SEM + Radio + Other + discount + weekly_listprice_infl + 
                    monthly_listprice_infl + tot_gmv_lag1 + discount_lag1 + listprice_lag1 + 
                    tot_gmv_lag1_prc_change + Digital_adstock + Sponsorship_adstock + 
                    Online.marketing_adstock + Affiliates_adstock + SEM_adstock + 
                    Radio_adstock + payment_type + is_special_sale_week + sub_category.xFlash + 
                    sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
                    sub_category.xStrap + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xpacman + 
                    special_sale_name.xrakshabandhan.sale + special_sale_name.xrepublic.day + 
                    discount_kpi.x0.20..Discount + discount_kpi.x21..50..Discount + 
                    product_type.xMassMarket, data = train_dm)
summary(model_2)
sort(vif(model_2))
#Multiple R-squared:  0.9318,	Adjusted R-squared:  0.9272 
#
#remove one by one by looking at high VIF 
#Online.marketing
#Radio
#Online.marketing_adstock
#Digital
#SEM_adstock
#listprice
#Affiliates
#monthly_listprice_infl
#Sponsorship
#SEM
#Radio_adstock
#discount_kpi.x0.20..Discount
#Content.Marketing 
#Digital_adstock
#product_type.xMassMarket
#listprice_lag1
#is_special_sale_week
model_3<-lm(formula = tot_gmv ~  delay_days + mrp + nps     + 
                       Other + discount + weekly_listprice_infl + 
                     tot_gmv_lag1 + discount_lag1  + 
                    tot_gmv_lag1_prc_change  + Sponsorship_adstock + 
                     Affiliates_adstock + 
                      payment_type  + sub_category.xFlash + 
                    sub_category.xLens + sub_category.xReflectorUmbrella + sub_category.xSoftbox + 
                    sub_category.xStrap + special_sale_name.xchristmas...new.year.sale + 
                    special_sale_name.xdaussera.sale + special_sale_name.xpacman + 
                    special_sale_name.xrakshabandhan.sale + special_sale_name.xrepublic.day  + 
                    discount_kpi.x21..50..Discount , data = train_dm)
summary(model_3)
sort(vif(model_3))
#Multiple R-squared:  0.8955,	Adjusted R-squared:  0.8916 
#
#further remove looking at high p-value
#sub_category.xFlash
#sub_category.xSoftbox
#special_sale_name.xchristmas...new.year.sale
#special_sale_name.xdaussera.sale
#discount_lag1
#special_sale_name.xrepublic.day
#sub_category.xLens
#special_sale_name.xpacman 
#Other
#sub_category.xStrap
#sub_category.xReflectorUmbrella
#delay_days
#weekly_listprice_infl
#special_sale_name.xrakshabandhan.sale
#payment_type
#iscount_kpi.x21..50..Discount
#Sponsorship_adstock
model_4<-lm(formula = tot_gmv ~    mrp + nps+discount  + 
                    tot_gmv_lag1 + tot_gmv_lag1_prc_change   + 
                    Affiliates_adstock    , data = train_dm)
summary(model_4)
sort(vif(model_4))
#Multiple R-squared:  0.8884,	Adjusted R-squared:  0.8873 

# model Evalution ------------------

# Make predictions and compute the R2, RMSE and MAE
predictions <- model_4 %>% predict(test_dm)
data.frame( R2 = R2(predictions, test_dm$tot_gmv),
            RMSE = RMSE(predictions, test_dm$tot_gmv),
            MAE = MAE(predictions, test_dm$tot_gmv))
# R2      RMSE       MAE
# 1 0.8921863 0.5849947 0.4246664
#
final.model.multi.dist <- model_4


#k- fold cross validation

# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(tot_gmv ~    mrp + nps+discount  + 
                       tot_gmv_lag1 + tot_gmv_lag1_prc_change   + 
                       Affiliates_adstock   , data = cam_acc.dist.multi, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)
# RMSE       Rsquared   MAE      
# 0.5994403  0.8878269  0.4404095

##  ----------------------
##  Visulisation of models -------------------------


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
