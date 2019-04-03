#setwd("~/Documents/Learning/IIITb/RFiles/Datafiles/SVM/SVM Dataset")


library(caret)
library(kernlab)
# 1. Business Understanding:---------------------------------------------------
# The objective is to develop a SVM model that can correctly identify the digit 
# (between 0-9) written in an image. 

# 2. Data Understanding: -------------------------------------------------------
#MNIST data is an image of hand wrriten digit dataset : 
#image consists (28 * 28) pixcels = 784 points stored in 784 columns and first 
#column is the digit of the image thus making 785 columns

# Data loading -----------------------------------------------------------------
#data files are without headers , R will give headers 
train<-read.csv('mnist_train.csv',header=FALSE,sep = ",")
test<-read.csv('mnist_test.csv',header=FALSE,sep=",")

dim(train)#[1] 60000   785
dim(test)#[1] 10000  785

#Structure of the dataset
str(train)
str(test)

#printing first few rows
head(train)
head(test)

#Exploring the data
summary(train)
summary(test)

# Data Cleaning ---------------------------------------------------------------
#checking missing value
sum(sapply(train, function(x) sum(is.na(x))) > 0)
sum(sapply(test, function(x) sum(is.na(x))) > 0)

#check for duplicates
sum(duplicated(train)) # 0 duplicates
sum(duplicated(test))  # 0 duplicates

# Changing col name of dependent variable
colnames(train)[1]<-'label'
colnames(test)[1]<-'label'

#Making our target class to factor
train$label<-factor(train$label)
test$label<-factor(test$label)

max(train[,-1]) #255
#Normalize the data - convert pixel values from 0-255 to 0-1
train[,-1]<-train[,-1] /255
test[,-1]<-test[,-1] /255

# 3. Data Preparation ----------------------------------------------------------

#lets see the frequency on labels
table(train$label)
round(prop.table(table(train$label)),2)

table(test$label)
round(prop.table(table(test$label)),2)

#frequencies of labels are somewhat equal around 0.1 for each class in test 
#and train datasets

# Its big dataset and takes high computational time 
# Reducing its size by randomly selecting 15% of data from train and test

set.seed(150)
train.indices <- sample(1:nrow(train), 0.15*nrow(train))
train.small <- train[train.indices,]
test.indices <- sample(1:nrow(test), 0.15*nrow(test))
test.small <- test[test.indices,]

#checking dimentions
dim(train.small)
dim(test.small)

# 4. Model Building ------------------------------------------------------------
# Model with C =1
Linear_model_1<- ksvm(label ~ ., data = train.small,
                      kernel="vanilladot",C=1,scaled=FALSE)
Linear_model_1
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# Linear (vanilla) kernel function. 
# Number of Support Vectors : 2588
Eval_model_1<- predict(Linear_model_1, test.small)
confusionMatrix(Eval_model_1,test.small$label)
# Accuracy : 0.918

# linear model with C=10
Linear_model_2<- ksvm(label ~ ., data = train.small,kernel="vanilladot",C=10,scaled=FALSE)
Eval_model_2<- predict(Linear_model_2, test.small)
confusionMatrix(Eval_model_2,test.small$label)
#Accuracy : 0.9173
#
# linear model with C=100
Linear_model_3<- ksvm(label ~ ., data = train.small,kernel="vanilladot",C=100,scaled = FALSE)
Eval_model_3<- predict(Linear_model_3, test.small)
confusionMatrix(Eval_model_3,test.small$label)
#Accuracy : 0.9173

#C=1 gives best accuracy in above models lets find best C around 1
# Cross validation for find optimum C value
set.seed(123)
fit.svm.linear <- train(label ~ .,data=train.small,
                method="svmLinear", 
                metric="Accuracy", 
                tuneGrid= expand.grid(C=c(0.01,0.2,0.5,1,1.2)), 
                trControl=trainControl(method="cv", number=5),scaled=FALSE)

print(fit.svm.linear) 
plot(fit.svm.linear)

# C     Accuracy   Kappa    
# 0.01  0.9224465  0.9137753
# 0.20  0.9173354  0.9080956
# 0.50  0.9103351  0.9003098
# 1.00  0.9064471  0.8959861
# 1.20  0.9054471  0.8948745
# 
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 0.01.

Eval_model_CV<- predict(fit.svm.linear, test.small)
confusionMatrix(Eval_model_CV,test.small$label)
#Accuracy = 0.9307

# C can be less than 0.01  so lets tune it further
fit.svm.linear1 <- train(label ~ .,data=train.small,
                        method="svmLinear", 
                        metric="Accuracy", 
                        tuneGrid= expand.grid(C=seq(0.001,0.1,0.001)), 
                        trControl=trainControl(method="cv", number=5),scaled=FALSE)
print(fit.svm.linear1) 

# 0.025  0.9268880  0.9187142
# 0.026  0.9271102  0.9189610
# 0.027  0.9275551  0.9194562
# 0.028  0.9273326  0.9192087
# 0.029  0.9273322  0.9192085
# 0.030  0.9269984  0.9188373
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 0.027.
plot(fit.svm.linear1)

Eval_model_CV<- predict(fit.svm.linear1, test.small)
confusionMatrix(Eval_model_CV,test.small$label)

#Accuracy : 0.9353 



# 4.2 Using  Polynomial kernel------------------------------------------
Model_poly <-ksvm(label ~ ., data = train.small, kernel="polydot",scaled=FALSE)
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# Polynomial kernel function. 
# Hyperparameters : degree =  1  scale =  1  offset =  1 
# Number of Support Vectors : 2561 
Eval_poly<-predict(Model_poly, test.small)
confusionMatrix(Eval_poly,test.small$label)
#Accuracy : 0.918


Model_poly_1 <-ksvm(label ~ ., data = train.small, kernel="polydot",scaled=FALSE,
                  kpar=list(degree=3,scale=1,offset = 1))
Eval_poly_1<-predict(Model_poly_1, test.small)
confusionMatrix(Eval_poly_1,test.small$label)
#Accuracy : 0.9607

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
grid = expand.grid(C=c(1,2,3),degree = c(1,3,4,5),scale=1)

set.seed(100)
fit.svm.poly <- train(label ~ .,data=train.small,method="svmPoly", metric=metric, 
                     tuneGrid=grid, trControl=trainControl,scaled=FALSE)
print(fit.svm.poly) 
# C  degree  Accuracy   Kappa    
# 1  1       0.9081112  0.8978386
# 1  3       0.9513331  0.9458933
# 1  4       0.9387763  0.9319246
# 1  5       0.9183315  0.9091694
# 2  1       0.9071115  0.8967267
# 2  3       0.9513331  0.9458933
# 2  4       0.9387763  0.9319246
# 2  5       0.9183315  0.9091694
# 3  1       0.9064448  0.8959853
# 3  3       0.9513331  0.9458933
# 3  4       0.9387763  0.9319246
# 3  5       0.9183315  0.9091694
# 
# Tuning parameter 'scale' was held constant at a value of 1
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were degree = 3, scale = 1 and C = 1.
plot(fit.svm.poly)

Eval_model_CV_poly<- predict(fit.svm.poly, test.small)
confusionMatrix(Eval_model_CV_poly,test.small$label)
# Accuracy : 0.9607          
# 95% CI : (0.9496, 0.9699)

# Accurcy is improved by  3% fromlinear to poly kernel
# Lets tune the hyper paramerter again
# Keeping Degree = 3 but with smaller C

Model_poly_2 <-ksvm(label ~ ., data = train.small, kernel="polydot",scaled=FALSE,
                    kpar=list(degree=3,scale=0.1,offset = 1),C=0.1)
Eval_poly_2<-predict(Model_poly_2, test.small)
confusionMatrix(Eval_poly_2,test.small$label)
#Accuracy : 0.9613 



# 4.3 Using RBF Kernel-------------------------------------------------
Model_RBF1 <- ksvm(label~ ., data = train.small,kernel = "rbfdot",scaled=F)
# SV type: C-svc  (classification) 
# parameter : cost C = 1 
# Gaussian Radial Basis kernel function. 
# Hyperparameter : sigma =  0.010799349700729
# Number of Support Vectors : 3504 

Eval_RBF1<- predict(Model_RBF1, test.small)
confusionMatrix(Eval_RBF1, test.small$label)
#Accuracy : 0.9587
#using RBF kernel we get more accuracy

Model_RBF2 <- ksvm(label~ ., data = train.small,kernel = "rbfdot",scaled=F,
                   kpar=list(sigma=0.02))
Eval_RBF2<- predict(Model_RBF2, test.small)
confusionMatrix(Eval_RBF2, test.small$label)
# Accuracy : 0.9687          
# 95% CI : (0.9585, 0.9769)
# No Information Rate : 0.1187          
# P-Value [Acc > NIR] : < 2.2e-16     

Model_RBF3 <- ksvm(label~ ., data = train.small,kernel = "rbfdot",scaled=F,
                   kpar=list(sigma=0.03),C=2.88)

Eval_RBF3<- predict(Model_RBF3, test.small)
confusionMatrix(Eval_RBF3, test.small$label)



# 5 Hyperparameter tuning and cross validation----------------------------------
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
#grid <- expand.grid(.sigma=c(0.01,0.02,0.03), .C=c(0.5,1,10))
grid_rbf = expand.grid(.C=c(1,2,3),.sigma = c(0.01,0.02,0.03,0.04))

fit.svm <- train(label~.,data=train.small,method="svmRadial", metric=metric, 
                     tuneGrid=grid_rbf, trControl=trainControl,scaled=F)

print(fit.svm) 
plot(fit.svm)

# C  sigma  Accuracy   Kappa    
# 1  0.01   0.9521110  0.9467595
# 1  0.02   0.9605562  0.9561501
# 1  0.03   0.9617783  0.9575095
# 1  0.04   0.9616673  0.9573872
# 2  0.01   0.9572225  0.9524432
# 2  0.02   0.9642233  0.9602271
# 2  0.03   0.9657786  0.9619563
# 2  0.04   0.9642226  0.9602276
# 3  0.01   0.9585554  0.9539252
# 3  0.02   0.9650011  0.9610914
# 3  0.03   0.9661120  0.9623270
# 3  0.04   0.9642228  0.9602277
# 
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 0.03 and C = 3.

Eval_RBF_CV<- predict(fit.svm, test.small)
confusionMatrix(Eval_RBF_CV, test.small$label)
# Accuracy : 0.9727  

#evaluating with whole test data
Eval_RBF_CV<- predict(fit.svm, test)
confusionMatrix(Eval_RBF_CV, test$label)
# Accuracy : 0.9706  

grid_rbf = expand.grid(.C=seq(2,3,0.02),.sigma = c(0.025,0.035))

fit.svm1 <- train(label~.,data=train.small,method="svmRadial", metric=metric, 
                 tuneGrid=grid_rbf, trControl=trainControl,scaled=F)

print(fit.svm1) 
# 2.86  0.025  0.9654438  0.9615840
# 2.86  0.035  0.9647769  0.9608431
# 2.88  0.025  0.9655549  0.9617075
# 2.88  0.035  0.9647769  0.9608431
# 2.90  0.025  0.9655549  0.9617075
# 2.90  0.035  0.9647769  0.9608431
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 0.025 and C = 2.88.
plot(fit.svm1)

Eval_RBF_CV1<- predict(fit.svm1, test.small)
confusionMatrix(Eval_RBF_CV1, test.small$label)
# Accuracy : 0.972           
# 95% CI : (0.9623, 0.9797)


grid_rbf = expand.grid(.C=2.5,.sigma = seq(0.025,0.03,0.001))
fit.svm2 <- train(label~.,data=train.small,method="svmRadial", metric=metric, 
                  tuneGrid=grid_rbf, trControl=trainControl,scaled=F)

print(fit.svm2) 
# sigma  Accuracy   Kappa    
# 0.025  0.9660013  0.9622036
# 0.026  0.9660013  0.9622037
# 0.027  0.9656681  0.9618332
# 0.028  0.9655571  0.9617100
# 0.029  0.9652236  0.9613393
# 0.030  0.9646683  0.9607221

# Tuning parameter 'C' was held constant at a value of 2.5
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 0.026 and C = 2.5.
plot(fit.svm2)
Eval_RBF_CV2<- predict(fit.svm2, test.small)
confusionMatrix(Eval_RBF_CV2, test.small$label)
#Accuracy : 0.972

#lets test model on whole test data
Eval_RBF_CV2.1<- predict(fit.svm2, test)
confusionMatrix(Eval_RBF_CV2.1, test$label)
#Accuracy : 0.9704 


# Summary
# Linear kernal models performance  
# ------------------------------
# 1) C=1   Accuracy : 0.918 -- 91.8%
# 2) C=10   #Accuracy : 0.9173 -- 91.8%
# 3) C=100  #Accuracy : 0.9173 -- 91.7%
#  As C  increases accuracy decrease a bit no big change so keeing C around 1 
#  Cross - Validation for svmlinear for C= c(0.01,0.2,0.5,1,1.2), 
#  for C=0.01  Accuracy = 0.9307 -- 93%
#  Refining C further  C= seq(0.001,0.1,0.001)
#  for C= 0.027. Accuracy : 0.9353 
#  After tuning of parameter C Accuracy is still 93.5%
# 
# Polynomial kernal model performance 
# -------------------------------
# 1) C = 1, degree = 1 #Accuracy : 0.918  -- 91%
# 2) C = 1, degree = 3 #Accuracy : 0.9607 -- 96%
# 5 fold Cross Validation for svmPoly method 
#  degree = 3, scale = 1 and C = 1. Accuracy : 0.9607 -- 96%
#  with scale = 0.1 Accuracy : 0.9613  -- 96.1%
#  
# Radial kernal model performance
# --------------------------------
#  1) C = 1 and sigma = 0.0107 Accuracy : 0.9587 -- 95%
#  2) C = 1 and sigma = 0.02 Accuracy : 0.9687 -- 96.8%
#  5 Fold Cross Validation for svmRadial method
#  1)
#  for .C=c(1,2,3),.sigma = c(0.01,0.02,0.03,0.04)
#  Selected Values : sigma = 0.03 and C = 3.. 
#  for 15% test data :Accuracy 0.9727 -- 97%
#  for 100% test data :Accuracy 0.9706 -- 97%
#  2)
#  for C=seq(2,3,0.02),.sigma = c(0.025,0.035)
#  Selected Values : sigma = 0.025 and C = 2.88.
#  for 15% test data :Accuracy 0.9727 -- 97%
#  3)
#  for C=2.5,.sigma = seq(0.025,0.03,0.001)
#  selected values : sigma = 0.026 and C = 2.5
#  for 15% test data :Accuracy 0.9727 -- 97%
#  for 100% test data :Accuracy 0.9704 -- 97%

# Conclusion----------------------------------------------------------

# RBF Kernal is the optimum model with
# optimum hyperparameter sigma = 0.026 and C = 2.5
# gives 97% of accuracy to whole test data
