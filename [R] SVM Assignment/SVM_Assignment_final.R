#Loading libraries
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)

#Loading Dataset
mnist_train <- read.csv("mnist_train.csv",stringsAsFactors=FALSE)
mnist_test <- read.csv("mnist_test.csv",stringsAsFactors=FALSE)

#Data preparation
#Giving custom column names
colnames(mnist_train) <- seq(0,784,1)
colnames(mnist_test) <- seq(0,784,1)
nrow(mnist_train) #Too many data

#Time to sample data
train_size <- 0.15*nrow(mnist_train)
set.seed(100)
indices <- sample(seq(1:nrow(mnist_train)),train_size)

#Sample selection
mnist_train <- mnist_train[indices,]

#convert digit to factor
mnist_test$`0` <- as.factor(mnist_test$`0`)
mnist_train$`0` <- as.factor(mnist_train$`0`)

#Check the data
str(mnist_test)
str(mnist_train)
nrow(mnist_train)
nrow(mnist_test)

#Data cleaning
sapl <- sapply(mnist_train, function(x) sum(is.na(x))) 
sapl <- as.data.frame(sapl)
sum(sapl) #0
#No NA found

sum(duplicated(mnist_train)) #No duplicates

###Lets try a linear model
Model_linear <- ksvm(`0`~ ., data = mnist_train, scaled = FALSE, kernel = "vanilladot",C=1)
Eval_linear<- predict(Model_linear, mnist_test)
confusionMatrix(Eval_linear,mnist_test$`0`)
#Overall Accuracy 91.39% for C=1

#Lets try a huge C=1000000
Model_linear <- ksvm(`0`~ ., data = mnist_train, scaled = FALSE, kernel = "vanilladot",C=1000000)
Eval_linear<- predict(Model_linear, mnist_test)
confusionMatrix(Eval_linear,mnist_test$`0`)
#Overall Accuracy 91.39% for C=1000000

#Lets try a small C=0.000001
Model_linear <- ksvm(`0`~ ., data = mnist_train, scaled = FALSE, kernel = "vanilladot",C=0.000001)
Eval_linear<- predict(Model_linear, mnist_test)
confusionMatrix(Eval_linear,mnist_test$`0`)
#Overall Accuracy 93% for C=0.000001

###LINEAR CROSS VALIDATION###
trainControl <- trainControl(method = 'cv',number = 5)
metric <- 'Accuracy'
#((0.000001 - 0.000000001)/5) #Getting difference
grid <- expand.grid(C=seq(0.000000001,0.000001, by=1.998e-07)) #Data frame of c values

#Performing 5-fold cross validation
fit.svm <- train(`0`~., data=mnist_train, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl,scaled=FALSE)
plot(fit.svm)

##choosing c as 0.000000001

#Valdiating the model after cross validation on test data

evaluate_linear_test<- predict(fit.svm, mnist_test)
confusionMatrix(evaluate_linear_test, mnist_test$`0`)

#Overall Statistics
#Accuracy : 0.93
#Accuracy is same after training

###Lets try RBF Kernal###
Model_RBF <- ksvm(`0`~ ., data = mnist_train, scaled = FALSE, kernel = "rbfdot")
Eval_RBF<- predict(Model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnist_test$`0`)

#Overall Statistics
#Accuracy : 0.9575
#Sensitivity and specificity over 93

#Modyfying sigma and C
Model_RBF <- ksvm(`0`~ ., data = mnist_train, scaled = FALSE, kernel = "rbfdot",
                  .sigma=0.000001,C=1)
Eval_RBF<- predict(Model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnist_test$`0`)


#####################################################################
#Hyperparameter tuning and Cross Validation - Non-Linear - SVM 
######################################################################

#Overall Statistics
#Accuracy : 95.77% . Has greatly improved from Linear

##Time to cross validate to find the proper sigma value
grid_rbf <- expand.grid(.sigma=c(0.000001, 0.0001), .C=c(1,100) )
fit2.svm <- train(`0`~., data=mnist_train, method="svmRadial",scaled = FALSE, metric=metric, 
                 tuneGrid=grid_rbf, trControl=trainControl)

plot(fit2.svm)

#Modyfying sigma and C
Model_RBF <- ksvm(`0`~ ., data = mnist_train, scaled = FALSE, kernel = "rbfdot",
                  .sigma=1e-7,C=1)
Eval_RBF<- predict(Model_RBF, mnist_test)

#confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,mnist_test$`0`)

#sigma  C    Accuracy   Kappa    
#1e-06    1  0.9475490  0.9416998
#1e-06  100  0.9493271  0.9436758
#1e-04    1  0.1145683  0.0000000
#1e-04  100  0.1145683  0.0000000
#Accuracy seems to be highest near very low sigma values and C can be 1
#Data needs to be trained at bigger intervals

#Using Polynomial Kernel
Model_poly <- ksvm(`0`~ ., data = mnist_train, scaled = FALSE, kernel = "polydot")
Eval_poly<- predict(Model_poly, mnist_test)
confusionMatrix(Eval_poly,mnist_test$`0`)
#Accuracy is 91.39 % which is same as linear kernal, meaning the no misclassifications are
#Same in linear and polynomial and RBF is the best choice

#Choosing RBF model with sigma and C values
#Model_RBF

#Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8
#Sensitivity           0.98673   0.9894  0.95349  0.96139  0.96130  0.94058  0.96764  0.93184  0.94045
#Specificity           0.99590   0.9971  0.99465  0.99410  0.99401  0.99528  0.99635  0.99610  0.99579
#Pos Pred Value        0.96315   0.9774  0.95349  0.94824  0.94589  0.95125  0.96563  0.96472  0.96017
#Neg Pred Value        0.99855   0.9986  0.99465  0.99565  0.99578  0.99419  0.99657  0.99223  0.99359
#Prevalence            0.09801   0.1135  0.10321  0.10101  0.09821  0.08921  0.09581  0.10271  0.09741
#Detection Rate        0.09671   0.1123  0.09841  0.09711  0.09441  0.08391  0.09271  0.09571  0.09161
#Detection Prevalence  0.10041   0.1149  0.10321  0.10241  0.09981  0.08821  0.09601  0.09921  0.09541
#Balanced Accuracy     0.99132   0.9932  0.97407  0.97775  0.97766  0.96793  0.98200  0.96397  0.96812

#Class: 9
#Sensitivity           0.93954
#Specificity           0.99377
#Pos Pred Value        0.94422
#Neg Pred Value        0.99322
#Prevalence            0.10091
#Detection Rate        0.09481
#Detection Prevalence  0.10041
#Balanced Accuracy     0.96666
