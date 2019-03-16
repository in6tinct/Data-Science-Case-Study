#Load libraries

library(caTools)
library(MASS)
library(car)
library(caret)
library(ggplot2)
library(gridExtra)
library(ROCR)

#Load Data
emp_surv <- read.csv("employee_survey_data.csv",stringsAsFactors = FALSE)
gen_data <- read.csv("general_data.csv",stringsAsFactors = FALSE)
man_surv <- read.csv("manager_survey_data.csv",stringsAsFactors = FALSE)
in_time <- read.csv("in_time.csv",stringsAsFactors = FALSE)
out_time <- read.csv("out_time.csv",stringsAsFactors = FALSE)

###Check data
str(emp_surv) #4 Variables all Integers
str(gen_data) #Could have outliers
str(man_surv)
str(in_time)

#Check for NA and duplicates
sum(is.na(emp_surv)) #83 NA's present
sapply(emp_surv, function(x) sum(is.na(x)))
sum(duplicated(emp_surv)) # no duplicates

sum(is.na(gen_data)) #28 NA's present
sum(duplicated(gen_data)) # no duplicates
sapply(gen_data, function(x) sum(is.na(x)))

sum(is.na(man_surv)) #no NA
sum(duplicated(man_surv)) # no duplicates
sum(is.na(in_time)) #109080 NA
sum(duplicated(in_time)) # no duplicates
sum(is.na(out_time)) #109080 NA
sum(duplicated(out_time)) # no duplicates

sapply(in_time, function(x) sum(is.na(x)))
sapply(out_time, function(x) sum(is.na(x)))

#NA Comparison
in_time_Na <- data.frame(sapply(in_time, function(x) sum(is.na(x))))
out_time_Na <- data.frame(sapply(out_time, function(x) sum(is.na(x))))
sum(!(in_time_Na[,1] == out_time_Na[,1]))
#The NA's must be for the holidays and absent days for each employee. Not removing them for now.

#Remove NA
emp_surv <- na.omit(emp_surv)
gen_data <- na.omit(gen_data)

###Check for outliers
boxplot.stats(emp_surv$EnvironmentSatisfaction)$out #none
boxplot.stats(emp_surv$JobSatisfaction)$out #none
boxplot.stats(emp_surv$WorkLifeBalance)$out #none
boxplot.stats(gen_data$Age)$out #none
boxplot.stats(gen_data$DistanceFromHome)$out #none
boxplot.stats(gen_data$EmployeeCount)$out #none
boxplot.stats(gen_data$MonthlyIncome)$out #Some outliers
boxplot.stats(gen_data$NumCompaniesWorked)$out ##Some outliers
boxplot.stats(gen_data$PercentSalaryHike)$out #none
boxplot.stats(gen_data$StandardHours)$out #none
boxplot.stats(gen_data$StockOptionLevel)$out #Some outliers
boxplot.stats(gen_data$TotalWorkingYears)$out #Some outliers
boxplot.stats(gen_data$TrainingTimesLastYear)$out #Some outliers
boxplot.stats(gen_data$YearsAtCompany)$out #Some outliers
boxplot.stats(gen_data$YearsSinceLastPromotion)$out #Some outliers
boxplot.stats(gen_data$YearsWithCurrManager)$out #Some outliers
boxplot.stats(man_surv$JobInvolvement)$out #none
boxplot.stats(man_surv$PerformanceRating)$out #Some outliers

#Lets analyze the data and decide for the outliers
quantile(gen_data$MonthlyIncome,seq(0,1,0.01)) #Need outlier fixing - picking 180000
quantile(gen_data$NumCompaniesWorked,seq(0,1,0.01)) #No Need outlier fixing
quantile(gen_data$StockOptionLevel,seq(0,1,0.01)) #No Need outlier fixing
quantile(gen_data$TotalWorkingYears,seq(0,1,0.01)) #Need outlier fixing - picking 26
quantile(gen_data$TrainingTimesLastYear,seq(0,1,0.01)) #No Need outlier fixing
quantile(gen_data$YearsAtCompany,seq(0,1,0.01)) #Need outlier fixing - picking 24
quantile(gen_data$YearsSinceLastPromotion,seq(0,1,0.01)) #Need outlier fixing - picking 11
quantile(gen_data$YearsWithCurrManager,seq(0,1,0.01)) #Need outlier fixing - picking 11
quantile(man_surv$PerformanceRating,seq(0,1,0.01)) #No Need outlier fixing

#Fixing outliers : Outliers will be maxed out at the level picked
gen_data[which(gen_data$MonthlyIncome>180000),'MonthlyIncome'] <- 180000
gen_data[which(gen_data$TotalWorkingYears>26),'TotalWorkingYears'] <- 26
gen_data[which(gen_data$YearsAtCompany>24),'YearsAtCompany'] <- 24
gen_data[which(gen_data$YearsSinceLastPromotion>11),'YearsSinceLastPromotion'] <- 11
gen_data[which(gen_data$YearsWithCurrManager>11),'YearsWithCurrManager'] <- 11

#Attrition Rate
nrow(gen_data[which(gen_data$Attrition=="Yes"),])/nrow(gen_data)
#16.08% Attrition Rate

###EDA

str(gen_data)
a <- ggplot(gen_data,aes(x=BusinessTravel,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=BusinessTravel,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Rare Travellers leave more however the percentage of frequent travellers are leaving more

a <- ggplot(gen_data,aes(x=Education,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=Education,fill=Attrition))+geom_bar(position = 'fill') 
grid.arrange(a,b)
#Average Education leveled people leave more, the % attrition is similar in all
#so this should not have any relavance

a <- ggplot(gen_data,aes(x=EducationField,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=EducationField,fill=Attrition))+geom_bar(position = 'fill') 
grid.arrange(a,b)
#Over 30% of human resources field leave

a <- ggplot(gen_data,aes(x=Gender,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=Gender,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#No signicance

a <- ggplot(gen_data,aes(x=JobLevel,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=JobLevel,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#No signicance

a <- ggplot(gen_data,aes(x=JobRole,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=JobRole,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Nothing conclusive

a <- ggplot(gen_data,aes(x=MaritalStatus,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=MaritalStatus,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Single people leave more

a <- ggplot(gen_data,aes(x=NumCompaniesWorked,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=NumCompaniesWorked,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#1 year leave a lot

ggplot(gen_data,aes(y=MonthlyIncome,x=Attrition))+geom_boxplot()
#The 3rd quitile show people who leave get lower salary

ggplot(gen_data,aes(y=PercentSalaryHike,x=Attrition))+geom_boxplot()
a <- ggplot(gen_data,aes(x=PercentSalaryHike,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=PercentSalaryHike,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Looks like greater the hike the less people leave. #More data required

a <- ggplot(gen_data,aes(x=StockOptionLevel,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=StockOptionLevel,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Nothing conclusive

a <- ggplot(gen_data,aes(x=TotalWorkingYears,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=TotalWorkingYears,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#close to 50% of the people with 0-1 working exp leave.

a <- ggplot(gen_data,aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=TrainingTimesLastYear,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#0 - Leaves more, 6 - the least

a <- ggplot(gen_data,aes(x=YearsAtCompany,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=YearsAtCompany,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#0-1years people leave the most

a <- ggplot(gen_data,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=YearsSinceLastPromotion,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Nothing conclusive

a <- ggplot(gen_data,aes(x=YearsWithCurrManager,fill=Attrition))+geom_bar()
b <- ggplot(gen_data,aes(x=YearsWithCurrManager,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Nothing conclusive

#Merging for analysis #Sorry i forgot
gen_data_new <- merge(emp_surv,gen_data,by=c("EmployeeID"))
gen_data_new <- merge(gen_data_new,man_surv,by=c("EmployeeID"))
str(gen_data_new)

a <- ggplot(gen_data_new,aes(x=EnvironmentSatisfaction,fill=Attrition))+geom_bar()
b <- ggplot(gen_data_new,aes(x=EnvironmentSatisfaction,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Lower env satisfied people leave more

a <- ggplot(gen_data_new,aes(x=JobSatisfaction,fill=Attrition))+geom_bar()
b <- ggplot(gen_data_new,aes(x=JobSatisfaction,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Lower job satisfied people leave more

a <- ggplot(gen_data_new,aes(x=WorkLifeBalance,fill=Attrition))+geom_bar()
b <- ggplot(gen_data_new,aes(x=WorkLifeBalance,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Lower work life balance people leave more

a <- ggplot(gen_data_new,aes(x=JobInvolvement,fill=Attrition))+geom_bar()
b <- ggplot(gen_data_new,aes(x=JobInvolvement,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Nothing conclusive

a <- ggplot(gen_data_new,aes(x=PerformanceRating,fill=Attrition))+geom_bar()
b <- ggplot(gen_data_new,aes(x=PerformanceRating,fill=Attrition))+geom_bar(position = 'fill')
grid.arrange(a,b)
#Looks like higher rated people leave more although percentage is almost same

###DATA CLEANING###
#Time for dummies and levels
gen_data$Attrition <- as.factor(gen_data$Attrition)
levels(gen_data$Attrition) <- c(0,1) #0 for no 1 for yes

levels(as.factor(gen_data$Over18)) #Over18 data not needed as all are over18
levels(as.factor(gen_data$StandardHours)) #StandardHours not needed
levels(as.factor(gen_data$EmployeeCount)) #EmployeeCount not needed

gen_data <- gen_data[,-c(16,18)]

#Creating dummy variables
dummy <- data.frame(model.matrix(~BusinessTravel+Department+
                                   EducationField+Gender+JobRole+
                                   MaritalStatus, data=gen_data))
dummy <- dummy[,-1]
gen_data2 <- cbind(gen_data,dummy)
str(gen_data2)
gen_data <- gen_data2[,-c(3,4,7,8,10,12,13)] #forgot to remove EmployeeCount so removing here alongwith the rest of the character columns

###Average working hours of each employee
time <- in_time

#Time to get the work hours from in and out time
in_time[,-1] <- lapply(in_time[,-1],function(x) as.POSIXct(strptime(x,"%Y-%m-%d %H:%M:%S")))
out_time[,-1] <- lapply(out_time[,-1],function(x) as.POSIXct(strptime(x,"%Y-%m-%d %H:%M:%S")))

time[,-1] <- out_time[,-1] - in_time[,-1]
time[,-1] <- lapply(time[,-1], function(x) as.double(x))
str(time)
time_diff <- as.data.frame(rowMeans(time[,-1],na.rm = T))
avg_worktime <- cbind(time[,1],time_diff)
colnames(avg_worktime) <- c("EmployeeID","Average_work") #New dataframe with average workhours of every employee

#Merge it all together
merge1 <- merge(emp_surv,gen_data,by = c("EmployeeID"))
merge2 <- merge(merge1,man_surv,by = c("EmployeeID"))
df <- merge(merge2,avg_worktime,by = c("EmployeeID"))

#Data stuff are done it seems

#Separate data
# splitting the data between train and test
set.seed(100)

indices = sample.split(df$Attrition, SplitRatio = 0.7)
train = df[indices,]
test = df[!(indices),]

########################################################################
# Logistic Regression: 

model1 <- glm(Attrition~.,data = train,family = "binomial")
summary(model1) # AIC 2138.5,Null deviance: 2661.4,Residual deviance: 2056.5

model2 <- stepAIC(model1,direction = "both")
summary(model2) #AIC: 3014.8,Null deviance: 3804.3,Residual deviance: 2954.8 
vif(model2)

model3 <- glm(Attrition~EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+Age+
                NumCompaniesWorked+TotalWorkingYears+TrainingTimesLastYear+
                YearsSinceLastPromotion+YearsWithCurrManager+
                BusinessTravelTravel_Frequently+BusinessTravelTravel_Rarely+
                EducationFieldLife.Sciences+EducationFieldMarketing+EducationFieldOther+
                EducationFieldMedical+EducationFieldTechnical.Degree+
                JobRoleManager+JobRoleManufacturing.Director+JobRoleSales.Executive+
                JobRoleResearch.Director+
                MaritalStatusSingle+Average_work
                ,data=train,family="binomial")
summary(model3)
vif(model3)

model4 <- glm(Attrition~EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+Age+
                NumCompaniesWorked+TotalWorkingYears+TrainingTimesLastYear+
                YearsSinceLastPromotion+YearsWithCurrManager+
                BusinessTravelTravel_Frequently+BusinessTravelTravel_Rarely+
                EducationFieldLife.Sciences+EducationFieldMarketing+
                EducationFieldOther + EducationFieldMedical+ EducationFieldTechnical.Degree+
                JobRoleManufacturing.Director+
                JobRoleResearch.Director+MaritalStatusSingle+Average_work
              ,data=train,family="binomial")
summary(model4)
vif(model4)

model5 <- glm(Attrition~EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+Age+
                NumCompaniesWorked+TotalWorkingYears+TrainingTimesLastYear+
                YearsSinceLastPromotion+YearsWithCurrManager+
                BusinessTravelTravel_Frequently+BusinessTravelTravel_Rarely+
                EducationFieldLife.Sciences+EducationFieldMarketing+
                EducationFieldOther + EducationFieldMedical+ EducationFieldTechnical.Degree+
                JobRoleManufacturing.Director+
                +MaritalStatusSingle+Average_work
              ,data=train,family="binomial")
summary(model5)
vif(model5)

model6 <- glm(Attrition~EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+Age+
                NumCompaniesWorked+TotalWorkingYears+TrainingTimesLastYear+
                YearsSinceLastPromotion+YearsWithCurrManager+
                BusinessTravelTravel_Frequently+
                EducationFieldLife.Sciences+EducationFieldMarketing+
                EducationFieldOther + EducationFieldMedical+ EducationFieldTechnical.Degree+
                JobRoleManufacturing.Director+
                +MaritalStatusSingle+Average_work
              ,data=train,family="binomial")
summary(model6)
vif(model6)

#EducationFieldLife.Sciences ha high vif
model7 <- glm(Attrition~EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+Age+
                NumCompaniesWorked+TotalWorkingYears+TrainingTimesLastYear+
                YearsSinceLastPromotion+YearsWithCurrManager+
                BusinessTravelTravel_Frequently+
                EducationFieldMarketing+
                EducationFieldOther + EducationFieldMedical+ EducationFieldTechnical.Degree+
                JobRoleManufacturing.Director+
                +MaritalStatusSingle+Average_work
              ,data=train,family="binomial")
summary(model7)
vif(model7)

#Education has no effect
model8 <- glm(Attrition~EnvironmentSatisfaction+JobSatisfaction+WorkLifeBalance+Age+
                NumCompaniesWorked+TotalWorkingYears+TrainingTimesLastYear+
                YearsSinceLastPromotion+YearsWithCurrManager+
                BusinessTravelTravel_Frequently+
                JobRoleManufacturing.Director+
                +MaritalStatusSingle+Average_work
              ,data=train,family="binomial")
summary(model8)
vif(model8) #Vif good for all


final_model <- model8

#Prediction
str(test)
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-1])
summary(test_pred)

# Let's find out the optimal probalility cutoff 
test_actual_attr <- factor(ifelse(test$Attrition==1, "Yes", "No"))
perform_fn <- function(cutoff) 
{
  test_pred_attr <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(test_pred_attr, test_actual_attr, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

OUT #Now contains "sensitivity", "specificity", "accuracy"
data <- as.data.frame(OUT)
colnames(data) <- c("sensitivity", "specificity", "accuracy")

#plot_data <- data.frame(test_actual_attr,test_pred_attr)
s[which.max(data$accuracy)] #Max accuracy at 0.44
cutoff <- s[which((abs(OUT[,1]-OUT[,2]))<0.01)] #intersection point 0.178

#Plotting a chart to check the intersection point
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

#Time for confusion
test_pred_attr_insctn <- factor(ifelse(test_pred >= 0.178, "Yes", "No"))
table(test_pred_attr_insctn,test_actual_attr)

test_pred_attr_accurcy <- factor(ifelse(test_pred >= 0.44, "Yes", "No"))
table(test_pred_attr_accurcy,test_actual_attr)

#Testing Data ROCR
#For 0.44 max accuracy
pred_attr <- ifelse(test_pred_attr_accurcy=="Yes",1,0)
actual_attr <- ifelse(test_actual_attr=="Yes",1,0)
pred_object_test<- prediction(pred_attr,actual_attr)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)  #0.32

#For 0.178 intersection
pred_attr <- ifelse(test_pred_attr_insctn=="Yes",1,0)
actual_attr <- ifelse(test_actual_attr=="Yes",1,0)
pred_object_test<- prediction(pred_attr,actual_attr)
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])
max(ks_table_test)  #0.50

#KS stuff
ks_table <- function(cut){
  ks_pred <- factor(ifelse(test_pred >= cut, "Yes", "No"))
  ks_pred_attr <- ifelse(ks_pred=="Yes",1,0)
  ks_pred_object_test<- prediction(ks_pred_attr,actual_attr)
  ks_measures_test<- performance(ks_pred_object_test, "tpr", "fpr")
  ks_table_test <- attr(ks_measures_test, "y.values")[[1]] - 
    (attr(ks_measures_test, "x.values")[[1]])
  return(max(ks_table_test))
}
ks_data <- data.frame((matrix(vector(),ncol=2)))

for (i in s) {
  d <- ks_table(i)
  ks_data <- rbind(c(i,d),ks_data)
}
colnames(ks_data) <- c("cutoff",'KS')
ks_data[which.max(ks_data$KS),]

#0.17 for max ks
#We can pick 0.17 as the cutoff (event rate is 16.8%)

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package 
require(dplyr)
library(dplyr)

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

#Getting ready to plot ks or gain charts
attr_decile <- data.frame(lift(actual_attr, test_pred, groups = 10))
attr_decile$Gain <- round(attr_decile$Gain)
attr_decile$Gain_p <- paste(as.character(attr_decile$Gain),'%')
attr_decile2 <- rbind(data.frame(bucket=0,total=0,totalresp=0,Cumresp=0,Gain=0,Cumlift=0,Gain_p=0),attr_decile)
perfect_ks_gain <- c(0,38,75,100,100,100,100,100,100,100,100)
attr_decile2 <- cbind(perfect_ks_gain,attr_decile2)

#Plotting the model to check performance
ggplot(attr_decile2)+
  geom_line(aes(x=bucket,y=Gain,color='blue'))+
  geom_point(aes(x=bucket,y=Gain))+
  geom_line(aes(x=seq(0,10,1),y=seq(0,100,10),color="red"))+
  geom_text(aes(bucket,Gain,label=Gain_p),hjust=2,vjust=0)+
  geom_line(aes(x=bucket,y=perfect_ks_gain,color="yellow"))+
  scale_color_discrete(name = "Legend", labels = c("Model","Random Model","Perfect Model"))

#Model looks good so choosing cutoff 0.17

             