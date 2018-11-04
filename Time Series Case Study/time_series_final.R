#Libraries
library(tseries)
library(forecast)
library(TTR)
require(dplyr)
library(ggplot2)
library(gridExtra)

##Load the dataset
data <- read.csv("Global Superstore.csv")

#Lets check for issues in the data first
sum(duplicated(data)) #No duplicates
sapply(data, function(x) sum(is.na(x))) #Postal code has 41296 NA's **Not needed**
nrow(data) #51290
summary(data)
head(data)
str(data)
#Dates need to be converted to, well, dates

#Date Convertion
data$Order.Date <- as.Date(data$Order.Date,'%d-%m-%Y')
data$Ship.Date <- as.Date(data$Ship.Date,'%d-%m-%Y')
data$month <- format(data$Order.Date,"%y-%m") #year-month
max(data$month)
min(data$month) # got 4 years data

#Aggregate to monthly segments
group <- data %>% group_by(month,Market,Segment)
agg <- summarise(group, Total_profit = sum(Profit), 
                 Total_Sales = sum(Sales), 
                 Total_Quantity = sum(Quantity))
head(agg)

###Seggregate the buckets
levels(agg$Market) #"Africa" "APAC" "Canada" "EMEA" "EU" "LATAM" "US"
levels(agg$Segment) #"Consumer"    "Corporate"   "Home Office"

#Time to look who brings the most dollahs
group2 <- agg %>% group_by(Market,Segment)
agg_profit <- summarise(group2, Total_Profit = sum(Total_profit))
agg_profit <- agg_profit[order(-agg_profit$Total_Profit),]
head(agg_profit)

#Time to check who is more consistent using covariance (sd/mean)

for(i in levels(agg$Market)){
  for(j in levels(agg$Segment)){
    cov <- sd(agg[which(agg$Market== i & agg$Segment == j),]$Total_Sales)/
      mean(agg[which(agg$Market== i & agg$Segment == j),]$Total_Sales)
    print(paste(i,"-",j,":",cov))
    agg_profit[which(agg_profit$Market== i & agg_profit$Segment == j),'Covariance'] <- cov
  }
}
agg_profit[order(-agg_profit$Total_Profit),] #Lets look at the data

#Market Segment   Total_Profit
#<fct>  <fct>            <dbl>
#1 APAC   Consumer       222818. #0.456 1st choice
#2 EU     Consumer       188688. #0.488 2nd choice
#3 US     Consumer       134119.
#4 APAC   Corporate      129737.
#5 EU     Corporate      123394.
#6 LATAM  Consumer       120633.

#EDA
str(agg)
ggplot(agg,aes(x=Segment,y=Total_profit,fill=Market)) + geom_bar(stat = "identity")
ggplot(agg,aes(x=Segment,y=Total_Sales,fill=Market)) + geom_bar(stat = "identity")
ggplot(agg,aes(x=Segment,y=Total_Quantity,fill=Market)) + geom_bar(stat = "identity")
#Consumers are the biggest part of the business

############################################
##############APAC Consumer#################
############################################
APAC <- agg[which(agg$Market == "APAC" & agg$Segment == "Consumer"),]
nrow(APAC) #48
APAC$month <- seq(1:48)

#Lets train with 40 months of data and 8 months for test
plot(ts(APAC[,c("Total_Sales")]))
train_apac <- APAC[1:42,]
tail(train_apac)
test_apac <- APAC[43:48,]

#smoothing function
detach("package:dplyr") #dplyr causes issues with filter
smooth_func <- function(ts){
  w <-1
  smoothedseries <- filter(ts, 
                           filter=rep(1/(2*w+1),(2*w+1)), 
                           method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(ts)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  return(smoothedseries)
}

##################SALES##################

train_apac_ts_sales <- ts(train_apac$Total_Sales)

plot(decompose(ts(APAC$Total_Sales,frequency = 12)))
#Seasonal looks like a sine wave and trend is somewhat linear : additive time series

#Smoothing the series
train_apac_ts_sales <- smooth_func(train_apac_ts_sales)
plot(train_apac_ts_sales)
timevals_in <- train_apac$month

smootheddf_apac_sales <- as.data.frame(cbind(timevals_in, as.vector(train_apac_ts_sales)))
colnames(smootheddf_apac_sales) <- c('month', 'Sales')

#Checking global pattern with linear model
lmfit_apac <- lm(Sales ~ sin(0.5*month) * poly(month,2) 
                 + sin(0.15*month)
                 + cos(0.5*month) * poly(month,2)
                 , data=smootheddf_apac_sales)
months <- train_apac$month
globalpred_apac <- predict(lmfit_apac, month=months)
plot(train_apac_ts_sales)
lines(months, globalpred_apac, col='red', lwd=2)
#Lines seem oversmoothed but it might be useful

#Get local prediction
localpred_apac <- train_apac$Total_Sales - globalpred_apac
plot(localpred_apac, col='red', type = "l")

adf.test(localpred_apac,alternative = "stationary") #0.01
kpss.test(localpred_apac) #0.1 
#Looks stationary

acf(localpred_apac) #inside the band
acf(localpred_apac, type="partial") #inside band

#Arma gonna be sure
armafit_apac_local <- auto.arima(localpred_apac)
tsdiag(armafit_apac_local)
armafit_apac_local
#ARIMA(0,0,0) with zero mean
#sigma^2 estimated as 91371203:  log likelihood=-444.53
#AIC=891.07   AICc=891.17   BIC=892.81

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- test_apac$month

global_pred_out_apac <- predict(lmfit_apac,data.frame(month=timevals_out))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(globalpred_apac),ts(global_pred_out_apac))
plot(ts(APAC$Total_Sales), col = "black")
lines(class_dec_pred, col = "red")

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out_apac,test_apac$Total_Sales)[5]
MAPE_class_dec # 35.66734

#Classical decomposition is able to pickup the seasonality and trend but
#not completely, only partially right. Mape shown.

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts(as.vector(train_apac_ts_sales),frequency = 12),D=1)
autoarima
#ARIMA(1,0,0)(1,1,0)[12] with drift 
#Coefficients:
#  ar1     sar1     drift
#0.5569  -0.5294  674.1800
#s.e.  0.1485   0.1647  107.7582
#sigma^2 estimated as 20539851:  log likelihood=-295.71
#AIC=599.43   AICc=601.03   BIC=605.03

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- as.vector(train_apac_ts_sales) - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary") #0.02185
kpss.test(resi_auto_arima) #0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(autoarima, h = 6)
plot(fcast_auto_arima)

forecast_values <- data.frame(fcast_auto_arima)$Point.Forecast
#Mape it

MAPE_auto_arima <- accuracy(forecast_values,test_apac$Total_Sales)[5]
MAPE_auto_arima # 24.90061

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(forecast_values))
plot(ts(APAC[,"Total_Sales"]), col = "black")
lines(auto_arima_pred, col = "red")
#AUTO ARIMA is much better than classical decomposition
#Was able to predict the upward and downward movement

###Lets forecast the future 6 months yet to come
apac_ts <- ts(APAC$Total_Sales,frequency = 12)
arima <- auto.arima(apac_ts,D=1)
arima
forecast_arima <- forecast(arima,h=6)
plot(forecast_arima)

#Lets try one forecast with smoothened values
apac_smooth_ts <- ts(smooth_func(ts(APAC$Total_Sales)),frequency = 12)
arima_smooth <- auto.arima(apac_smooth_ts,D=1)
arima_smooth
forecast_arima_smooth <- forecast(arima_smooth,h=6)
plot(forecast_arima_smooth)
#Visual instincts do seem to reflect in these ARIMA series
#Business can decide

##################QUANTITY##################

train_apac_ts_quantity <- ts(train_apac$Total_Quantity)
plot(train_apac_ts_quantity)
plot(decompose(ts(train_apac$Total_Quantity,frequency = 12)))
#Seasonal looks like a sine wave and trend is somewhat linear : Additive

#Smoothing the series
train_apac_ts_quantity <- smooth_func(train_apac_ts_quantity)
plot(train_apac_ts_quantity)
timevals_in <- train_apac$month

smootheddf_apac_quantity <- as.data.frame(cbind(timevals_in, as.vector(train_apac_ts_quantity)))
colnames(smootheddf_apac_quantity) <- c('month', 'Quantity')

#Checking global pattern with linear model
lmfit_apac <- lm(Quantity ~ sin(0.5*month) * poly(month,2) 
                 + sin(0.15*month)
                 + cos(0.5*month) * poly(month,2)
                 , data=smootheddf_apac_quantity)
months <- train_apac$month
globalpred_apac <- predict(lmfit_apac, month=months)
plot(train_apac_ts_quantity)
lines(months, globalpred_apac, col='red', lwd=2)

#Get local prediction
localpred_apac <- train_apac$Total_Quantity - globalpred_apac
plot(localpred_apac, col='red', type = "l")

adf.test(localpred_apac,alternative = "stationary") #0.01
kpss.test(localpred_apac) #0.1 
#Looks stationary

acf(localpred_apac) #inside the band
acf(localpred_apac, type="partial") #inside band

#Arma gonna be sure
armafit_apac_local <- auto.arima(localpred_apac)
tsdiag(armafit_apac_local)
armafit_apac_local
#ARIMA(0,0,0) with zero mean

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- test_apac$month

global_pred_out_apac <- predict(lmfit_apac,data.frame(month=timevals_out))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(globalpred_apac),ts(global_pred_out_apac))
plot(ts(APAC$Total_Quantity), col = "black")
lines(class_dec_pred, col = "red")
#Classical decomposition was able to predict the upward movement

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out_apac,test_apac$Total_Quantity)[5]
MAPE_class_dec # 33.86242

###########################################

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts(as.vector(train_apac_ts_quantity),frequency = 12),D=1)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- as.vector(train_apac_ts_quantity) - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary") #0.04133
kpss.test(resi_auto_arima) #0.1
acf(resi_auto_arima)
acf(resi_auto_arima,type = "partial")

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(autoarima, h = 6)
plot(fcast_auto_arima)

forecast_values <- data.frame(fcast_auto_arima)$Point.Forecast
#Mape it

MAPE_auto_arima <- accuracy(forecast_values,test_apac$Total_Quantity)[5]
MAPE_auto_arima #25.78485

#Arima seems to be better than classical decomposition

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(forecast_values))
plot(ts(APAC[,"Total_Quantity"]), col = "black")
lines(auto_arima_pred, col = "red")
#MAPE lower than classical decomp.

###Lets forecast the future 6 months yet to come
apac_ts <- ts(APAC$Total_Quantity,frequency = 12)
arima <- auto.arima(apac_ts,D=1)
arima
#ARIMA(0,0,0)(1,1,0)[12] with drift 
#Coefficients:
#  sar1   drift
#-0.6050  8.3406
#s.e.   0.1424  0.9583
#sigma^2 estimated as 9720:  log likelihood=-218.06
#AIC=442.12   AICc=442.87   BIC=446.87

forecast_arima <- forecast(arima,h=6)
plot(forecast_arima) #Looks good

#Lets try one forecast with smoothened values
apac_smooth_ts <- ts(smooth_func(ts(APAC$Total_Quantity)),frequency = 12)
arima_smooth <- auto.arima(apac_smooth_ts,D=1)
arima_smooth
forecast_arima_smooth <- forecast(arima_smooth,h=6)
plot(forecast_arima_smooth)
#Visual instincts do seem to reflect in these ARIMA series

############################################
###############EU Consumer##################
############################################
EU <- agg[which(agg$Market == "EU" & agg$Segment == "Consumer"),]
nrow(EU) #48
EU$month <- seq(1:48)

#Lets train with 40 months of data and 8 months for test
plot(ts(EU[,c("Total_Sales")]))
train_eu <- EU[1:42,]
tail(train_eu)
test_eu <- EU[43:48,]

##################SALES##################

train_eu_ts_sales <- ts(train_eu$Total_Sales)

plot(decompose(ts(train_eu$Total_Sales,frequency = 12)))
#Seasonal looks like a sine wave and trend is somewhat linear : Additive

#Smoothing the series
train_eu_ts_sales <- smooth_func(train_eu_ts_sales)
plot(train_eu_ts_sales)
timevals_in <- train_eu$month

smootheddf_eu_sales <- as.data.frame(cbind(timevals_in, as.vector(train_eu_ts_sales)))
colnames(smootheddf_eu_sales) <- c('month', 'Sales')

#Checking global pattern with linear model
lmfit_eu <- lm(Sales ~ sin(0.5*month) * poly(month,3) 
               + poly(month*3)
               + cos(0.5*month) * poly(month,3)
               , data=smootheddf_eu_sales)
months <- train_eu$month
globalpred_eu <- predict(lmfit_eu, month=months)
plot(train_eu_ts_sales)
lines(months, globalpred_eu, col='red', lwd=2)

#Get local prediction
localpred_eu <- train_eu$Total_Sales - globalpred_eu
plot(localpred_eu, col='red', type = "l")

adf.test(localpred_eu,alternative = "stationary") #0.01
kpss.test(localpred_eu) #0.1 
#Looks stationary

acf(localpred_eu) #inside the band
acf(localpred_eu, type="partial") #inside band

#Arma gonna be sure
armafit_eu_local <- auto.arima(localpred_eu)
tsdiag(armafit_eu_local)
armafit_eu_local
#ARIMA(0,0,0) with zero mean

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- test_eu$month

global_pred_out_eu <- predict(lmfit_eu,data.frame(month=timevals_out))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(globalpred_eu),ts(global_pred_out_eu))
plot(ts(EU$Total_Sales), col = "black")
lines(class_dec_pred, col = "red")

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out_eu,test_eu$Total_Sales)[5]
MAPE_class_dec # 92.95788 Pretty high MAPE

#Classical decomposition is able to predict the huge sale  but
#not able to predict the pullback from such a huge spike

###########################################

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts(as.vector(train_eu_ts_sales),frequency = 12),D=1)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- as.vector(train_eu_ts_sales) - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary") #0.023
kpss.test(resi_auto_arima) #0.1

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(autoarima, h = 6)
plot(fcast_auto_arima)

forecast_values <- data.frame(fcast_auto_arima)$Point.Forecast
#Mape it

MAPE_auto_arima <- accuracy(forecast_values,test_eu$Total_Sales)[5]
MAPE_auto_arima #28.38917 #Much better than classical decompo

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(forecast_values))
plot(ts(EU[,"Total_Sales"]), col = "black")
lines(auto_arima_pred, col = "red")
#AUTO ARIMA is much better than classical decomposition
#Was able to predict the upward and downward movement

###Lets forecast the future 6 months yet to come
eu_ts <- ts(EU$Total_Sales,frequency = 12)
arima <- auto.arima(eu_ts,D=1)
arima
forecast_arima <- forecast(arima,h=6)
plot(forecast_arima)

#Lets try one forecast with smoothened values
eu_smooth_ts <- ts(smooth_func(ts(EU$Total_Sales)),frequency = 12)
arima_smooth <- auto.arima(eu_smooth_ts,D=1)
arima_smooth
forecast_arima_smooth <- forecast(arima_smooth,h=6)
plot(forecast_arima_smooth)
#Visual instincts do seem to reflect in these ARIMA series
#ARIMA seems like a better solution

##################Quanity##################

train_eu_ts_quantity <- ts(train_eu$Total_Quantity)

plot(decompose(ts(train_eu$Total_Quantity,frequency = 12)))
#Seasonal looks like a sine wave and trend is somewhat linear :  Additive

#Smoothing the series
train_eu_ts_quantity <- smooth_func(train_eu_ts_quantity)
plot(train_eu_ts_quantity)
timevals_in <- train_eu$month

smootheddf_eu_quantity <- as.data.frame(cbind(timevals_in, as.vector(train_eu_ts_quantity)))
colnames(smootheddf_eu_quantity) <- c('month', 'Quantity')

#Checking global pattern with linear model
lmfit_eu <- lm(Quantity ~ sin(0.6*month) * poly(month,3) 
               + poly(month,2)
               + cos(0.6*month) * poly(month,3)
               , data=smootheddf_eu_quantity)
months <- train_eu$month
globalpred_eu <- predict(lmfit_eu, month=months)
plot(train_eu_ts_quantity)
lines(months, globalpred_eu, col='red', lwd=2)

#Get local prediction
localpred_eu <- train_eu$Total_Quantity - globalpred_eu
plot(localpred_eu, col='red', type = "l")

adf.test(localpred_eu,alternative = "stationary") #0.01
kpss.test(localpred_eu) #0.1 
#Looks stationary

acf(localpred_eu) #inside the band
acf(localpred_eu, type="partial") #inside band

#Arma gonna be sure
armafit_eu_local <- auto.arima(localpred_eu)
tsdiag(armafit_eu_local)
armafit_eu_local
#ARIMA(2,0,1) with zero mean : Some AR ness left

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- test_eu$month

global_pred_out_eu <- predict(lmfit_eu,data.frame(month=timevals_out))

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(globalpred_eu),ts(global_pred_out_eu))
plot(ts(EU$Total_Quantity), col = "black")
lines(class_dec_pred, col = "red")

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out_eu,test_eu$Total_Quantity)[5]
MAPE_class_dec # 24.97678

#Classical decomposition is forecasting ok values

###########################################

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(ts(as.vector(train_eu_ts_quantity),frequency = 12),D=1)
autoarima
#ARIMA(0,1,0)(1,1,0)[12] 
#Coefficients:
#  sar1
#-0.4410
#s.e.   0.1619
#sigma^2 estimated as 1744:  log likelihood=-150.17
#AIC=304.33   AICc=304.79   BIC=307.06

tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- as.vector(train_eu_ts_quantity) - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary") #0.02186
kpss.test(resi_auto_arima) #0.1 Looks stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- forecast(autoarima, h = 6)
plot(fcast_auto_arima)

forecast_values <- data.frame(fcast_auto_arima)$Point.Forecast
#Mape it

MAPE_auto_arima <- accuracy(forecast_values,test_eu$Total_Quantity)[5]
MAPE_auto_arima #29.56798 #Mape is lower un classical decomposition

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(forecast_values))
plot(ts(EU[,"Total_Quantity"]), col = "black")
lines(auto_arima_pred, col = "red")
#AUTO ARIMA MAPE is higher however the sudden spike might be a coincidence
#Business should decide

###Lets forecast the future 6 months yet to come
eu_ts <- ts(EU$Total_Quantity,frequency = 12)
arima <- auto.arima(eu_ts,D=1)
arima
forecast_arima <- forecast(arima,h=6)
plot(forecast_arima)

#Lets try one forecast with smoothened values
eu_smooth_ts <- ts(smooth_func(ts(EU$Total_Quantity)),frequency = 12)
arima_smooth <- auto.arima(eu_smooth_ts,D=1)
arima_smooth
forecast_arima_smooth <- forecast(arima_smooth,h=6)
plot(forecast_arima_smooth)
#Visual instincts do seem to reflect in these ARIMA series
