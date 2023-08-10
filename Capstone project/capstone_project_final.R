##################################################################################################################
######################################### Capstone Project - Ecommerce ###########################################
##################################################################################################################

#### Business Objective:

# ElecKart is an e-commerce firm specialising in electronic products. Over the last one year,
# they had spent a significant amount of money in marketing. Occasionally, 
# they had also offered big-ticket promotions (similar to the Big Billion Day). 
# They are about to create a marketing budget for the next year which includes spending on commercials, online campaigns, 
# and pricing & promotion strategies. The CFO feels that the money spent over last 12 months on marketing was not sufficiently impactful, 
# and, that they can either cut on the budget or reallocate it optimally across marketing levers to improve the revenue response.
# As part of the marketing team, we will develop a market mix model to observe the actual impact of different marketing variables over the last year. 
# Using our understanding of the model, we will recommend the optimal budget allocation for different marketing levers for the next year.


### Libraries used

library(dplyr)
library(readxl)
library(lubridate)
library(ggplot2)
library(cowplot)
library(MASS)
library(tidyr)
library(caTools)
library(car)
library(DAAG)
library(DataCombine)
library(caret)

### Reading the files

cons_elec<-read.csv("ConsumerElectronics.csv",header = T,stringsAsFactors = F)
media_investment <- read_excel("Media data and other information.xlsx", sheet = "Media Investment", skip = 2)
nps <- read_excel("Media data and other information.xlsx", sheet = "Monthly NPS Score", skip =1, col_names = F)

######################################################################################################
##################################### Data Cleaning & Data Preparation ###############################
######################################################################################################

### Converting given datasets into daily and weekly level ###
day <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=day, Month = month(day),Year = year(day),weekday=weekdays(day))

#Media Investment dataset
##########################

#changing NA values as 0
media_investment[is.na(media_investment)] <- 0

#converting the dataset into daily level
media_investment_week<-arrange(merge(media_investment,weekdays,by=c('Year','Month'),all.x = TRUE),days)

#converting media investments into daily granular level
media_investment_week$`Total Investment`<-media_investment_week$`Total Investment`/366
media_investment_week$TV<-media_investment_week$TV/366
media_investment_week$Digital<-media_investment_week$Digital/366
media_investment_week$Sponsorship<-media_investment_week$Sponsorship/366
media_investment_week$`Content Marketing`<-media_investment_week$`Content Marketing`/366
media_investment_week$`Online marketing`<-media_investment_week$`Online marketing`/366
media_investment_week$Affiliates<-media_investment_week$Affiliates/366
media_investment_week$SEM<-media_investment_week$SEM/366
media_investment_week$Radio<-media_investment_week$Radio/366
media_investment_week$Other<-media_investment_week$Other/366

#converting the investment into crore scale
media_investment_week[,3:12]<-media_investment_week[,3:12]*10000000


#NPS Dataset
##############
#removing 1st column
nps<-nps[,-1]

#transposing nps dataset
nps<-data.frame(t(nps))
rownames(nps)<-1:nrow(nps)

#converting 1st column into special_days column
nps$X1<-as.character(nps$X1)
nps$X2<-as.numeric(as.character(nps$X2))
nps[nps$X1=="Sept'15",1]<-"Sep'15"
nps$X1<-paste("01",replace(nps$X1,"'","-")[1:12],sep = "-")
nps$X1<-as.Date(nps$X1,format="%d-%B'%y")

#Deriving month and year from special_days column
nps$Year<-year(nps$X1)
nps$Month<-month(nps$X1)

#Naming the column
colnames(nps)[2]<-"NPS_Score"

#Creating NPS dataset into daily level
nps_week<-arrange(merge(nps[,-1],weekdays,by=c('Year','Month'),all.x = TRUE),days)

#converting NPS score into daily granular level
nps_week$NPS_Score<-nps_week$NPS_Score/366


#Consumer Electronics Dataset
################################

#Data should be considered from July 2015 to June 2016
cons_elec<-filter(cons_elec,!(Month ==5 & Year==2015|Month ==6 & Year==2015|Month ==7 & Year==2016))

#converting order_date into date format
cons_elec$order_date<-as.Date(cons_elec$order_date,format = "%Y-%m-%d %H:%M:%S")

#Creating a week column
cons_elec$week_number<-as.numeric(strftime(cons_elec$order_date, format = "%V"))

# Jan 2016 week should be 1 instead of 53
cons_elec$week_number[cons_elec$Year==2016 & cons_elec$Month==1 & cons_elec$week_number==53]<- 1


#Merging the datasets into one masterdataset
cons_elec_final<-merge(cons_elec,unique(media_investment_week[,-c(1,2)]),by.x="order_date",by.y="days",all.x = TRUE)

cons_elec_final<-arrange(merge(cons_elec_final,nps_week[,-c(1,2,5)],by.x="order_date",by.y="days",all.x = TRUE),order_date)


#Aggregating the fact columns to weekly levels
weekly_levels<-summarise(group_by(cons_elec_final,week_number),Total_Investment_weekly=sum(`Total Investment`),
                         TV_weekly=sum(TV),
                         Digital_weekly=sum(Digital),
                         Sponsorship_weekly=sum(Sponsorship),
                         Content_Marketing_weekly=sum(`Content Marketing`),
                         Online_marketing_weekly=sum(`Online marketing`),
                         Affiliates_weekly=sum(Affiliates),
                         SEM_weekly=sum(SEM),
                         Radio_weekly=sum(Radio),
                         Other_weekly=sum(Other),
                         NPS_Score_weekly=sum(NPS_Score))

# Adding weekly level aggregates to main dataset
cons_elec_final_weekly<-merge(cons_elec_final,weekly_levels,by="week_number",all.x = TRUE)


### Data Cleaning ###
#Changing the column names for ease of use
colnames(cons_elec_final_weekly)<-c("week_number","order_date","fsn_id","Year","Month","order_id" , "order_item_id", "gmv","units" ,"deliverybdays","deliverycdays","order_payment_type","sla", 
                                    "cust_id","pincode","product_super_category","product_category","product_sub_category","product_vertical","product_mrp","product_procurement_sla",
                                    "Total_Investment_daily","TV_daily","Digital_daily","Sponsorship_daily","Content_Marketing_daily","Online_marketing_daily", "Affiliates_daily" ,"SEM_daily","Radio_daily","Other_daily","weekday","NPS_Score_daily",
                                    "Total_Investment_weekly","TV_weekly","Digital_weekly","Sponsorship_weekly","Content_Marketing_weekly","Online_marketing_weekly","Affiliates_weekly",              
                                    "SEM_weekly","Radio_weekly", "Other_weekly","NPS_Score_weekly")


#Checking for NA values
sapply(cons_elec_final_weekly, function(x) sum(is.na(x)))

#As we can see there are 4904 NA's for cust_id, pincode and gmv
#Removing the rows with NA's
cons_elec_final_weekly<-cons_elec_final_weekly[- which(is.na(cons_elec_final_weekly$cust_id|cons_elec_final_weekly$pincode|cons_elec_final_weekly$gmv)),]


#checking for duplicates
#with the combination of order_id and order_item_id there are 139075 duplicates
sum(duplicated(cons_elec_final_weekly[,c(6,7)]))

#removing the duplicates
cons_elec_final_weekly <- cons_elec_final_weekly[!duplicated(cons_elec_final_weekly[c(6,7)]),]

# removing unimportant columns which are not required for analysis ( fsn_id ,order_id ,order_item_id, cust_id,pincode)
cons_elec_final_weekly<-cons_elec_final_weekly[,-c(3,6,7,14,15)]

## deliverybdays and deliverycdays - we can see that lot of values are "\\N" and few negative values
unique(cons_elec_final_weekly$deliverybdays)
unique(cons_elec_final_weekly$deliverycdays)

count1<-group_by(cons_elec_final_weekly,deliverycdays)%>% count(deliverycdays)
count2<-group_by(cons_elec_final_weekly,deliverybdays)%>% count(deliverybdays)

head(arrange(count1,desc(n))) #"\\N"         1177091
head(arrange(count2,desc(n))) #"\\N"         1177092

# transforming //N and negative values in deliverycdays and deliveryddays to 0
cons_elec_final_weekly$deliverybdays[cons_elec_final_weekly$deliverybdays == "\\N" | cons_elec_final_weekly$deliverybdays < 0] <- 0
cons_elec_final_weekly$deliverycdays[cons_elec_final_weekly$deliverycdays == "\\N" | cons_elec_final_weekly$deliverycdays < 0] <- 0

cons_elec_final_weekly$deliverybdays<-as.numeric(cons_elec_final_weekly$deliverybdays)
cons_elec_final_weekly$deliverycdays<-as.numeric(cons_elec_final_weekly$deliverycdays)

# Checking product_mrp to have values 0 or less than 0
length(cons_elec_final_weekly$product_mrp[cons_elec_final_weekly$product_mrp<=0]) #4620

# removing the rows with product mrp as 0
cons_elec_final_weekly<-filter(cons_elec_final_weekly,product_mrp>0)

#removing the rows with gmv equal to 0 or less than 0
cons_elec_final_weekly<-filter(cons_elec_final_weekly,gmv>0)

# Checking for outliers in different columns
#99% of the values are >=2 , therefore capping the values to a maximum of orders to 2
quantile(cons_elec_final_weekly$units,seq(0,1,0.01))
boxplot(cons_elec_final_weekly$units)
cons_elec_final_weekly$units[cons_elec_final_weekly$units>2]<-2

#99% of the values are >=8 , therefore capping the values to a maximum of days 8
quantile(as.numeric(cons_elec_final_weekly$deliverybdays),seq(0,1,0.01))
boxplot(as.numeric(cons_elec_final_weekly$deliverybdays))
cons_elec_final_weekly$deliverybdays[cons_elec_final_weekly$deliverybdays>8]<-8

#99% of the values are >=10 , therefore capping the values to a maximum of days 10
quantile(as.numeric(cons_elec_final_weekly$deliverycdays),seq(0,1,0.01))
boxplot(as.numeric(cons_elec_final_weekly$deliverycdays))
cons_elec_final_weekly$deliverycdays[cons_elec_final_weekly$deliverycdays>10]<-10

#99% of the values are >=13 , therefore capping the values to a maximum of days 13
quantile(as.numeric(cons_elec_final_weekly$sla),seq(0,1,0.01))
boxplot(as.numeric(cons_elec_final_weekly$sla))
cons_elec_final_weekly$sla[cons_elec_final_weekly$sla>13]<-13

#99% of the values are >=13 , therefore capping the values to a maximum of days 13
#Also, there are negative values in product_procurement_sla , making them to 0
quantile(as.numeric(cons_elec_final_weekly$product_procurement_sla),seq(0,1,0.01))
boxplot(as.numeric(cons_elec_final_weekly$product_procurement_sla))
cons_elec_final_weekly$product_procurement_sla[cons_elec_final_weekly$product_procurement_sla>13]<-13
cons_elec_final_weekly$product_procurement_sla[cons_elec_final_weekly$product_procurement_sla<0]<-0


# As we have data from July-2015 to June-2016, So we're considering June-105 as our base for week calculation/number
# i.e 1st week of July-2015 as 1 (instead of 27), 2nd week of July-2015 as 2 (instead of 28) and so on till June-2016
# Also, for Jan-2016 we'll consider the subsequent week number i.e week number after Dec-2015 last week ,instead as 1st week
cons_elec_final_weekly$week_number <- ifelse(cons_elec_final_weekly$week_number>=27, cons_elec_final_weekly$week_number-26, cons_elec_final_weekly$week_number+27)


###Deriving new columns
########################

# Adding Special Sale Calender from the Media data and other information dataset to the master df
special_days <- as.Date(c("2015-07-18","2015-07-19","2015-08-15","2015-08-16","2015-08-17","2015-08-28","2015-08-29","2015-08-30","2015-10-15","2015-10-16","2015-10-17",
                          "2015-11-07","2015-11-08","2015-11-09","2015-11-10","2015-11-11","2015-11-12","2015-11-13","2015-11-14","2015-12-25","2015-12-26","2015-12-27",
                          "2015-12-28","2015-12-29","2015-12-30","2015-12-31","2016-01-01","2016-01-02","2016-01-03","2016-01-20","2016-01-21","2016-01-22","2016-02-01",
                          "2016-02-02","2016-02-14","2016-02-15","2016-02-20","2016-02-21","2016-03-07","2016-03-08","2016-03-09","2016-05-25","2016-05-26","2016-05-27"))

cons_elec_final_weekly$special_sale_calendar<-ifelse(cons_elec_final_weekly$order_date %in% special_days,"Special Sale","No Sale")

## Creating one more column "special_sale_calendar_day" which stores which special day it was (like Diwali, Eid etc.)
cons_elec_final_weekly$special_sale_calendar_day='Regular Day'

cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[1:2])]='Eid & Rathayatra'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[3:5])]='Independence Day'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[6:8])]='Rakshabandhan'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[9:11])]='Daussera'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[12:19])]='Diwali'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[20:29])]='Christmas & New Year'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[30:32])]='Republic Day'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[33:34])]='BED'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[35:36])]='Valentine Day'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[37:38])]='FHSD'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[39:41])]='BSD'
cons_elec_final_weekly$special_sale_calendar_day[cons_elec_final_weekly$order_date  %in% (special_days[42:44])]='Pacman'

## Creating a column for list price i.e. gmv/units sold
cons_elec_final_weekly$list_price<-cons_elec_final_weekly$gmv/cons_elec_final_weekly$units

## Creating a column for Product pricing which defines if a product is Premium, Mass or Medium Priced
cons_elec_final_weekly$product_pricing<-case_when(cons_elec_final_weekly$list_price<=1000 ~ "Mass",
                                                  cons_elec_final_weekly$list_price>1000 & cons_elec_final_weekly$list_price<=99999 ~ "Medium Priced",
                                                  cons_elec_final_weekly$list_price>10000 ~ "Premium")

## Creating a column for discount/promotional offer
cons_elec_final_weekly$discount<-(cons_elec_final_weekly$product_mrp-cons_elec_final_weekly$list_price)/cons_elec_final_weekly$product_mrp*100

# Creating the adstock for each media investment
# Keeping the adstock rate at 0.5
adstock_rate=0.50
df <- data.frame(week_number=1:53)
investment_weekly<-unique(cons_elec_final_weekly[,c(1,29:37)])

for(i in 2:10){
  df[[paste0(colnames(investment_weekly)[i],"_adstock")]] <- as.numeric(stats::filter(x=investment_weekly[i],filter=adstock_rate, method="recursive"))
}

# Merging the adstock with the actual dataset
cons_elec_final_weekly <- merge(cons_elec_final_weekly, df, by = c("week_number"), all.x = TRUE)


### Dividing the data for the product sub-categories- camera accessory, home audio and gaming accessory. 
elec_CameraAccessory <- subset(cons_elec_final_weekly, product_sub_category == "CameraAccessory")
elec_HomeAudio <- subset(cons_elec_final_weekly, product_sub_category == "HomeAudio")
elec_GamingAccessory <- subset(cons_elec_final_weekly, product_sub_category == "GamingAccessory")


######################################################################################################
##################################### Exploratory Data Analysis ######################################
######################################################################################################

###-- No. of orders on Special and Non Sepcial days

orders<-group_by(cons_elec_final_weekly,special_sale_calendar)%>% summarise(no_orders=length(NPS_Score_weekly),revenue=sum(gmv))

plot_grid(ggplot(orders,aes(x=special_sale_calendar,y=no_orders))+geom_col()+labs(title="Orders on Speacial & Non Special days"),
          ggplot(orders,aes(x=special_sale_calendar,y=revenue))+geom_col()+labs(title="Revenue on Speacial & Non Special days"),align = "v")

### -- Media Investment accross the months

media_investment_long <- gather(media_investment, Medium, Spend, 3:12)

ggplot(filter(media_investment_long,Medium!="Total Investment"), aes (x = Month, y = Spend, colour = Medium)) + geom_line() +
  scale_x_discrete(name="Months starting June 2015", limits=seq(1,12,1))


### -- Revenue obtained

# GMV (Gross revenue) on special and non special calendar sale
# As we can see for the three categories the revenue has higest peaks when there are special season sales

gmv_CA<-group_by(elec_CameraAccessory,order_date,special_sale_calendar)%>% summarise(gmv=sum(gmv))
gmv_HA<-group_by(elec_HomeAudio,order_date,special_sale_calendar)%>% summarise(gmv=sum(gmv))
gmv_GA<-group_by(elec_GamingAccessory,order_date,special_sale_calendar)%>% summarise(gmv=sum(gmv))

ggplot(gmv_CA,aes(x=order_date,y=gmv,fill=special_sale_calendar))+geom_area(aes(alpha=0.6))+
  labs(title="Gross Revenue for Camera accessory",y="Gross Merchendise Value",x="Dates")+ theme(legend.position = c(0.5, 0.8))
ggplot(gmv_HA,aes(x=order_date,y=gmv,fill=special_sale_calendar))+geom_area(aes(alpha=0.6))+
  labs(title="Gross Revenue for Home Audio",y="Gross Merchendise Value",x="Dates")+ theme(legend.position = c(0.5, 0.8))
ggplot(gmv_GA,aes(x=order_date,y=gmv,fill=special_sale_calendar))+geom_area(aes(alpha=0.6))+
  labs(title="Gross Revenue for Gaming accessory",y="Gross Merchendise Value",x="Dates")+ theme(legend.position = c(0.5, 0.8))

### -- Media Investment

# Checking Total media investments on regular and on sepcial calender sale on weekly level
# We can see that during the week 16 , there is higher total medial investment accross three categories and that is during dussera season

ggplot(elec_CameraAccessory,aes(x=week_number,y=Total_Investment_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Total Media Investment for Camera accessory",y="Total Investment Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Total_Investment_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Total Media Investment for Home Audio",y="Total Investment Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Total_Investment_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Total Media Investment for Gaming accessory",y="Total Investment Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))


# Checking TV investments on regular and on special calender sales on weekly level
# We can see that during the week 16 & 37, there is higher TV medial investment accross three categories and that is during dussera & BSD season

ggplot(elec_CameraAccessory,aes(x=week_number,y=TV_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="TV Media Investment for Camera accessory",y="TV Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=TV_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="TV Media Investment for Home Audio",y="TV Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=TV_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="TV Media Investment for Gaming accessory",y="TV Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))


# Checking Digital investments on regular and on special calender sales on weekly level
# We can see that during the week 16 , there is higher total Digital investment accross three categories and that is during dussera season

ggplot(elec_CameraAccessory,aes(x=week_number,y=Digital_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Digital Media Investment for Camera accessory",y="Digital Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Digital_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Digital Media Investment for Home Audio",y="Digital Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Digital_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Digital Media Investment for Gaming accessory",y="Digital Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))


# Checking Sponsorship investments on regular and on special calender sales on weekly level
# We can see that during the week 16 , there is higher total Sponsorship investment accross three categories and that is during dussera season

ggplot(elec_CameraAccessory,aes(x=week_number,y=Sponsorship_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Sponsorship Media Investment for Camera accessory",y="Sponsorship Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Sponsorship_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Sponsorship Media Investment for Home Audio",y="Sponsorship Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Sponsorship_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Sponsorship Media Investment for Gaming accessory",y="Sponsorship Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))


# Checking Content_Marketing investments on regular and on special calender sales on weekly level
# We can see that during the week 16 , there is higher total Content_Marketing investment accross three categories and that is during dussera season

ggplot(elec_CameraAccessory,aes(x=week_number,y=Content_Marketing_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Content_Marketing Media Investment for Camera accessory",y="Content_Marketing Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Content_Marketing_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Content_Marketing Media Investment for Home Audio",y="Content_Marketing Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Content_Marketing_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Content_Marketing Media Investment for Gaming accessory",y="Content_Marketing Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))


# Checking Online_marketing investments on regular and on special calender sales on weekly level
# We can see that during the week 16 , there is higher total Online_marketing investment accross three categories and that is during dussera season
# We also see, there are some higher media investments on season weeks when compared with regular weeks

ggplot(elec_CameraAccessory,aes(x=week_number,y=Online_marketing_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Online_marketing Media Investment for Camera accessory",y="Online_marketing Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Online_marketing_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Online_marketing Media Investment for Home Audio",y="Online_marketing Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Online_marketing_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Online_marketing Media Investment for Gaming accessory",y="Online_marketing Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))


# Checking Affiliates investments on regular and on special calender sales on weekly level
# We can see that during the week 16 , there is higher total Affiliates investment accross three categories and that is during dussera season
# We also see, there are some higher media investments on season weeks when compared with regular weeks

ggplot(elec_CameraAccessory,aes(x=week_number,y=Affiliates_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Affiliates Media Investment for Camera accessory",y="Affiliates Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Affiliates_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Affiliates Media Investment for Home Audio",y="Affiliates Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Affiliates_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Affiliates Media Investment for Gaming accessory",y="Affiliates Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))


# Checking SEM investments on regular and on special calender sales on weekly level
# We can see that during the week 16 , there is higher total SEM investment accross three categories and that is during dussera season

ggplot(elec_CameraAccessory,aes(x=week_number,y=SEM_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="SEM Media Investment for Camera accessory",y="SEM Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=SEM_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="SEM Media Investment for Home Audio",y="SEM Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=SEM_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="SEM Media Investment for Gaming accessory",y="SEM Weekly level",x="Week numbers")+ theme(legend.position = c(0.5, 0.8))



# Checking Radio investments on regular and on special calender sales on weekly level
# Radio Investments are only done on few Special Sale Seasons, such as Republic Day,BED, FHSD.

ggplot(elec_CameraAccessory,aes(x=week_number,y=Radio_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Radio Media Investment for Camera accessory",y="Radio Weekly level",x="Week numbers")+ theme(legend.position = c(0.05, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Radio_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Radio Media Investment for Home Audio",y="Radio Weekly level",x="Week numbers")+ theme(legend.position = c(0.05, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Radio_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Radio Media Investment for Gaming accessory",y="Radio Weekly level",x="Week numbers")+ theme(legend.position = c(0.05, 0.8))


# Checking Other investments on regular and on special calender sales on weekly level
# Other Investments are only done on few Special Sale Seasons, such as Republic Day,BED, FHSD.

ggplot(elec_CameraAccessory,aes(x=week_number,y=Other_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Other Media Investment for Camera accessory",y="Other Weekly level",x="Week numbers")+ theme(legend.position = c(0.05, 0.8))
ggplot(elec_HomeAudio,aes(x=week_number,y=Other_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Other Media Investment for Home Audio",y="Other Weekly level",x="Week numbers")+ theme(legend.position = c(0.05, 0.8))
ggplot(elec_GamingAccessory,aes(x=week_number,y=Other_weekly,fill=special_sale_calendar))+geom_col(position = "dodge")+
  labs(title="Other Media Investment for Gaming accessory",y="Other Weekly level",x="Week numbers")+ theme(legend.position = c(0.05, 0.8))


### -- NPS Score Weekly 

# Checking the correlation betwen NPS score and number of orders placed per week
# As we can see the number of orders are on par with the NPS score per week in the given year
orders_cam<-group_by(elec_CameraAccessory,week_number)%>% summarise(no_orders=length(NPS_Score_weekly),revenue=sum(gmv))
orders_home<-group_by(elec_HomeAudio,week_number)%>% summarise(no_orders=length(NPS_Score_weekly),revenue=sum(gmv))
orders_game<-group_by(elec_GamingAccessory,week_number)%>% summarise(no_orders=length(NPS_Score_weekly),revenue=sum(gmv))

plot_grid(ggplot(elec_CameraAccessory,aes(x=week_number,y=NPS_Score_weekly))+geom_line()+
            labs(title="NPS for Camera accessory",y="NPS Score",x="Week numbers"),
          ggplot(orders_cam,aes(x=week_number,y=no_orders))+geom_line()+labs(title="Number of orders per week",y="No of Orders",x="Week numbers"))

plot_grid(ggplot(elec_HomeAudio,aes(x=week_number,y=NPS_Score_weekly))+geom_line()+
            labs(title="NPS for Home Audio",y="NPS Score",x="Week numbers"),
          ggplot(orders_home,aes(x=week_number,y=no_orders))+geom_line()+labs(title="Number of orders per week",y="No of Orders",x="Week numbers"))

plot_grid(ggplot(elec_GamingAccessory,aes(x=week_number,y=NPS_Score_weekly))+geom_line()+
            labs(title="NPS for Gaming accessory",y="NPS Score",x="Week numbers"),
          ggplot(orders_game,aes(x=week_number,y=no_orders))+geom_line()+labs(title="Number of orders per week",y="No of Orders",x="Week numbers"))


### --No of Orders Monthly ,Weekly and daily

# Category - CameraAccessory
orders_weekly_CA<-group_by(elec_CameraAccessory,week_number)%>% summarise(no_orders=length(units))
ggplot(orders_weekly_CA,aes(x=week_number,y=no_orders))+geom_line()+geom_point(aes(y=orders_weekly_CA$no_orders))+labs(title="Weekly orders on Camera Accessory")

orders_monthly_CA<-group_by(elec_CameraAccessory,Year,Month)%>% summarise(no_orders=length(units))
ggplot(orders_monthly_CA,aes(x=Month,y=no_orders))+geom_line()+geom_point(aes(y=orders_monthly_CA$no_orders))+labs(title="Monthly orders")

orders_daily_CA<-group_by(elec_CameraAccessory,order_date)%>% summarise(no_orders=length(units))
ggplot(orders_daily_CA,aes(x=order_date,y=no_orders))+geom_line()+geom_point(aes(y=orders_daily_CA$no_orders))+labs(title="Daily orders")


# Category - Home Audio
orders_weekly_HA<-group_by(elec_HomeAudio,week_number)%>% summarise(no_orders=length(units))
ggplot(orders_weekly_HA,aes(x=week_number,y=no_orders))+geom_line()+geom_point(aes(y=orders_weekly_HA$no_orders))+labs(title="Weekly orders on Home Audio")

orders_monthly_HA<-group_by(elec_HomeAudio,Year,Month)%>% summarise(no_orders=length(units))
ggplot(orders_monthly_HA,aes(x=Month,y=no_orders))+geom_line()+geom_point(aes(y=orders_monthly_HA$no_orders))+labs(title="Monthly orders")

orders_daily_HA<-group_by(elec_HomeAudio,order_date)%>% summarise(no_orders=length(units))
ggplot(orders_daily_HA,aes(x=order_date,y=no_orders))+geom_line()+geom_point(aes(y=orders_daily_HA$no_orders))+labs(title="Daily orders")

# Category - Gaming Accessory
orders_weekly_GA<-group_by(elec_GamingAccessory,week_number)%>% summarise(no_orders=length(units))
ggplot(orders_weekly_GA,aes(x=week_number,y=no_orders))+geom_line()+geom_point(aes(y=orders_weekly_GA$no_orders))+labs(title="Weekly orders on Gaming Accessory")

orders_monthly_GA<-group_by(elec_GamingAccessory,Year,Month)%>% summarise(no_orders=length(units))
ggplot(orders_monthly_GA,aes(x=Month,y=no_orders))+geom_line()+geom_point(aes(y=orders_monthly_GA$no_orders))+labs(title="Monthly orders")

orders_daily_GA<-group_by(elec_GamingAccessory,order_date)%>% summarise(no_orders=length(units))
ggplot(orders_daily_GA,aes(x=order_date,y=no_orders))+geom_line()+geom_point(aes(y=orders_daily_GA$no_orders))+labs(title="Daily orders")


#####################################################################################
##################################### Modelling #####################################
#####################################################################################

### Creating dummy variables for categorical columns and making the data into one single grain
##############################################################################################
weekly_grain<- function(df){
  
  # creating dummy variables for special sale calendar
  df$special_sale_calendar<- ifelse(df$special_sale_calendar=="Special Sale",1,0)
  
  # Aggregating the pricing variables at product vertical level
  data_price<-unique(df)%>%group_by(product_vertical,week_number)%>%summarise(mrp=mean(product_mrp),list_price=mean(list_price),
                                                                              discount=mean(discount))
  
  # Making all the other facts to one grain (weekly)
  data<-unique(df)%>%group_by(week_number)%>%summarise(gmv=sum(gmv),units = sum(units), deliverybdays = round(mean(deliverybdays)), 
                                                       deliverycdays = round(mean(deliverycdays)),
                                                       sla = round(mean(sla)), product_procurement_sla = round(mean(product_procurement_sla)),
                                                       special_sale_calendar=round(mean(special_sale_calendar)))
  
  ## Removing unwanted columns for modelling
  # removing Year,month and media spends at daily level as the grain is at weekly level
  df<- unique(df[,-c(2:13,15:28,40:44)] )
  
  #Merging the dataset with pricing variables
  df_final<-unique(merge(df,data_price,by=c("product_vertical","week_number"),all.x = T))
  #Merging the weekly grain data with main dataset
  df_final<-unique(merge(df_final,data,by=c("week_number"),all.x = T))
  
  #creating dummy variables for product_vertical
  dummy <- data.frame(model.matrix( ~product_vertical, data = df_final))
  dummy<- dummy[,-1]
  df_final <- cbind(df_final[,-2], dummy)
}

# Calling the weekly grain function on 3 product categories
GamingAccessory_df <- weekly_grain(elec_GamingAccessory)
HomeAudio_df <- weekly_grain(elec_HomeAudio)
CameraAccessory_df <- weekly_grain(elec_CameraAccessory)

# Creating a function to derive elasticity
elas<-function(data_cam,final_cam){
  elasticity <- function(var){
    elasticity <-as.numeric(final_cam$coefficients[var]*mean(data_cam[,var])/mean(data_cam$gmv))
    return(elasticity)
  } 
  var_cam <- list()
  for(i in 2:length(final_cam$coefficients)){
    var_cam[i-1] <-elasticity(names(final_cam$coefficients)[i])
  }
  elasticity_var <- data.frame(names(final_cam$coefficients[2:length(final_cam$coefficients)]))
  elasticity_var <- cbind(elasticity_var,do.call(rbind.data.frame, var_cam))
  colnames(elasticity_var) <- c("Variables","Elasticity")
  elasticity_var$direction <- ifelse(elasticity_var$Elasticity > 0, "Positive", "Negative")
  # View the elasticity of the variables
  print(elasticity_var)
  # plotting the elasticity of the variables
  ggplot(elasticity_var,aes(x=Variables,y=Elasticity,fill=direction))+geom_col()+theme(axis.text.x = element_text(size=8, angle=90))+ggtitle("Elasticity of the Variables")
}


############################################################################################################
##################################### Camera Accessory  Model Building #####################################
############################################################################################################


###### Linear model #########
#############################

# Scaling the variables
CameraAccessory_linear<-CameraAccessory_df
CameraAccessory_linear[,-25]<-scale(CameraAccessory_linear[,-25])

#-------------------------------------------------------------------------------------------

# splitting the data between train and test
set.seed(100)
indices = sample.split(CameraAccessory_linear$gmv, SplitRatio = 0.75)
train_cam_ln = CameraAccessory_linear[indices,]
test_cam_ln = CameraAccessory_linear[!(indices),]

#Initial model with all the dependent and independent variables
model_ln<-lm(gmv~. ,data = train_cam_ln)
summary(model_ln)

#using stepAIC to remove insignificant variables
step_lm <- stepAIC(model_ln, direction="both")
step_lm

#Model 1
linear_cam_1<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + product_verticalTeleconverter, data = train_cam_ln)

summary(linear_cam_1)#0.9761
vif(linear_cam_1)

#Model 2
#removing product_verticalTeleconverter as it has high P value 
linear_cam_2<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar , data = train_cam_ln)

summary(linear_cam_2)#0.976
vif(linear_cam_2)


#Model 3
#removing Content_Marketing_weekly_adstock  column as it has high P value and high vif
linear_cam_3<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar , data = train_cam_ln)

summary(linear_cam_3)#0.9759
vif(linear_cam_3)


#Model 4
#removing special_sale_calendar column as it is highly correlated (high VIF) and high p value
linear_cam_4<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla
                    , data = train_cam_ln)

summary(linear_cam_4)#0.9757
vif(linear_cam_4)


#Model 5
#removing deliverybdays   column as it has high vif value
linear_cam_5<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                   units  + deliverycdays + sla + product_procurement_sla
                 , data = train_cam_ln)

summary(linear_cam_5)#0.9712
vif(linear_cam_5)


#Model 6
#removing SEM_weekly_adstock column as it has high P value
linear_cam_6<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   Online_marketing_weekly_adstock  + Radio_weekly_adstock + 
                   units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_6)#0.9712
vif(linear_cam_6)

#Model 7
#removing Online_marketing_weekly_adstock column as it has high VIF value
linear_cam_7<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   Radio_weekly_adstock + 
                   units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_7)#0.9597
vif(linear_cam_7)

#Model 8
#removing  Radio_weekly_adstock column as it has high VIF value and high P value
linear_cam_8<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_8)#0.9597
vif(linear_cam_8)


#Model 9
#removing Digital_weekly_adstock column as it has high VIF value and high P value
linear_cam_9<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Sponsorship_weekly_adstock  + 
                   units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_9)#0.9596
vif(linear_cam_9)

#Model 10
#removing Other_weekly column as it has high vif value
linear_cam_10<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                    Sponsorship_weekly_adstock  + 
                    units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_10)#0.9586
vif(linear_cam_10)

#Model 11
#removing Online_marketing_weekly column as it has high VIF value and highly correlated
linear_cam_11<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                    Sponsorship_weekly_adstock  + 
                    units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_11)#0.9587
vif(linear_cam_11)

#Model 12
#removing NPS_Score_weekly column as it has high VIF and high P value
linear_cam_12<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    Total_Investment_weekly_adstock + TV_weekly_adstock + 
                    Sponsorship_weekly_adstock  + 
                    units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_12)#0.9433
vif(linear_cam_12)


#Model 13
#removing Radio_weekly column as it has high VIF and highly correlated
linear_cam_13<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + 
                    Total_Investment_weekly_adstock + TV_weekly_adstock + 
                    Sponsorship_weekly_adstock  + 
                    units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_13)#0.9433
vif(linear_cam_13)


#Model 14
#removing Total_Investment_weekly_adstock  column as it has high P value and highly significant
linear_cam_14<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + 
                    TV_weekly_adstock + Sponsorship_weekly_adstock  + 
                    units  + deliverycdays + sla + product_procurement_sla, data = train_cam_ln)

summary(linear_cam_14)#0.9434
vif(linear_cam_14)


#Model 15
#removing sla column as it has high p value
linear_cam_15<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + 
                    TV_weekly_adstock + Sponsorship_weekly_adstock  + 
                    units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_15)#0.9435
vif(linear_cam_15)


#Model 16
#removing Sponsorship_weekly_adstock column as it has high p value
linear_cam_16<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + 
                    TV_weekly_adstock + 
                    units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_16)#0.9428
vif(linear_cam_16)


#Model 17
#removing Affiliates_weekly column as it has high p value
linear_cam_17<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    SEM_weekly + TV_weekly_adstock + 
                    units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_17)#0.9419
vif(linear_cam_17)


#Model 19
#removing Sponsorship_weekly column as it has high vif value
linear_cam_19<-lm(formula = gmv ~ TV_weekly + Digital_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    SEM_weekly + TV_weekly_adstock + 
                    units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_19)#0.9342
vif(linear_cam_19)

#Model 20
#removing Sponsorship_weekly column as it has high vif value
linear_cam_20<-lm(formula = gmv ~ TV_weekly + Digital_weekly + 
                    Content_Marketing_weekly + SEM_weekly + TV_weekly_adstock + 
                    units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_20)#0.934
vif(linear_cam_20)


#Model 21
#removing TV_weekly column as it has high p value
linear_cam_21<-lm(formula = gmv ~ Digital_weekly + 
                    Content_Marketing_weekly + SEM_weekly + TV_weekly_adstock + 
                    units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_21)#0.933
vif(linear_cam_21)

#Model 22
#removing SEM_weekly column as it has high vif
linear_cam_22<-lm(formula = gmv ~ Digital_weekly + 
                    Content_Marketing_weekly + TV_weekly_adstock + 
                    units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_22)#0.8989
vif(linear_cam_22)

#Model 23
#removing TV_weekly_adstock column as it has high p value
linear_cam_23<-lm(formula = gmv ~ Digital_weekly + 
                    Content_Marketing_weekly +units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)

summary(linear_cam_23)#0.8985
vif(linear_cam_23)

#Model 24
#removing Digital_weekly column as it has high vif
linear_cam_24<-lm(formula = gmv ~ Content_Marketing_weekly +units  + deliverycdays +  product_procurement_sla, data = train_cam_ln)
summary(linear_cam_24)#0.888
vif(linear_cam_24)

#FINAL MODEL -> linear_cam_24
# Adjusted R-squared:  0.888
# signicant variables : Content_Marketing_weekly ,units , deliverycdays, product_procurement_sla
#-------------------------------------------------------------------------------------------

# Predict the gmv in the testing dataset
Predict_ln_1 <- predict(linear_cam_24,test_cam_ln[,-25])
test_cam_ln$test_gmv <- Predict_ln_1

# Calculate correlation
r <- cor(test_cam_ln$test_gmv,test_cam_ln$gmv)
# calculate R squared by squaring correlation
rsquared_ln <- cor(test_cam_ln$gmv,test_cam_ln$test_gmv)^2
# check R-squared
rsquared_ln #0.88254

#Scatter plot to show the linearity of the data
plot(test_cam_ln$gmv, test_cam_ln$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="Actual ", ylab="Prediction")

#-------------------------------------------------------------------------------------------
#Cross validation with 5 folds
CV_cam<-cv.lm(data = CameraAccessory_linear, form.lm = linear_cam_24, m=5, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)

#Overall Mean Square Error
attr(CV_cam,"ms") #2.27e+11

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(CameraAccessory_linear,linear_cam_24)



########## Multiplicative model ############ 
############################################

#Converting 0 to 0.001 as log(0) is not defined
CameraAccessory_mul<-CameraAccessory_df
CameraAccessory_mul[CameraAccessory_mul<=0]<-0.001

#converting into logs
CameraAccessory_mul<-log(CameraAccessory_mul)

#-------------------------------------------------------------------------------------------

# splitting the data between train and test
set.seed(100)
indices = sample.split(CameraAccessory_mul$gmv, SplitRatio = 0.75)
train_cam_mul = CameraAccessory_mul[indices,]
test_cam_mul = CameraAccessory_mul[!(indices),]

#Initial model with all the dependent and independent variables
model_mul<-lm(gmv~. ,data = train_cam_mul)
summary(model_mul)

#using stepAIC to remove insignificant variables
step_mul <- stepAIC(model_mul, direction="both")
step_mul


#Model 1
mul_cam_1<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                discount + units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla + special_sale_calendar + product_verticalCameraFilmRolls + 
                product_verticalCameraLEDLight + product_verticalFlashShoeAdapter + 
                product_verticalTeleconverter, data = train_cam_mul)
summary(mul_cam_1)#0.9952
vif(mul_cam_1)


#Model 2
#removing Sponsorship_weekly_adstock as it has high vif and high p value
mul_cam_2<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                discount + units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla + special_sale_calendar + product_verticalCameraFilmRolls + 
                product_verticalCameraLEDLight + product_verticalFlashShoeAdapter + 
                product_verticalTeleconverter, data = train_cam_mul)
summary(mul_cam_2)#0.9952
vif(mul_cam_2)

#Model 3
# removing special_sale_calendar column as it has  high p value
mul_cam_3<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                discount + units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla + product_verticalCameraFilmRolls + 
                product_verticalCameraLEDLight + product_verticalFlashShoeAdapter + 
                product_verticalTeleconverter, data = train_cam_mul)
summary(mul_cam_3)#0.9952
vif(mul_cam_3)

#Model 4
# removing product_verticalFlashShoeAdapter column as it has high p value
mul_cam_4<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                discount + units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla + product_verticalCameraFilmRolls + 
                product_verticalCameraLEDLight +product_verticalTeleconverter, data = train_cam_mul)
summary(mul_cam_4)#0.9952
vif(mul_cam_4)

#Model 5
# removing product_verticalCameraFilmRolls column as it has high p value
mul_cam_5<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                discount + units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla  + 
                product_verticalCameraLEDLight +product_verticalTeleconverter, data = train_cam_mul)
summary(mul_cam_5)#0.9952
vif(mul_cam_5)

#Model 6
#removing product_verticalCameraLEDLight column as it has high p value
mul_cam_6<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                discount + units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla  + 
                product_verticalTeleconverter, data = train_cam_mul)
summary(mul_cam_6)#0.9952
vif(mul_cam_6)

#Model 7
#removing discount column as it has high p value 
mul_cam_7<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla  + 
                product_verticalTeleconverter, data = train_cam_mul)
summary(mul_cam_7)#0.9952
vif(mul_cam_7)

#Model 8 
#removing product_verticalTeleconverter column as it has high p value 
mul_cam_8<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla , data = train_cam_mul)
summary(mul_cam_8)#0.9951
vif(mul_cam_8)

#Model 9
#removing Online_marketing_weekly_adstock column as it has high p value and high vif value
mul_cam_9<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                Content_Marketing_weekly_adstock +SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + 
                product_procurement_sla , data = train_cam_mul)
summary(mul_cam_9)#0.9951
vif(mul_cam_9)

#Model 10
#removing Other_weekly column as it has  high vif value
mul_cam_10<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_10)#0.9943
vif(mul_cam_10)

#Model 11
#removing Content_Marketing_weekly column as it has high p value and high vif value
mul_cam_11<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_11)#0.9943
vif(mul_cam_11)

#Model 12
#removing Radio_weekly_adstock column as it has high p value and high vif value
mul_cam_12<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_12)#0.9943
vif(mul_cam_12)

#model 13
#removing Total_Investment_weekly column as it has high vif value
mul_cam_13<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_13)#0.9941
vif(mul_cam_13)

#model 14
#removing TV_weekly column as it has high p value
mul_cam_14<-lm(formula = gmv ~ week_number + 
                 Digital_weekly + Sponsorship_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_14)#0.9941
vif(mul_cam_14)

#Model 15
#removing Total_Investment_weekly_adstock column as it has high vif
mul_cam_15<-lm(formula = gmv ~ week_number + 
                 Digital_weekly + Sponsorship_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly + TV_weekly_adstock + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_15)#0.9903
vif(mul_cam_15)


#Model 16
#removing Sponsorship_weekly column as it has high vif value
mul_cam_16<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly + TV_weekly_adstock + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_16)#0.9903
vif(mul_cam_16)

#Model 17
#removing TV_weekly_adstock column as it has high vif and p value
mul_cam_17<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Affiliates_weekly + SEM_weekly + Radio_weekly  + 
                 NPS_Score_weekly  + Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_17)#0.9902
vif(mul_cam_17)

#Model 18
#removing SEM_weekly column as it has high vif value
mul_cam_18<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Affiliates_weekly + Radio_weekly  + 
                 NPS_Score_weekly  + Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_18)#0.9863
vif(mul_cam_18)


#Model 19
#removing NPS_Score_weekly column as it has high vif and high p value
mul_cam_19<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Affiliates_weekly + Radio_weekly  + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays + sla + 
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_19)#0.9862
vif(mul_cam_19)

#Model 20
#removing sla column as it has high p value
mul_cam_20<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Affiliates_weekly + Radio_weekly  + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units + deliverybdays + deliverycdays +  
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_20)#0.9862
vif(mul_cam_20)

#Model 21
#removing deliverybdays column as it has high vif
mul_cam_21<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Affiliates_weekly + Radio_weekly  + 
                 Content_Marketing_weekly_adstock +SEM_weekly_adstock  + 
                 units  + deliverycdays +  
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_21)#0.9861
vif(mul_cam_21)


#Model 22
#removing SEM_weekly_adstock column as it has high vif
mul_cam_22<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Affiliates_weekly + Radio_weekly  + 
                 Content_Marketing_weekly_adstock  + 
                 units  + deliverycdays +  
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_22)#0.986
vif(mul_cam_22)


#Model 23
#removing Affiliates_weekly column as it has high vif
mul_cam_23<-lm(formula = gmv ~ week_number + 
                 Digital_weekly  + Radio_weekly  + 
                 Content_Marketing_weekly_adstock  + 
                 units  + deliverycdays +  
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_23)#0.9851
vif(mul_cam_23)

#Model 24
#removing week_number column as it has high p value
mul_cam_24<-lm(formula = gmv ~  Digital_weekly  + Radio_weekly  + 
                 Content_Marketing_weekly_adstock  +  units  + deliverycdays +  
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_24)#0.9851
vif(mul_cam_24)


#Model 25
#removing Digital_weekly column as it has high p value
mul_cam_25<-lm(formula = gmv ~   Radio_weekly  + 
                 Content_Marketing_weekly_adstock  +  units  + deliverycdays +  
                 product_procurement_sla , data = train_cam_mul)
summary(mul_cam_25)#0.9851
vif(mul_cam_25)

#FINAL MODEL -> mul_cam_25
# Adjusted R-squared:  0.9851
# signicant variables : Radio_weekly,Content_Marketing_weekly_adstock ,units, deliverycdays,product_procurement_sla
#-------------------------------------------------------------------------------------------

# Predict the gmv in the testing dataset
Predict_mul_1 <- predict(mul_cam_25,test_cam_mul[,-25])
test_cam_mul$test_gmv <- Predict_mul_1

# Calculate correlation
r <- cor(test_cam_mul$test_gmv,test_cam_mul$gmv)
# calculate R squared by squaring correlation
rsquared_mul <- cor(test_cam_mul$test_gmv,test_cam_mul$gmv)^2
# check R-squared
rsquared_mul #0.969

#Scatter plot to show the linearity of the data
plot(10^test_cam_mul$gmv, 10^test_cam_mul$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="Actual ", ylab="Prediction")

#-------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_cam_mul<-cv.lm(data = CameraAccessory_mul, form.lm = mul_cam_25, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

#Overall Mean Sqiare Error
attr(CV_cam_mul,"ms")
#0.0124

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(CameraAccessory_mul,mul_cam_25)



######### Kyock Model ############
##################################

# Scaling the variables
CameraAccessory_kyock<-CameraAccessory_df

#Creating a lag variable for gmv
CameraAccessory_kyock<-slide(CameraAccessory_kyock,Var = "gmv",slideBy = -1)

#removing NA from the lag variable
CameraAccessory_kyock <- na.omit(CameraAccessory_kyock)

# scaling the variables
CameraAccessory_kyock[,-25]<-scale(CameraAccessory_kyock[,-25])

#-------------------------------------------------------------------------------------------

# splitting the data between train and test
set.seed(100)
indices = sample.split(CameraAccessory_kyock$gmv, SplitRatio = 0.75)
train_cam_kyock = CameraAccessory_kyock[indices,]
test_cam_kyock = CameraAccessory_kyock[!(indices),]

#Initial model with all the dependent and independent variables
model_kyock<-lm(gmv~. ,data = train_cam_kyock)
summary(model_kyock)

#using stepAIC to remove insignificant variables
step_kyock <- stepAIC(model_kyock, direction="both")
step_kyock

#Model 1
kyock_cam_1<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                  special_sale_calendar + product_verticalCameraLEDLight + 
                  product_verticalTeleconverter + `gmv-1` + product_verticalCameraAccessory, 
                data = train_cam_kyock)
summary(kyock_cam_1)#0.98
vif(kyock_cam_1)


#Model 2
#removing product_verticalTeleconverter column as it has high p value
kyock_cam_2<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                  special_sale_calendar + product_verticalCameraLEDLight + 
                  `gmv-1` + product_verticalCameraAccessory, 
                data = train_cam_kyock)
summary(kyock_cam_2)#0.98
vif(kyock_cam_2)

#Model 3
#removing product_verticalCameraAccessory columns as it has high p value and high vif
kyock_cam_3<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                  special_sale_calendar + product_verticalCameraLEDLight + 
                  `gmv-1` ,   data = train_cam_kyock)
summary(kyock_cam_3)#0.98
vif(kyock_cam_3)

#Model 4
#removing Content_Marketing_weekly_adstock column as it has high p value and high vif value
kyock_cam_4<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                  special_sale_calendar + product_verticalCameraLEDLight + 
                  `gmv-1` ,   data = train_cam_kyock)
summary(kyock_cam_4)#0.98
vif(kyock_cam_4)

#Model 5
#removing special_sale_calendar column as it has high p value
kyock_cam_5<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                  product_verticalCameraLEDLight +`gmv-1` ,   data = train_cam_kyock)
summary(kyock_cam_5)#0.98
vif(kyock_cam_5)

#Model 6
#removing product_verticalCameraLEDLight column as it has high p value 
kyock_cam_6<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                  `gmv-1` ,   data = train_cam_kyock)
summary(kyock_cam_6)#0.98
vif(kyock_cam_6)

#Model 7
#removing deliverybdays column as it has high vif value
kyock_cam_7<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla + 
                  `gmv-1` ,   data = train_cam_kyock)
summary(kyock_cam_7)#0.977
vif(kyock_cam_7)

#Model 8
#removing SEM_weekly_adstock column as it has high vif and high p value
kyock_cam_8<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                  Online_marketing_weekly_adstock  + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla + 
                  `gmv-1` ,   data = train_cam_kyock)
summary(kyock_cam_8)#0.977
vif(kyock_cam_8)

#Model 9
#removing product_procurement_sla column as it has high vif and high p value
kyock_cam_9<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                  NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                  Online_marketing_weekly_adstock  + Radio_weekly_adstock + 
                  units + deliverycdays + sla  +`gmv-1` ,   data = train_cam_kyock)
summary(kyock_cam_9)#0.977
vif(kyock_cam_9)

#Model 10
#removing `gmv-1` column as it has high vif and high p value
kyock_cam_10<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   Online_marketing_weekly_adstock  + Radio_weekly_adstock + 
                   units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_10)#0.97
vif(kyock_cam_10)

#Model 11
#removing Online_marketing_weekly_adstock column as it has high p value and high vif value
kyock_cam_11<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   Radio_weekly_adstock + units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_11)#0.958
vif(kyock_cam_11)

#Model 12
#removing Radio_weekly_adstock column as it has high p value
kyock_cam_12<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock  + 
                   units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_12)#0.952
vif(kyock_cam_12)

#Model 13
#removing Digital_weekly_adstock column as it has high p value and high vif value
kyock_cam_13<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Sponsorship_weekly_adstock  +units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_13)#0.958
vif(kyock_cam_13)

#Model 14
#removing Other_weekly column as it has high p value and high vif value
kyock_cam_14<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Sponsorship_weekly_adstock  +units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_14)#0.956
vif(kyock_cam_14)

#Model 15
#removing Online_marketing_weekly as it is highly colinear
kyock_cam_15<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Sponsorship_weekly_adstock  +units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_15)#0.956
vif(kyock_cam_15)

#Model 16
#removing Content_Marketing_weekly as it has high p value
kyock_cam_16<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                   Sponsorship_weekly +Affiliates_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Sponsorship_weekly_adstock  +units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_16)#0.955
vif(kyock_cam_16)

#Model 17
#removing Digital_weekly as it has high vif value
kyock_cam_17<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                   Sponsorship_weekly +Affiliates_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + TV_weekly_adstock + 
                   Sponsorship_weekly_adstock  +units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_17)#0.938
vif(kyock_cam_17)

#Model 18
#removing Total_Investment_weekly_adstock as it has high vif value
kyock_cam_18<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                   Sponsorship_weekly +Affiliates_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock + Sponsorship_weekly_adstock  +units + deliverycdays + sla ,   data = train_cam_kyock)
summary(kyock_cam_18)#0.938
vif(kyock_cam_18)

#Model 19
#removing sla as it has high vif value
kyock_cam_19<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                   Sponsorship_weekly +Affiliates_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock + Sponsorship_weekly_adstock  +units + deliverycdays ,   data = train_cam_kyock)
summary(kyock_cam_19)#0.938
vif(kyock_cam_19)

#Model 20
#removing Total_Investment_weekly as it has high vif value
kyock_cam_20<-lm(formula = gmv ~TV_weekly + 
                   Sponsorship_weekly +Affiliates_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock + Sponsorship_weekly_adstock  +units + deliverycdays ,   data = train_cam_kyock)
summary(kyock_cam_20)#0.916
vif(kyock_cam_20)

#Model 21
#removing Affiliates_weekly as it has high p value
kyock_cam_21<-lm(formula = gmv ~TV_weekly + Sponsorship_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock + Sponsorship_weekly_adstock  +units + deliverycdays ,   data = train_cam_kyock)
summary(kyock_cam_21)#0.916
vif(kyock_cam_21)

#Model 22
#removing Sponsorship_weekly as it has high vif value
kyock_cam_22<-lm(formula = gmv ~TV_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock + Sponsorship_weekly_adstock  +units + deliverycdays ,   data = train_cam_kyock)
summary(kyock_cam_22)#0.909
vif(kyock_cam_22)

#Model 23
#removing Sponsorship_weekly_adstock as it has high vif value
kyock_cam_23<-lm(formula = gmv ~TV_weekly + SEM_weekly + Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock+units + deliverycdays ,   data = train_cam_kyock)
summary(kyock_cam_23)#0.909
vif(kyock_cam_23)

#Model 24
#removing SEM_weekly as it has high p value
kyock_cam_24<-lm(formula = gmv ~TV_weekly + Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock+units + deliverycdays ,   data = train_cam_kyock)
summary(kyock_cam_24)#0.909
vif(kyock_cam_24)

#Model 25
#removing TV_weekly as it has high p value
kyock_cam_25<-lm(formula = gmv ~Radio_weekly + 
                   NPS_Score_weekly + TV_weekly_adstock+units + deliverycdays ,   data = train_cam_kyock)
summary(kyock_cam_25)#0.909
vif(kyock_cam_25)

#FINAL MODEL -> kyock_cam_25
# Adjusted R-squared:  0.909
# signicant variables : Radio_weekly,NPS_Score_weekly,TV_weekly_adstock+units ,deliverycdays
#-------------------------------------------------------------------------------------------

# Predict the gmv in the testing dataset
Predict_kyock_1 <- predict(kyock_cam_25,test_cam_kyock[,-25])
test_cam_kyock$test_gmv <- Predict_kyock_1

# Calculate correlation
r <- cor(test_cam_kyock$test_gmv,test_cam_kyock$gmv)
# calculate R squared by squaring correlation
rsquared_kyock <- cor(test_cam_kyock$test_gmv,test_cam_kyock$gmv)^2
# check R-squared
rsquared_kyock #0.905

#Scatter plot to show the linearity of the data
plot(test_cam_kyock$gmv, test_cam_kyock$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="Actual ", ylab="Prediction")

#-------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_cam_kyock<-cv.lm(data = CameraAccessory_kyock, form.lm = kyock_cam_25, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

#Overall Mean Sqiare Error
attr(CV_cam_kyock,"ms")
#1.83e+11

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(CameraAccessory_kyock,kyock_cam_25)



########## Distributed Lag Model ##################
##################################################

### Creating lag variables
CameraAccessory_dlm<-CameraAccessory_df

# Lag variables for Adstock (2 lags)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Total_Investment_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Total_Investment_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "TV_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "TV_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Digital_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Digital_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Sponsorship_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Sponsorship_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Content_Marketing_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Content_Marketing_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Online_marketing_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Online_marketing_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Affiliates_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Affiliates_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "SEM_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "SEM_weekly_adstock",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Radio_weekly_adstock",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "Radio_weekly_adstock",slideBy = -2)

# Lag variables for Pricing features
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "mrp",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "mrp",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "list_price",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "list_price",slideBy = -2)

CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "discount",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "discount",slideBy = -2)

# Lag variables for gmv (2lags)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "gmv",slideBy = -1)
CameraAccessory_dlm<-slide(CameraAccessory_dlm,Var = "gmv",slideBy = -2)

colnames(CameraAccessory_dlm)[55:80] <- c('Total_Investment_weekly_adstock_1',
                                          'Total_Investment_weekly_adstock_2',
                                          'TV_weekly_adstock_1','TV_weekly_adstock_2',
                                          'Digital_weekly_adstock_1','Digital_weekly_adstock_2',
                                          'Sponsorship_weekly_adstock_1','Sponsorship_weekly_adstock_2',
                                          'Content_Marketing_weekly_adstock_1',
                                          'Content_Marketing_weekly_adstock_2',
                                          'Online_marketing_weekly_adstock_1',
                                          'Online_marketing_weekly_adstock_2',
                                          'Affiliates_weekly_adstock_1',
                                          'Affiliates_weekly_adstock_2','SEM_weekly_adstock_1',
                                          'SEM_weekly_adstock_2','Radio_weekly_adstock_1',
                                          'Radio_weekly_adstock_2','mrp_1','mrp_2','list_price_1',
                                          'list_price_2','discount_1','discount_2','gmv_1','gmv_2')

CameraAccessory_dlm <- na.omit(CameraAccessory_dlm)

# scaling the variable
CameraAccessory_dlm[,-25]<-scale(CameraAccessory_dlm[,-25])

#-------------------------------------------------------------------------------------------

#set seed
set.seed(100)
indices = sample.split(CameraAccessory_dlm$gmv, SplitRatio = 0.75)
train_cam_dlm = CameraAccessory_dlm[indices,]
test_cam_dlm = CameraAccessory_dlm[!(indices),]

#Initial model with all the dependent and independent variables
model_dlm<-lm(gmv~. ,data = train_cam_dlm)
summary(model_dlm)

#using stepAIC to remove insignificant variables
step_dlm <- stepAIC(model_dlm, direction="both")
step_dlm


#Model 1
dlm_cam_1<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                special_sale_calendar + Total_Investment_weekly_adstock_1 + 
                Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Online_marketing_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                mrp_2 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_1)#0.982
vif(dlm_cam_1)


#Model 2
#removing Total_Investment_weekly_adstock_2 column as it has high p value
dlm_cam_2<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                special_sale_calendar + Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Online_marketing_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                mrp_2 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_2)#0.982
vif(dlm_cam_2)


#Model 3
#removing Online_marketing_weekly_adstock_2 column as it has high p value
dlm_cam_3<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                special_sale_calendar + Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + mrp_2 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_3)#0.982
vif(dlm_cam_3)

#Model 4
#removing Online_marketing_weekly_adstock column as it has high p value
dlm_cam_4<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                special_sale_calendar + Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + mrp_2 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_4)#0.981
vif(dlm_cam_4)


#Model 5
#removing mrp_2  column as it has high p value
dlm_cam_5<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                special_sale_calendar + Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_5)#0.981
vif(dlm_cam_5)


#Model 6
#removing special_sale_calendar  column as it has high p value
dlm_cam_6<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_6)#0.981
vif(dlm_cam_6)


#Model 7
#removing Total_Investment_weekly  column as it has high p value
dlm_cam_7<-lm(formula = gmv ~ TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_7)#0.98
vif(dlm_cam_7)

#Model 8
#removing Content_Marketing_weekly  column as it has high p value
dlm_cam_8<-lm(formula = gmv ~ TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_8)#0.98
vif(dlm_cam_8)


#Model 9
#removing Content_Marketing_weekly  column as it has high p value
dlm_cam_9<-lm(formula = gmv ~ TV_weekly + Digital_weekly + 
                Sponsorship_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_9)#0.98
vif(dlm_cam_9)


#Model 10
#removing TV_weekly  column as it has high p value
dlm_cam_10<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                SEM_weekly_adstock + Radio_weekly_adstock + 
                units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                Total_Investment_weekly_adstock_1 + 
                TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_10)#0.98
vif(dlm_cam_10)


#Model 11
#removing Total_Investment_weekly_adstock_1  column as it has high vif value
dlm_cam_11<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_1 + 
                 Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_11)#0.976
vif(dlm_cam_11)


#Model 12
#removing Online_marketing_weekly_adstock_1  column as it has high p value
dlm_cam_12<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_12)#0.976
vif(dlm_cam_12)


#Model 13
#removing product_procurement_sla  column as it has high p value
dlm_cam_13<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_13)#0.976
vif(dlm_cam_13)


#Model 14
#removing Content_Marketing_weekly_adstock  column as it has high vif value
dlm_cam_14<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock +  SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_14)#0.974
vif(dlm_cam_14)


#Model 15
#removing Radio_weekly_adstock  column as it has high p value
dlm_cam_15<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock +  SEM_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_15)#0.974
vif(dlm_cam_15)


#Model 16
#removing TV_weekly_adstock_1  column as it has high p value
dlm_cam_16<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock +  SEM_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_16)#0.974
vif(dlm_cam_16)


#Model 17
#removing TV_weekly_adstock  column as it has high p value
dlm_cam_17<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock +  SEM_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_17)#0.974
vif(dlm_cam_17)


#Model 18
#removing Sponsorship_weekly_adstock  column as it has high p value
dlm_cam_18<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + Digital_weekly_adstock + 
                 SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 + Sponsorship_weekly_adstock_1 + 
                 Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_18)#0.974
vif(dlm_cam_18)


#Model 19
#removing Sponsorship_weekly_adstock_1  column as it has high p value
dlm_cam_19<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + Digital_weekly_adstock + 
                 SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 +Content_Marketing_weekly_adstock_1 + Radio_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_19)#0.974
vif(dlm_cam_19)


#Model 20
#removing Radio_weekly_adstock_1  column as it has high p value
dlm_cam_20<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + Digital_weekly_adstock + 
                 SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 +Content_Marketing_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_20)#0.974
vif(dlm_cam_20)


#Model 21
#removing SEM_weekly  column as it has high vif value
dlm_cam_21<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + Radio_weekly + Other_weekly + 
                 NPS_Score_weekly + Digital_weekly_adstock + 
                 SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 +Content_Marketing_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_21)#0.974
vif(dlm_cam_21)


#Model 22
#removing Radio_weekly  column as it has high p value
dlm_cam_22<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + Other_weekly + 
                 NPS_Score_weekly + Digital_weekly_adstock + 
                 SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 +Content_Marketing_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_22)#0.955
vif(dlm_cam_22)


#Model 23
#removing Other_weekly  column as it has high p value
dlm_cam_23<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                 SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 +Content_Marketing_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_23)#0.955
vif(dlm_cam_23)

#Model 24
#removing Digital_weekly  column as it has high p value
dlm_cam_24<-lm(formula = gmv ~ Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                 SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 +Content_Marketing_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_24)#0.955
vif(dlm_cam_24)


#Model 25
#removing SEM_weekly_adstock  column as it has high vif value
dlm_cam_25<-lm(formula = gmv ~ Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 +Content_Marketing_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_25)#0.952
vif(dlm_cam_25)

#Model 26
#removing Content_Marketing_weekly_adstock_1  column as it has high vif value
dlm_cam_26<-lm(formula = gmv ~ Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + 
                 Digital_weekly_adstock_1 + gmv_1, data = train_cam_dlm)
summary(dlm_cam_26)#0.952
vif(dlm_cam_26)


#Model 27
#removing Digital_weekly_adstock_1  column as it has  vif value
dlm_cam_27<-lm(formula = gmv ~ Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla +gmv_1, data = train_cam_dlm)
summary(dlm_cam_27)#0.952
vif(dlm_cam_27)

#Model 28
#removing sla  column as it has p value
dlm_cam_28<-lm(formula = gmv ~ Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                 units + deliverybdays + deliverycdays +gmv_1, data = train_cam_dlm)
summary(dlm_cam_28)#0.952
vif(dlm_cam_28)


#Model 29
#removing Digital_weekly_adstock  column as it has p value
dlm_cam_29<-lm(formula = gmv ~ Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + 
                 units + deliverybdays + deliverycdays +gmv_1, data = train_cam_dlm)
summary(dlm_cam_29)#0.952
vif(dlm_cam_29)


#Model 30
#removing deliverybdays  column as it has p value
dlm_cam_30<-lm(formula = gmv ~ Sponsorship_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + NPS_Score_weekly + 
                 units + deliverycdays +gmv_1, data = train_cam_dlm)
summary(dlm_cam_30)#0.951
vif(dlm_cam_30)


#Model 31
#removing Online_marketing_weekly  column as it has vif value
dlm_cam_31<-lm(formula = gmv ~ Sponsorship_weekly + Affiliates_weekly + NPS_Score_weekly + 
                 units + deliverycdays +gmv_1, data = train_cam_dlm)
summary(dlm_cam_31)#0.949
vif(dlm_cam_31)


#Model 32
#removing Sponsorship_weekly  column as it has p value
dlm_cam_32<-lm(formula = gmv ~ Affiliates_weekly + NPS_Score_weekly + 
                 units + deliverycdays +gmv_1, data = train_cam_dlm)
summary(dlm_cam_32)#0.948
vif(dlm_cam_32)


#Model 33
#removing Affiliates_weekly  column as it has vif value
dlm_cam_33<-lm(formula = gmv ~ NPS_Score_weekly + 
                 units + deliverycdays +gmv_1, data = train_cam_dlm)
summary(dlm_cam_33)#0.947
vif(dlm_cam_33)

#Model 34
#removing units  column as it has vif value
dlm_cam_34<-lm(formula = gmv ~ NPS_Score_weekly + 
                 deliverycdays +gmv_1, data = train_cam_dlm)
summary(dlm_cam_34)#0.926
vif(dlm_cam_34)

#FINAL MODEL -> dlm_cam_34
# Adjusted R-squared:  0.926
# signicant variables : NPS_Score_weekly,deliverycdays ,gmv_1
#-------------------------------------------------------------------------------------------

# Predict the gmv in the testing dataset
Predict_dlm_1 <- predict(dlm_cam_34,test_cam_dlm[,-25])
test_cam_dlm$test_gmv <- Predict_dlm_1

# Calculate correlation
r <- cor(test_cam_dlm$test_gmv,test_cam_dlm$gmv)
# calculate R squared by squaring correlation
rsquared_dlm <- cor(test_cam_dlm$test_gmv,test_cam_dlm$gmv)^2
# check R-squared
rsquared_dlm #0.851

#Scatter plot to show the linearity of the data
plot(test_cam_dlm$gmv, test_cam_dlm$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="Actual ", ylab="Prediction")

#-------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_cam_dlm<-cv.lm(data = CameraAccessory_dlm, form.lm = dlm_cam_34, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

#Overall Mean Sqiare Error
attr(CV_cam_dlm,"ms")
#1.89e+11

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(CameraAccessory_dlm,dlm_cam_34)



######## Distributed Lag + Multiplicative Model #########
#########################################################

### Creating lag variables
CameraAccessory_dlml<-CameraAccessory_df

# Lag variables for Adstock (2 lags)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Total_Investment_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Total_Investment_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "TV_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "TV_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Digital_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Digital_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Sponsorship_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Sponsorship_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Content_Marketing_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Content_Marketing_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Online_marketing_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Online_marketing_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Affiliates_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Affiliates_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "SEM_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "SEM_weekly_adstock",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Radio_weekly_adstock",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "Radio_weekly_adstock",slideBy = -2)

# Lag variables for Pricing features
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "mrp",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "mrp",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "list_price",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "list_price",slideBy = -2)

CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "discount",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "discount",slideBy = -2)

# Lag variables for gmv (2lags)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "gmv",slideBy = -1)
CameraAccessory_dlml<-slide(CameraAccessory_dlml,Var = "gmv",slideBy = -2)

colnames(CameraAccessory_dlml)[55:80] <- c('Total_Investment_weekly_adstock_1',
                                          'Total_Investment_weekly_adstock_2',
                                          'TV_weekly_adstock_1','TV_weekly_adstock_2',
                                          'Digital_weekly_adstock_1','Digital_weekly_adstock_2',
                                          'Sponsorship_weekly_adstock_1','Sponsorship_weekly_adstock_2',
                                          'Content_Marketing_weekly_adstock_1',
                                          'Content_Marketing_weekly_adstock_2',
                                          'Online_marketing_weekly_adstock_1',
                                          'Online_marketing_weekly_adstock_2',
                                          'Affiliates_weekly_adstock_1',
                                          'Affiliates_weekly_adstock_2','SEM_weekly_adstock_1',
                                          'SEM_weekly_adstock_2','Radio_weekly_adstock_1',
                                          'Radio_weekly_adstock_2','mrp_1','mrp_2','list_price_1',
                                          'list_price_2','discount_1','discount_2','gmv_1','gmv_2')

CameraAccessory_dlml <- na.omit(CameraAccessory_dlml)

# making values 0 to 0.001 as log(0) is undefined
CameraAccessory_dlml[CameraAccessory_dlml<=0]<-0.001

# converting them into log variables
CameraAccessory_dlml<-log(CameraAccessory_dlml)

#-------------------------------------------------------------------------------------------

#set seed and splitting the data into test and train
set.seed(100)
indices = sample.split(CameraAccessory_dlml$gmv, SplitRatio = 0.75)
train_cam_dlml = CameraAccessory_dlml[indices,]
test_cam_dlml = CameraAccessory_dlml[!(indices),]

#Initial model with all the dependent and independent variables
model_dlml<-lm(gmv~. ,data = train_cam_dlml)
summary(model_dlml)

#using stepAIC to remove insignificant variables
step_dlml<- stepAIC(model_dlml, direction="both")
step_dlml


#Model 1
model_dlml_1<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Affiliates_weekly_adstock + SEM_weekly_adstock + list_price + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   product_verticalCameraHousing + product_verticalCameraMicrophone + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   mrp_1 + discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_1)#0.996
vif(model_dlml_1)


#Model 2
#removing product_verticalCameraMicrophone column as it has high p value
model_dlml_2<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Affiliates_weekly_adstock + SEM_weekly_adstock + list_price + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   product_verticalCameraHousing +Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   mrp_1 + discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_2)#0.996
vif(model_dlml_2)


#Model 3
#removing product_verticalCameraHousing column as it has high p value
model_dlml_3<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Affiliates_weekly_adstock + SEM_weekly_adstock + list_price + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   mrp_1 + discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_3)#0.996
vif(model_dlml_3)


#Model 4
#removing Affiliates_weekly_adstock column as it has high p value
model_dlml_4<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   SEM_weekly_adstock + list_price + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   mrp_1 + discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_4)#0.996
vif(model_dlml_4)


#Model 5
#removing list_price column as it has high p value
model_dlml_5<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   mrp_1 + discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_5)#0.996
vif(model_dlml_5)


#Model 6
#removing mrp_1 column as it has high p value
model_dlml_6<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_6)#0.996
vif(model_dlml_6)


#Model 7
#removing week_number column as it has high p value
model_dlml_7<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_7)#0.995
vif(model_dlml_7)


#Model 8
#removing SEM_weekly_adstock column as it has high p value
model_dlml_8<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   discount_1 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_8)#0.995
vif(model_dlml_8)

#Model 9
#removing discount_1 column as it has high p value
model_dlml_9<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_1 + Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_9)#0.995
vif(model_dlml_9)


#Model 10
#removing Total_Investment_weekly_adstock_1 column as it has high p value
model_dlml_10<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                   NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                   units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   Total_Investment_weekly_adstock_2 + 
                   TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                   Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                   Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                   Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                   Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                   SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                   gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_10)#0.995
vif(model_dlml_10)


#Model 11
#removing Total_Investment_weekly_adstock_2 column as it has high p value
model_dlml_11<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                    Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                    SEM_weekly_adstock_1 + SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                    gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_11)#0.995
vif(model_dlml_11)


#Model 12
#removing SEM_weekly_adstock_1 column as it has high p value
model_dlml_12<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                    Content_Marketing_weekly_adstock_1 + Content_Marketing_weekly_adstock_2 + 
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                    SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                    gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_12)#0.995
vif(model_dlml_12)


#Model 13
#removing Content_Marketing_weekly_adstock_2 column as it has high p value
model_dlml_13<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                    Content_Marketing_weekly_adstock_1 + 
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                    SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                    gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_13)#0.995
vif(model_dlml_13)


#Model 14
#removing Content_Marketing_weekly_adstock_1 column as it has high p value
model_dlml_14<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                    SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + 
                    gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_14)#0.995
vif(model_dlml_14)


#Model 15
#removing SEM_weekly_adstock_2 column as it has high p value
model_dlml_15<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Affiliates_weekly_adstock_1 + Affiliates_weekly_adstock_2 + 
                    Radio_weekly_adstock_2 +gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_15)#0.995
vif(model_dlml_15)

#Model 16
#removing Affiliates_weekly_adstock_1 column as it has high vif value
model_dlml_16<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Affiliates_weekly_adstock_2 + 
                    Radio_weekly_adstock_2 +gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_16)#0.995
vif(model_dlml_16)


#Model 17
#removing Affiliates_weekly_adstock_2 column as it has high p value
model_dlml_17<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Radio_weekly_adstock_2 +gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_17)#0.995
vif(model_dlml_17)

#Model 18
#removing Other_weekly column as it has high vif value
model_dlml_18<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    Radio_weekly_adstock_2 +gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_18)#0.994
vif(model_dlml_18)


#Model 19
#removing Radio_weekly_adstock_2 column as it has high p value
model_dlml_19<-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_19)#0.994
vif(model_dlml_19)


#Model 20
#removing Total_Investment_weekly column as it has high p value
model_dlml_20<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                    gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_20)#0.994
vif(model_dlml_20)


#Model 21
#removing Online_marketing_weekly_adstock_1 column as it has high p value
model_dlml_21<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_21)#0.994
vif(model_dlml_21)

#Model 22
#removing Content_Marketing_weekly column as it has high p value
model_dlml_22<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 +
                    Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_22)#0.994
vif(model_dlml_22)


#Model 23
#removing Sponsorship_weekly_adstock_2 column as it has high vif value
model_dlml_23<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 +Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_23)#0.994
vif(model_dlml_23)


#Model 24
#removing Digital_weekly_adstock_1 column as it has high p value
model_dlml_24<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units + deliverybdays + deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1+ Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 +Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_24)#0.994
vif(model_dlml_24)


#Model 25
#removing deliverybdays column as it has high p value
model_dlml_25<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units+ deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1+ Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 +Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_25)#0.993
vif(model_dlml_25)



#Model 26
#removing Affiliates_weekly column as it has high vif value
model_dlml_26<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units+ deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1+ Digital_weekly_adstock_2 + 
                    Sponsorship_weekly_adstock_1 +Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_26)#0.991
vif(model_dlml_26)


#Model 27
#removing Sponsorship_weekly_adstock_1 column as it has high p value
model_dlml_27<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Content_Marketing_weekly_adstock + 
                    units+ deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1+ Digital_weekly_adstock_2 + 
                    Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_27)#0.991
vif(model_dlml_27)


#Model 28
#removing Total_Investment_weekly_adstock column as it has high p value
model_dlml_28<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Content_Marketing_weekly_adstock + 
                    units+ deliverycdays + sla + product_procurement_sla +
                    TV_weekly_adstock_1+ Digital_weekly_adstock_2 + 
                    Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_28)#0.99
vif(model_dlml_28)


#Model 29
#removing TV_weekly_adstock_1 column as it has high p value
model_dlml_29<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Content_Marketing_weekly_adstock + 
                    units+ deliverycdays + sla + product_procurement_sla +
                    Digital_weekly_adstock_2 +Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_29)#0.99
vif(model_dlml_29)


#Model 30
#removing units column as it has high vif value
model_dlml_30<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Content_Marketing_weekly_adstock + 
                    deliverycdays + sla + product_procurement_sla +
                    Digital_weekly_adstock_2 +Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_30)#0.955
vif(model_dlml_30)


#Model 31
#removing product_procurement_sla column as it has high p value
model_dlml_31<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    Digital_weekly_adstock_2 +Online_marketing_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_31)#0.955
vif(model_dlml_31)


#Model 32
#removing Online_marketing_weekly_adstock_2 column as it has high p value
model_dlml_32<-lm(formula = gmv ~ TV_weekly + Digital_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    Digital_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_32)#0.955
vif(model_dlml_32)


#Model 33
#removing Digital_weekly column as it has high p value
model_dlml_33<-lm(formula = gmv ~ TV_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    Digital_weekly_adstock_2 + gmv_1 + gmv_2, data = train_cam_dlml)
summary(model_dlml_33)#0.955
vif(model_dlml_33)


#Model 34
#removing gmv_2 column as it has high p value
model_dlml_34<-lm(formula = gmv ~ TV_weekly + Sponsorship_weekly + 
                    SEM_weekly + Radio_weekly + 
                    NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    Digital_weekly_adstock_2 + gmv_1, data = train_cam_dlml)
summary(model_dlml_34)#0.955
vif(model_dlml_34)


#Model 35
#removing Radio_weekly column as it has high p value
model_dlml_35<-lm(formula = gmv ~ TV_weekly + Sponsorship_weekly + 
                    SEM_weekly +NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    Digital_weekly_adstock_2 + gmv_1, data = train_cam_dlml)
summary(model_dlml_35)#0.955
vif(model_dlml_35)


#Model 36
#removing Sponsorship_weekly column as it has high p value
model_dlml_36<-lm(formula = gmv ~ TV_weekly + SEM_weekly +NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    Digital_weekly_adstock_2 + gmv_1, data = train_cam_dlml)
summary(model_dlml_36)#0.947
vif(model_dlml_36)


#Model 37
#removing TV_weekly column as it has high p value
model_dlml_37<-lm(formula = gmv ~ SEM_weekly +NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    Digital_weekly_adstock_2 + gmv_1, data = train_cam_dlml)
summary(model_dlml_37)#0.947
vif(model_dlml_37)


#Model 38
#removing Digital_weekly_adstock_2 column as it has high p value
model_dlml_38<-lm(formula = gmv ~ SEM_weekly +NPS_Score_weekly + Content_Marketing_weekly_adstock + deliverycdays + sla +
                    gmv_1, data = train_cam_dlml)
summary(model_dlml_38)#0.947
vif(model_dlml_38)


#Model 39
#removing Content_Marketing_weekly_adstock column as it has high p value and high vif value
model_dlml_39<-lm(formula = gmv ~ SEM_weekly +NPS_Score_weekly + deliverycdays + sla +
                    gmv_1, data = train_cam_dlml)
summary(model_dlml_39)#0.947
vif(model_dlml_39)


#Model 40
#removing SEM_weekly column as it has high p value and high vif value
model_dlml_40<-lm(formula = gmv ~ NPS_Score_weekly + deliverycdays + sla +
                    gmv_1, data = train_cam_dlml)
summary(model_dlml_40)#0.946
vif(model_dlml_40)


#Model 41
#removing sla column as it has high p value and high vif value
model_dlml_41<-lm(formula = gmv ~ NPS_Score_weekly + deliverycdays +gmv_1, data = train_cam_dlml)
summary(model_dlml_41)#0.945
vif(model_dlml_41)

#FINAL MODEL -> model_dlml_41
# Adjusted R-squared:  0.945
# signicant variables : NPS_Score_weekly ,deliverycdays ,gmv_1

#-------------------------------------------------------------------------------------------


# Predict the gmv in the testing dataset
Predict_dlml_1 <- predict(model_dlml_41,test_cam_dlml[,-25])
test_cam_dlml$test_gmv <- Predict_dlml_1

# Calculate correlation
r <- cor(test_cam_dlml$test_gmv,test_cam_dlml$gmv)
# calculate R squared by squaring correlation
rsquared_dlml <- cor(test_cam_dlml$test_gmv,test_cam_dlml$gmv)^2
# check R-squared
rsquared_dlml #0.9

#Scatter plot
plot(10^test_cam_dlml$gmv, 10^test_cam_dlml$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="Actual ", ylab="Prediction")

#-------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_cam_dlml<-cv.lm(data = CameraAccessory_dlml, form.lm = model_dlml_41, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

#Overall Mean Sqiare Error
attr(CV_cam_dlm,"ms")
#0.0508

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(CameraAccessory_dlml,model_dlml_41)



############################################################################################################
##################################### Gaming Accessory  Model Building #####################################
############################################################################################################


###### Linear model ###########
###############################

# Train : First 40 weeks, Test: Last 12 Weeks
GamingAccessory_linear<-GamingAccessory_df[which(GamingAccessory_df$week_number<41),]
GamingAccessory_linear_test<-GamingAccessory_df[which(GamingAccessory_df$week_number>=41),]

#Scaling only train data and not scaling gmv
str(GamingAccessory_linear)
GamingAccessory_linear[,-c(25,seq(32,46))]<-scale(GamingAccessory_linear[,-c(25,seq(32,46))])

#-------------------------------------------------------------------------------------------

#set seed
set.seed(100)

#Initial model with all the dependent and independent variables
model_ln<-lm(gmv~. ,data = GamingAccessory_linear)
summary(model_ln)

#using stepAIC to remove insignificant variables
step_game_lm <- stepAIC(model_ln, direction="both")
step_game_lm

#Model 1
linear_game_1<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                    SEM_weekly_adstock + Radio_weekly_adstock + discount + units + 
                    deliverybdays + deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_1)#0.995
vif(linear_game_1)

#Model 2
#removing discount as it has high P value
linear_game_2<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                    SEM_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverybdays + deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_2)#0.995
vif(linear_game_2)

#Model 3
#removing Total_Investment_weekly  column as it has  high vif
linear_game_3<-lm(formula = gmv ~ week_number + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                    SEM_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverybdays + deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_3)#0.992
vif(linear_game_3)

#Model 4
#removing TV_weekly_adstock column as it is high p value
linear_game_4<-lm(formula = gmv ~ week_number + TV_weekly + 
                    Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                    SEM_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverybdays + deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_4)#0.992
vif(linear_game_4)


#Model 5
#removing Digital_weekly  column as it has high vif
linear_game_5<-lm(formula = gmv ~ week_number + TV_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                    SEM_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverybdays + deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_5)#0.989
vif(linear_game_5)


#Model 6
#removing Radio_weekly_adstock column as it has high P value
linear_game_6<-lm(formula = gmv ~ week_number + TV_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                    SEM_weekly_adstock + units + 
                    deliverybdays + deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_6)#0.989
vif(linear_game_6)

#Model 7
#removing sla column as it has high P value
linear_game_7<-lm(formula = gmv ~ week_number + TV_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                    SEM_weekly_adstock + units + 
                    deliverybdays + deliverycdays + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_7)#0.989
vif(linear_game_7)

#Model 8
#removing Online_marketing_weekly_adstock column as it has high VIF value
linear_game_8<-lm(formula = gmv ~ week_number + TV_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + units + 
                    deliverybdays + deliverycdays + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_8)#0.989
vif(linear_game_8)

#Model 9
#removing Affiliates_weekly_adstock column as it has high P value
linear_game_9<-lm(formula = gmv ~ week_number + TV_weekly + 
                    Sponsorship_weekly + Content_Marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                    Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                    SEM_weekly_adstock + units + 
                    deliverybdays + deliverycdays + product_procurement_sla + 
                    special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_9)#0.989
vif(linear_game_9)

#Model 10
#removing Sponsorship_weekly column as it has high P value
linear_game_10<-lm(formula = gmv ~ week_number + TV_weekly + Content_Marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + 
                     deliverybdays + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_10)#0.989
vif(linear_game_10)

#Model 11
#removing TV_weekly column as it has P
linear_game_11<-lm(formula = gmv ~ week_number + Content_Marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + 
                     deliverybdays + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_11)#0.989
vif(linear_game_11)

#Model 12
#removing SEM_weekly column as it has high VIF and high P value
linear_game_12<-lm(formula = gmv ~ week_number + Content_Marketing_weekly + 
                     Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + 
                     deliverybdays + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_12)#0.988
vif(linear_game_12)


#Model 13
#removing Total_Investment_weekly_adstock column as it has high VIF and highly correlated
linear_game_13<-lm(formula = gmv ~ week_number + Content_Marketing_weekly + 
                     Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                     Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + deliverybdays + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_13)#0.986
vif(linear_game_13)


#Model 14
#removing Content_Marketing_weekly  column as it has high P value and highly significant
linear_game_14<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                     Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + deliverybdays + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_14)#0.986
vif(linear_game_14)

#Model 15
#removing deliverybdays column as it has high VIF value
linear_game_15<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                     Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_15)#0.984
vif(linear_game_15)

#Model 16
#removing Digital_weekly_adstock column as it has high VIF value
linear_game_16<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_16)#0.984
vif(linear_game_16)

#Model 17
#removing NPS_Score_weekly column as it has high VIF value
linear_game_17<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     SEM_weekly_adstock + units + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_17)#0.978
vif(linear_game_17)

#Model 18
#removing SEM_weekly_adstock column as it has high VIF value
linear_game_18<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     units + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_18)#0.977
vif(linear_game_18)

#Model 19
#removing Content_Marketing_weekly_adstock column as it has high P and VIF value
linear_game_19<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + 
                     Sponsorship_weekly_adstock + units + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_19)#0.976
vif(linear_game_19)

#Model 20
#checking if units is important
linear_game_20<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + 
                     Sponsorship_weekly_adstock + deliverycdays + product_procurement_sla + 
                     special_sale_calendar, data = GamingAccessory_linear)
summary(linear_game_20)#0.7055  CANNOT REMOVE UNITS EVEN THOUGH VIF>5
vif(linear_game_20)

#Model 21
#removing special_sale_calendar column as it has high P
linear_game_21<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + 
                     Sponsorship_weekly_adstock + units + deliverycdays + product_procurement_sla
                   , data = GamingAccessory_linear)
summary(linear_game_21)#0.975
vif(linear_game_21)

#Model 21
#removing product_procurement_sla column as it has high P and VIF value
linear_game_21<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + 
                     Sponsorship_weekly_adstock + units + deliverycdays
                   , data = GamingAccessory_linear)
summary(linear_game_21)#0.973
vif(linear_game_21)

#Model 22
#removing deliverycdays column as it has high P
linear_game_21<-lm(formula = gmv ~ week_number + Affiliates_weekly + Radio_weekly + 
                     Sponsorship_weekly_adstock + units
                   , data = GamingAccessory_linear)
summary(linear_game_21)#0.972
vif(linear_game_21)

#FINAL MODEL -> linear_game_21
# Adjusted R-squared:  0.972
# signicant variables : week_number , Affiliates_weekly , Radio_weekly , Sponsorship_weekly_adstock , units

#-------------------------------------------------------------------------------------------

#Cross validation with 10 folds
CV_game <-CVlm(data = GamingAccessory_linear, form.lm = linear_game_21, m=10, dots = FALSE, seed=50, plotit="Residual", printit=TRUE) #2.86e+11

#Overall Mean Square Error
attr(CV_game,"ms") #1.37e+10

#-------------------------------------------------------------------------------------------
GamingAccessory_linear_test[,-c(25,seq(32,46))]<-scale(GamingAccessory_linear_test[,-c(25,seq(32,46))])

actual_game_ln <- GamingAccessory_linear_test$gmv
predicted_game_ln <- predict(linear_game_21,newdata = GamingAccessory_linear_test[-25])
#Scatter plot
plot(actual_game_ln, predicted_game_ln, main="Correlation between Predicted and Actual Values", 
     xlab="Actual ", ylab="Prediction")

#Correlation & R^2 between actual gmv and predicted gmv
cor(actual_game_ln,predicted_game_ln) #0.969
cor(actual_game_ln,predicted_game_ln)^2 #0.939

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(GamingAccessory_linear,linear_game_21)


###### Multiplicative model ######### 
#####################################

#Converting 0 to 0.001 as log(0) is not defined
# Train : First 40 weeks, Test: Last 12 Weeks
GamingAccessory_mul<-GamingAccessory_df[which(GamingAccessory_df$week_number<41),]
GamingAccessory_mul_test<-GamingAccessory_df[which(GamingAccessory_df$week_number>=41),]

GamingAccessory_mul[GamingAccessory_mul<=0] <- 0.001

#converting into logs
GamingAccessory_mul<-log(GamingAccessory_mul)

#-------------------------------------------------------------------------------------------

#set seed
set.seed(100)

#Initial model with all the dependent and independent variables
model_mul<-lm(gmv~. ,data = GamingAccessory_mul)
summary(model_mul)

#using stepAIC to remove insignificant variables
step_game_mul <- stepAIC(model_mul, direction="both")
step_game_mul


#Model 1
mul_game_1<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla + special_sale_calendar + 
                 product_verticalGameControlMount, data = GamingAccessory_mul)
summary(mul_game_1)#0.999
vif(mul_game_1)


#Model 2
#removing special_sale_calendar as it has high p value
mul_game_2<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla + 
                 product_verticalGameControlMount, data = GamingAccessory_mul)
summary(mul_game_2)#0.999
vif(mul_game_2)

#Model 3
# removing product_verticalGameControlMount column as it has high p value
mul_game_3<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_3)#0.999
vif(mul_game_3)

#Model 4
# removing Radio_weekly column as it has high vif value and high p value
mul_game_4<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_4)#0.999
vif(mul_game_4)

#Model 5
# removing Content_Marketing_weekly_adstock column as it has high p value
mul_game_5<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                 Affiliates_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_5)#0.999
vif(mul_game_5)

#Model 6
#removing Affiliates_weekly column as it has high p value and high vif value
mul_game_6<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                 Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_6)#0.998
vif(mul_game_6)

#Model 7
#removing Content_Marketing_weekly column as it has high p value and high vif value
mul_game_7<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_7)#0.998
vif(mul_game_7)

#Model 8 
#removing Sponsorship_weekly_adstock column as it has high vif value
mul_game_8<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_8)#0.998
vif(mul_game_8)

#Model 9
#removing deliverybdays column as it has high p value and high vif value
mul_game_9<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + 
                 deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_9)#0.997
vif(mul_game_9)

#Model 10
#removing Affiliates_weekly_adstock column as it has high p value and high vif value
mul_game_10<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                  Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_10)#0.997
vif(mul_game_10)

#Model 11
#removing Digital_weekly_adstock column as it has high p value and high vif value
mul_game_11<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                  Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_11)#0.997
vif(mul_game_11)

#Model 12
#removing TV_weekly column as it has high vif value
mul_game_12<-lm(formula = gmv ~ week_number + Total_Investment_weekly + 
                  Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_12)#0.995
vif(mul_game_12)

#model 13
#removing Total_Investment_weekly column as it has high p value
mul_game_13<-lm(formula = gmv ~ week_number + Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_13)#0.995
vif(mul_game_13)

#model 14
#removing TV_weekly_adstock column as it has high p value
mul_game_14<-lm(formula = gmv ~ week_number + Digital_weekly + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_14)#0.995
vif(mul_game_14)

#model 15
#removing Digital_weekly column as it has high p value
mul_game_15<-lm(formula = gmv ~ week_number + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays + product_procurement_sla , data = GamingAccessory_mul)
summary(mul_game_15)#0.995
vif(mul_game_15)

#model 16
#removing product_procurement_sla column as it has high p value
mul_game_16<-lm(formula = gmv ~ week_number + Online_marketing_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays , data = GamingAccessory_mul)
summary(mul_game_16)#0.995
vif(mul_game_16)

#model 17
#removing Online_marketing_weekly column as it has high p value
mul_game_17<-lm(formula = gmv ~ week_number + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays , data = GamingAccessory_mul)
summary(mul_game_17)#0.995
vif(mul_game_17)

#model 18
#removing Total_Investment_weekly_adstock column as it has high p value
mul_game_18<-lm(formula = gmv ~ week_number + Other_weekly + NPS_Score_weekly + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays , data = GamingAccessory_mul)
summary(mul_game_18)#0.994
vif(mul_game_18)

#model 19
#removing Online_marketing_weekly_adstock column as it has high p value
mul_game_19<-lm(formula = gmv ~ week_number + Other_weekly + NPS_Score_weekly + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays , data = GamingAccessory_mul)
summary(mul_game_19)#0.994
vif(mul_game_19)

#model 20
#removing NPS_Score_weekly column as it has high p value
mul_game_20<-lm(formula = gmv ~ week_number + Other_weekly + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + 
                  deliverycdays , data = GamingAccessory_mul)
summary(mul_game_20)#0.994
vif(mul_game_20)

#model 21
#removing Radio_weekly_adstock column as it has high p value
mul_game_21<-lm(formula = gmv ~ week_number + Other_weekly + 
                  SEM_weekly_adstock + units + deliverycdays , data = GamingAccessory_mul)
summary(mul_game_21) #0.994
vif(mul_game_21)

#Final model : mul_game_21
# Adjusted R-squared:  0.994
# signicant variables : week_number,Other_weekly ,SEM_weekly_adstock ,units , deliverycdays
#-------------------------------------------------------------------------------------------

#Cross validation with 10 folds
CV_game_mul<-cv.lm(data = GamingAccessory_mul, form.lm = mul_game_21, m=10, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

#Overall Mean Sqiare Error
attr(CV_game_mul,"ms")
#0.00536

#-------------------------------------------------------------------------------------------
GamingAccessory_mul_test[GamingAccessory_mul_test<=0] <- 0.001
#converting into logs
GamingAccessory_mul_test<-log(GamingAccessory_mul_test)

actual_game_mul <- GamingAccessory_mul_test$gmv
predicted_game_mul <- predict(mul_game_21,newdata = GamingAccessory_mul_test[-25])
#Scatter plot
plot(actual_game_mul, predicted_game_mul, main="Correlation between Predicted and Actual Values", 
     xlab="Actual ", ylab="Prediction")

#Correlation & R^2 between actual gmv and predicted gmv
cor(actual_game_mul,predicted_game_mul) #0.989
cor(actual_game_mul,predicted_game_mul)^2 #0.977

#-------------------------------------------------------------------------------------------
# Calculating the elasticity of each variable
elas(GamingAccessory_mul,mul_game_21)




########## Koyck model ############
###################################

#Creating a lag variable for gmv
GamingAccessory_lag<-slide(GamingAccessory_df,Var = "gmv",slideBy = -1)
colnames(GamingAccessory_lag)[47] <- 'gmv_lag'

#removing NA from the lag variable
GamingAccessory_lag <- na.omit(GamingAccessory_lag)

# Train : First 40 weeks, Test: Last 12 Weeks
GamingAccessory_kyock<-GamingAccessory_lag[which(GamingAccessory_lag$week_number<41),]
GamingAccessory_kyock_test<-GamingAccessory_lag[which(GamingAccessory_lag$week_number>=41),]

#Scaling only train data and not scaling gmv
str(GamingAccessory_kyock)
GamingAccessory_kyock[,-c(25,seq(31,46))]<-scale(GamingAccessory_kyock[,-c(25,seq(31,46))])

#-------------------------------------------------------------------------------------------

#set seed
set.seed(100)

#Initial model with all the dependent and independent variables
model_ky<-lm(gmv~. ,data = GamingAccessory_kyock)
summary(model_ky)

#using stepAIC to remove insignificant variables
step_ky <- stepAIC(model_ky, direction="both")
step_ky

#Model 1
kyock_game_1<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                   Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                   SEM_weekly_adstock + Radio_weekly_adstock + discount + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_1)#0.995
vif(kyock_game_1)


#Model 2
#removing discount column as it has high p value
kyock_game_2<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                   Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                   SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_2)#0.995
vif(kyock_game_2)

#Model 3
#removing Total_Investment_weekly_adstock column as it has high p value
kyock_game_3<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   TV_weekly_adstock + Digital_weekly_adstock + 
                   Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                   SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_3)#0.992
vif(kyock_game_3)

#Model 4
#removing TV_weekly column as it has high p value
kyock_game_4<-lm(formula = gmv ~ week_number + Total_Investment_weekly + 
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   TV_weekly_adstock + Digital_weekly_adstock + 
                   Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                   SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_4)#0.992
vif(kyock_game_4)

#Model 5
#removing Total_Investment_weekly column as it has high vif
kyock_game_5<-lm(formula = gmv ~ week_number +  
                   Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   TV_weekly_adstock + Digital_weekly_adstock + 
                   Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                   SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_5)#0.99
vif(kyock_game_5)

#Model 6
#removing Content_Marketing_weekly column as it has high P
kyock_game_6<-lm(formula = gmv ~ week_number +  
                   Digital_weekly + Sponsorship_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   TV_weekly_adstock + Digital_weekly_adstock + 
                   Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                   SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_6)#0.99
vif(kyock_game_6)

#Model 7
#removing TV_weekly_adstock column as it has high P
kyock_game_7<-lm(formula = gmv ~ week_number +  
                   Digital_weekly + Sponsorship_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                   SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_7)#0.99
vif(kyock_game_7)

#Model 8
#removing Affiliates_weekly_adstock column as it has high P
kyock_game_8<-lm(formula = gmv ~ week_number +  
                   Digital_weekly + Sponsorship_weekly + 
                   Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_8)#0.99
vif(kyock_game_8)

#Model 9
#removing SEM_weekly column as it has high vif
kyock_game_9<-lm(formula = gmv ~ week_number +  
                   Digital_weekly + Sponsorship_weekly + 
                   Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                   Digital_weekly_adstock + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + units + 
                   deliverybdays + deliverycdays + sla + product_procurement_sla + 
                   special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_9)#0.989
vif(kyock_game_9)

#Model 10
#removing Sponsorship_weekly_adstock column as it has high p
kyock_game_10<-lm(formula = gmv ~ week_number +  
                    Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                    Digital_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverybdays + deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_10)#0.989
vif(kyock_game_10)

#Model 11
#removing deliverybdays column as it has high vif
kyock_game_11<-lm(formula = gmv ~ week_number +  
                    Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                    Digital_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_11)#0.989
vif(kyock_game_11)

#Model 12
#removing SEM_weekly_adstock column as it has high vif
kyock_game_12<-lm(formula = gmv ~ week_number +  
                    Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                    Digital_weekly_adstock + Content_Marketing_weekly_adstock + 
                    Online_marketing_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_12)#0.983
vif(kyock_game_12)

#Model 13
#removing Content_Marketing_weekly_adstock column as it has high P
kyock_game_13<-lm(formula = gmv ~ week_number +  
                    Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + Radio_weekly_adstock + units + 
                    deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_13)#0.983
vif(kyock_game_13)

#Model 14
#removing Radio_weekly_adstock column as it has high P
kyock_game_14<-lm(formula = gmv ~ week_number +  
                    Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    deliverycdays + sla + product_procurement_sla + 
                    special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_14)#0.983
vif(kyock_game_14)

#Model 15
#removing sla column as it has high P
kyock_game_15<-lm(formula = gmv ~ week_number +  
                    Digital_weekly + Sponsorship_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    deliverycdays + product_procurement_sla + 
                    special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_15)#0.983
vif(kyock_game_15)

#Model 16
#removing Sponsorship_weekly column as it has high vif
kyock_game_16<-lm(formula = gmv ~ week_number + Digital_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    deliverycdays + product_procurement_sla + 
                    special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_16)#0.981
vif(kyock_game_16)

#Model 17
#removing deliverycdays column as it has high P
kyock_game_17<-lm(formula = gmv ~ week_number + Digital_weekly + 
                    Affiliates_weekly + Radio_weekly + NPS_Score_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    product_procurement_sla + special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_17)#0.981
vif(kyock_game_17)

#Model 18
#removing NPS_Score_weekly column as it has high vif
kyock_game_18<-lm(formula = gmv ~ week_number + Digital_weekly + 
                    Affiliates_weekly + Radio_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    product_procurement_sla + special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_18)#0.98
vif(kyock_game_18)

#Model 19
#removing Digital_weekly column as it has high vif
kyock_game_19<-lm(formula = gmv ~ week_number + 
                    Affiliates_weekly + Radio_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    product_procurement_sla + special_sale_calendar + gmv_lag, data = GamingAccessory_kyock)
summary(kyock_game_19)#0.98
vif(kyock_game_19)

#Model 20
#removing gmv_lag column as it has high vif , also cant take out units
kyock_game_20<-lm(formula = gmv ~ week_number + 
                    Affiliates_weekly + Radio_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    product_procurement_sla + special_sale_calendar, data = GamingAccessory_kyock)
summary(kyock_game_20)#0.9773
vif(kyock_game_20)


#Model 21
#removing Affiliates_weekly column as it has high vif , also cant take out units
kyock_game_21<-lm(formula = gmv ~ week_number + 
                    Radio_weekly + Digital_weekly_adstock + 
                    Online_marketing_weekly_adstock + units + 
                    product_procurement_sla, data = GamingAccessory_kyock)
summary(kyock_game_21)#0.966
vif(kyock_game_21)

#final model would be kyock_game_21
# Adjusted R-squared:  0.966
# signicant variables : week_number,Radio_weekly ,Digital_weekly_adstock, Online_marketing_weekly_adstock ,units, product_procurement_sla

#-------------------------------------------------------------------------------------------

#Test the model: Scaling
GamingAccessory_kyock_test[,-c(25,seq(31,46))]<-scale(GamingAccessory_kyock_test[,-c(25,seq(31,46))])
actual_game_kyock <- GamingAccessory_kyock_test$gmv
predicted_game_kyock <- predict(kyock_game_21,newdata = GamingAccessory_kyock_test[-25])

#Scatter plot
plot(actual_game_kyock, predicted_game_kyock, main="Correlation between Predicted and Actual Valuesr", 
     xlab="actual ", ylab="predicted")

#Correlation & R^2 between actual gmv and predicted gmv
cor(actual_game_kyock,predicted_game_kyock) #0.969
cor(actual_game_kyock,predicted_game_kyock)^2 #0.939
#-------------------------------------------------------------------------------------------

#Cross validation with 10 folds
CV_game <-CVlm(data = GamingAccessory_kyock, form.lm = kyock_game_21, m=10, 
               dots = FALSE, seed=50, plotit="Residual", printit=TRUE)
#Overall Mean Square Error
attr(CV_game,"ms") #1.58e+10

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(GamingAccessory_kyock,kyock_game_21)



######### Distributed Lag Model ########
########################################

### Creating lag variables
GamingAccessory_dlm<-GamingAccessory_df

# Lag variables for Adstock (2 lags)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Total_Investment_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Total_Investment_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "TV_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "TV_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Digital_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Digital_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Sponsorship_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Sponsorship_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Content_Marketing_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Content_Marketing_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Online_marketing_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Online_marketing_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Affiliates_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Affiliates_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "SEM_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "SEM_weekly_adstock",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Radio_weekly_adstock",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "Radio_weekly_adstock",slideBy = -2)

# Lag variables for Pricing features
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "mrp",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "mrp",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "list_price",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "list_price",slideBy = -2)

GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "discount",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "discount",slideBy = -2)

# Lag variables for gmv (2lags)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "gmv",slideBy = -1)
GamingAccessory_dlm<-slide(GamingAccessory_dlm,Var = "gmv",slideBy = -2)

colnames(GamingAccessory_dlm)[47:72] <- c('Total_Investment_weekly_adstock_1',
                                          'Total_Investment_weekly_adstock_2',
                                          'TV_weekly_adstock_1','TV_weekly_adstock_2',
                                          'Digital_weekly_adstock_1','Digital_weekly_adstock_2',
                                          'Sponsorship_weekly_adstock_1','Sponsorship_weekly_adstock_2',
                                          'Content_Marketing_weekly_adstock_1',
                                          'Content_Marketing_weekly_adstock_2',
                                          'Online_marketing_weekly_adstock_1',
                                          'Online_marketing_weekly_adstock_2',
                                          'Affiliates_weekly_adstock_1',
                                          'Affiliates_weekly_adstock_2','SEM_weekly_adstock_1',
                                          'SEM_weekly_adstock_2','Radio_weekly_adstock_1',
                                          'Radio_weekly_adstock_2','mrp_1','mrp_2','list_price_1',
                                          'list_price_2','discount_1','discount_2','gmv_1','gmv_2')
colnames(GamingAccessory_dlm)

GamingAccessory_dlm_lag <- na.omit(GamingAccessory_dlm)
str(GamingAccessory_dlm_lag)

# Train : First 40 weeks, Test: Last 12 Weeks
GamingAccessory_dlm_test<-GamingAccessory_dlm_lag[which(GamingAccessory_dlm_lag$week_number>=41),]
GamingAccessory_dlm<-GamingAccessory_dlm_lag[which(GamingAccessory_dlm_lag$week_number<41),]

#Scaling only train data and not scaling gmv
str(GamingAccessory_dlm)
GamingAccessory_dlm[,-c(25,seq(31,46))]<-scale(GamingAccessory_dlm[,-c(25,seq(31,46))])

#-------------------------------------------------------------------------------------------

#set seed
set.seed(100)

#Initial model with all the dependent and independent variables
model_dlm<-lm(gmv~. ,data = GamingAccessory_dlm)
summary(model_dlm)

#using stepAIC to remove insignificant variables
step_game_dlm <- stepAIC(model_dlm, direction="both")
step_game_dlm

#Model 1
dlm_game_1<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + sla + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + 
                 Content_Marketing_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1 + gmv_2, 
               data = GamingAccessory_dlm)
summary(dlm_game_1)#0.995
vif(dlm_game_1)


#Model 2
#removing gmv_2 column as it has high p value
dlm_game_2<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + sla + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + 
                 Content_Marketing_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, 
               data = GamingAccessory_dlm)
summary(dlm_game_2)#0.995
vif(dlm_game_2)

#Model 3
#removing Total_Investment_weekly column as it has high vif
dlm_game_3<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + sla + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + 
                 Content_Marketing_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, 
               data = GamingAccessory_dlm)
summary(dlm_game_3)#0.992
vif(dlm_game_3)

#Model 4
#removing Content_Marketing_weekly_adstock column as it has high p value
dlm_game_4<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + 
                 Online_marketing_weekly_adstock + Affiliates_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + sla + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + 
                 Content_Marketing_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, 
               data = GamingAccessory_dlm)
summary(dlm_game_4)#0.992
vif(dlm_game_4)

#Model 5
#removing Affiliates_weekly_adstock column as it has high P
dlm_game_5<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + sla + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + 
                 Content_Marketing_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, 
               data = GamingAccessory_dlm)
summary(dlm_game_5)#0.992
vif(dlm_game_5)

#Model 6
#removing sla column as it has high vif
dlm_game_6<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + 
                 Content_Marketing_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, 
               data = GamingAccessory_dlm)
summary(dlm_game_6)#0.992
vif(dlm_game_6)

#Model 7
#removing SEM_weekly_adstock_2 column as it has high P
dlm_game_7<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + 
                 Content_Marketing_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_7)#0.992
vif(dlm_game_7)

#Model 8
#removing Content_Marketing_weekly_adstock_2 column as it has high vif
dlm_game_8<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + product_procurement_sla + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_8)#0.992
vif(dlm_game_8)

#Model 9
#removing product_procurement_sla column as it has high vif
dlm_game_9<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                 Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                 Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + 
                 SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                 deliverycdays + special_sale_calendar + 
                 Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                 Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                 Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_9)#0.992
vif(dlm_game_9)

#Model 10
#removing Total_Investment_weekly_adstock column as it has high vif
dlm_game_10<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  TV_weekly_adstock + Digital_weekly_adstock + 
                  Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_10)#0.992
vif(dlm_game_10)

#Model 11
#removing Online_marketing_weekly_adstock column as it has high vif
dlm_game_11<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_11)#0.992
vif(dlm_game_11)

#Model 12
#removing TV_weekly_adstock column as it has high vif
dlm_game_12<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_12)#0.992
vif(dlm_game_12)

#Model 13
#removing Digital_weekly_adstock column as it has high P
dlm_game_13<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Sponsorship_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_13)#0.992
vif(dlm_game_13)

#Model 14
#removing Digital_weekly column as it has high vif
dlm_game_14<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Sponsorship_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_14)#0.988
vif(dlm_game_14)

#Model 15
#removing SEM_weekly_adstock column as it has high P
dlm_game_15<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Sponsorship_weekly_adstock + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_15)#0.988
vif(dlm_game_15)

#Model 16
#removing Sponsorship_weekly_adstock column as it has high P
dlm_game_16<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Total_Investment_weekly_adstock_2 + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_16)#0.988
vif(dlm_game_16)

#Model 17
#removing Total_Investment_weekly_adstock_2 column as it has high vif
dlm_game_17<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + TV_weekly_adstock_2 + 
                  Digital_weekly_adstock_2 + Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_17)#0.987
vif(dlm_game_17)

#Model 18
#removing Digital_weekly_adstock_2 column as it has high P
dlm_game_18<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + TV_weekly_adstock_2 + 
                  Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_18)#0.987
vif(dlm_game_18)

#Model 19
#removing TV_weekly_adstock_2 column as it has high P
dlm_game_19<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  Radio_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_19)#0.987
vif(dlm_game_19)

#Model 20
#removing Radio_weekly_adstock_2 column as it has high P
dlm_game_20<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + SEM_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_20)#0.987
vif(dlm_game_20)

#Model 21
#removing SEM_weekly column as it has high vif
dlm_game_21<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                  gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_21)#0.985
vif(dlm_game_21)

#Model 22
#removing Online_marketing_weekly_adstock_2 column as it has high P
dlm_game_22<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverybdays + 
                  deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_22)#0.985
vif(dlm_game_22)

#Model 23
#removing deliverybdays column as it has high vif
dlm_game_23<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                  Radio_weekly_adstock + units + deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_23)#0.982
vif(dlm_game_23)

#Model 24
#removing Radio_weekly_adstock column as it has high P
dlm_game_24<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Content_Marketing_weekly + 
                  Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                  units + deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_24)#0.982
vif(dlm_game_24)

#Model 25
#removing Content_Marketing_weekly column as it has high P and vif
dlm_game_25<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Affiliates_weekly + Radio_weekly + NPS_Score_weekly + 
                  units + deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_25)#0.982
vif(dlm_game_25)

#Model 26
#removing NPS_Score_weekly column as it has high vif
dlm_game_26<-lm(formula = gmv ~ week_number + TV_weekly + 
                  Sponsorship_weekly + Affiliates_weekly + Radio_weekly + 
                  units + deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_26)#0.981
vif(dlm_game_26)


#Model 27
#removing TV_weekly column as it has high vif and P
dlm_game_27<-lm(formula = gmv ~ week_number + 
                  Sponsorship_weekly + Affiliates_weekly + Radio_weekly + 
                  units + deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_27)#0.98
vif(dlm_game_27)

#Model 28
#removing Sponsorship_weekly column as it has high vif
dlm_game_28<-lm(formula = gmv ~ week_number + 
                  Affiliates_weekly + Radio_weekly + 
                  units + deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2 + gmv_1, data = GamingAccessory_dlm)
summary(dlm_game_28)#0.977
vif(dlm_game_28)

#Model 29
#removing gmv_1 column as it has high vif
dlm_game_29<-lm(formula = gmv ~ week_number + 
                  Affiliates_weekly + Radio_weekly + 
                  units + deliverycdays + special_sale_calendar + 
                  Sponsorship_weekly_adstock_2, data = GamingAccessory_dlm)
summary(dlm_game_29)#0.974
vif(dlm_game_29)

#Model 30
#removing special_sale_calendar column as it has high vif
dlm_game_30<-lm(formula = gmv ~ week_number + 
                  Affiliates_weekly + Radio_weekly + 
                  units + deliverycdays + 
                  Sponsorship_weekly_adstock_2, data = GamingAccessory_dlm)
summary(dlm_game_30)#0.973
vif(dlm_game_30)

#Model 31
#removing deliverycdays column as it has high P
dlm_game_31<-lm(formula = gmv ~ week_number + 
                  Affiliates_weekly + Radio_weekly + 
                  units + Sponsorship_weekly_adstock_2, data = GamingAccessory_dlm)
summary(dlm_game_31)#0.972
vif(dlm_game_31)

#Final model is dlm_game_31
# Adjusted R-squared:  0.972
# signicant variables : week_number,Affiliates_weekly,Radio_weekly, units , Sponsorship_weekly_adstock_2

#-------------------------------------------------------------------------------------------

#Test the model: Scaling
GamingAccessory_dlm_test[,-c(25,seq(31,46))]<-scale(GamingAccessory_dlm_test[,-c(25,seq(31,46))])
actual_game_dlm <- GamingAccessory_dlm_test$gmv
predicted_game_dlm <- predict(dlm_game_31,newdata = GamingAccessory_dlm_test[-25])

#Scatter plot
plot(actual_game_dlm, predicted_game_dlm, main="Correlation between Predicted and Actual Values", 
     xlab="actual", ylab="predicted")

#Correlation & R^2 between actual gmv and predicted gmv
cor(actual_game_dlm,predicted_game_dlm) #0.967
cor(actual_game_dlm,predicted_game_dlm)^2 #0.935

#-------------------------------------------------------------------------------------------

#Cross validation with 10 folds
CV_game <-CVlm(data = GamingAccessory_dlm, form.lm = dlm_game_31, m=10, 
               dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

#Overall Mean Square Error
attr(CV_game,"ms") #1.31e+10

#-------------------------------------------------------------------------------------------

# Calculating the elasticity of each variable
elas(GamingAccessory_dlm_lag,dlm_game_31)


####### Combination of Multiplicative and Distributed Lag Model #######
#######################################################################

# Train : First 40 weeks, Test: Last 12 Weeks
GamingAccessory_com_test<-GamingAccessory_dlm_lag[which(GamingAccessory_dlm_lag$week_number>=41),]
GamingAccessory_com<-GamingAccessory_dlm_lag[which(GamingAccessory_dlm_lag$week_number<41),]

GamingAccessory_com[GamingAccessory_com<=0] <- 0.001
#converting into logs
GamingAccessory_com<-log(GamingAccessory_com)

#-------------------------------------------------------------------------------------------

#set seed
set.seed(100)

#Initial model with all the dependent and independent variables
model_com<-lm(gmv~. ,data = GamingAccessory_com)
summary(model_com)

#using stepAIC to remove insignificant variables
step_com <- stepAIC(model_com, direction="both")
step_com


#Model 1
com_game_1<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                 Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar + product_verticalGameControlMount + 
                 Total_Investment_weekly_adstock_2 + Affiliates_weekly_adstock_2, 
               data = GamingAccessory_com)
summary(com_game_1)#1
vif(com_game_1)

#Model 2
#removing Affiliates_weekly_adstock_2 as it has high vif and high p value
com_game_2<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                 Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar + Total_Investment_weekly_adstock_2, 
               data = GamingAccessory_com)
summary(com_game_2)#1
vif(com_game_2)

#Model 3
#removing Total_Investment_weekly_adstock_2 as it has high vif and high p value
com_game_3<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                 Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar, data = GamingAccessory_com)
summary(com_game_3)#1
vif(com_game_3)

#Model 4
#removing Affiliates_weekly as it has high vif 
com_game_4<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + SEM_weekly + 
                 Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar, data = GamingAccessory_com)
summary(com_game_4)#1
vif(com_game_4)

#Model 5
#removing Digital_weekly_adstock as it has high p value
com_game_5<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + SEM_weekly + 
                 Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar, data = GamingAccessory_com)
summary(com_game_5)#1
vif(com_game_5)

#Model 6
#removing NPS_Score_weekly as it has high p value
com_game_6<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + SEM_weekly + 
                 Radio_weekly + Other_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar, data = GamingAccessory_com)
summary(com_game_6)#1
vif(com_game_6)

#Model 7
#removing Other_weekly as it has high vif and high p value
com_game_7<-lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + SEM_weekly + 
                 Radio_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar, data = GamingAccessory_com)
summary(com_game_7)#1
vif(com_game_7)

#Model 8
#removing Total_Investment_weekly as it has high p value
com_game_8<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + SEM_weekly + 
                 Radio_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla + 
                 special_sale_calendar, data = GamingAccessory_com)
summary(com_game_8)#1
vif(com_game_8)

#Model 9
#removing special_sale_calendar as it has high p value
com_game_9<-lm(formula = gmv ~ week_number + TV_weekly + 
                 Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                 Online_marketing_weekly + SEM_weekly + 
                 Radio_weekly + Total_Investment_weekly_adstock + 
                 TV_weekly_adstock + Sponsorship_weekly_adstock + 
                 Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                 Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                 units + deliverybdays + deliverycdays + sla + product_procurement_sla
               , data = GamingAccessory_com)
summary(com_game_9)#1
vif(com_game_9)

#Model 10
#removing TV_weekly as it has high vif and high vif
com_game_10<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  Online_marketing_weekly + SEM_weekly + 
                  Radio_weekly + Total_Investment_weekly_adstock + 
                  TV_weekly_adstock + Sponsorship_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_10)#1
vif(com_game_10)

#Model 11
#removing Online_marketing_weekly as it has high p value
com_game_11<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  SEM_weekly + Radio_weekly + Total_Investment_weekly_adstock + 
                  TV_weekly_adstock + Sponsorship_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_11)#1
vif(com_game_11)

#Model 12
#removing deliverybdays as it has high p value
com_game_12<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  SEM_weekly + Radio_weekly + Total_Investment_weekly_adstock + 
                  TV_weekly_adstock + Sponsorship_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_12)#1
vif(com_game_12)

#Model 13
#removing Affiliates_weekly_adstock as it has high vif 
com_game_13<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  SEM_weekly + Radio_weekly + Total_Investment_weekly_adstock + 
                  TV_weekly_adstock + Sponsorship_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_13)#1
vif(com_game_13)

#Model 14
#removing TV_weekly_adstock as it has high p value
com_game_14<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                  SEM_weekly + Radio_weekly + Total_Investment_weekly_adstock + 
                  Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_14)#1
vif(com_game_14)

#Model 15
#removing Content_Marketing_weekly as it has high p value
com_game_15<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + 
                  SEM_weekly + Radio_weekly + Total_Investment_weekly_adstock + 
                  Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_15)#1
vif(com_game_15)

#Model 16
#removing Total_Investment_weekly_adstock as it has high vif and high p value
com_game_16<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + 
                  SEM_weekly + Radio_weekly + 
                  Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_16)#1
vif(com_game_16)

#Model 17
#removing SEM_weekly as it has high vif 
com_game_17<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + 
                  Radio_weekly + Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_17) #1
vif(com_game_17)

#Model 18
#removing Content_Marketing_weekly_adstock as it has high p value
com_game_18<-lm(formula = gmv ~ week_number + Digital_weekly + Sponsorship_weekly + 
                  Radio_weekly + Sponsorship_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_18)#1
vif(com_game_18)

#Model 19
#removing week_number as it has high p value
com_game_19<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + 
                  Radio_weekly + Sponsorship_weekly_adstock + 
                  Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_19)#0.99
vif(com_game_19)

#Model 20
#removing Radio_weekly_adstock as it has high p value
com_game_20<- lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + 
                   Radio_weekly + Sponsorship_weekly_adstock + 
                   Online_marketing_weekly_adstock + SEM_weekly_adstock + 
                   units + deliverycdays + sla + product_procurement_sla
                 , data = GamingAccessory_com)
summary(com_game_20) #1
vif(com_game_20)

#Model 21
#removing SEM_weekly_adstock as it has high vif
com_game_21<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + 
                  Radio_weekly + Sponsorship_weekly_adstock + Online_marketing_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_21)#0.999
vif(com_game_21)

#Model 22
#removing Sponsorship_weekly_adstock as it has high vif and high p value
com_game_22<-lm(formula = gmv ~ Digital_weekly + Sponsorship_weekly + 
                  Radio_weekly + Online_marketing_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_22)#0.99
vif(com_game_22)

#Model 23
#removing Digital_weekly as it has high p value
com_game_23<-lm(formula = gmv ~ Sponsorship_weekly + Radio_weekly + Online_marketing_weekly_adstock + 
                  units + deliverycdays + sla + product_procurement_sla
                , data = GamingAccessory_com)
summary(com_game_23)#0.99
vif(com_game_23)

#Model 24
#removing sla as it has high vif and high p value
com_game_24<-lm(formula = gmv ~ Sponsorship_weekly + Radio_weekly + Online_marketing_weekly_adstock + 
                  units + deliverycdays + product_procurement_sla, data = GamingAccessory_com)
summary(com_game_24)#0.999
vif(com_game_24)

#Model 25
#removing Sponsorship_weekly as it has high vif
com_game_25<-lm(formula = gmv ~ Radio_weekly + Online_marketing_weekly_adstock + 
                  units + deliverycdays + product_procurement_sla, data = GamingAccessory_com)
summary(com_game_25)#0.999
vif(com_game_25)

#Model 26
#removing Radio_weekly as it has high p value
com_game_26<-lm(formula = gmv ~ Online_marketing_weekly_adstock + 
                  units + deliverycdays + product_procurement_sla, data = GamingAccessory_com)
summary(com_game_26)#0.99
vif(com_game_26)

#Model 27
#removing product_procurement_sla as it has high vif
com_game_27<-lm(formula = gmv ~ Online_marketing_weekly_adstock + 
                  units + deliverycdays, data = GamingAccessory_com)
summary(com_game_27)#0.989
vif(com_game_27)

#Final model is dlm_game_27
# Adjusted R-squared:  0.989
# signicant variables : Online_marketing_weekly_adstock,units,deliverycdays

#-------------------------------------------------------------------------------------------

#Test the model:
GamingAccessory_com_test[GamingAccessory_com_test<=0] <- 0.001

#converting into logs
GamingAccessory_com_test<-log(GamingAccessory_com_test)

#Scatter plot
actual_game_com <- GamingAccessory_com_test$gmv
predicted_game_com <- predict(com_game_27,newdata = GamingAccessory_com_test[-25])

plot(actual_game_com, predicted_game_com, main="Correlation between Predicted and Actual Values", 
     xlab="actual ", ylab="predicted")

#Correlation & R^2 between actual gmv and predicted gmv
cor(actual_game_com,predicted_game_com) #0.989
cor(actual_game_com,predicted_game_com)^2 #0.979

#-------------------------------------------------------------------------------------------
#Cross validation with 10 folds
CV_game <-CVlm(data = GamingAccessory_dlm, form.lm = com_game_27, m=10,dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

#Overall Mean Square Error
attr(CV_game,"ms") #2.93e+10

#-------------------------------------------------------------------------------------------
# Calculating the elasticity of each variable
elas(GamingAccessory_df,com_game_27)


############################################################################################################
#####################################  Home Audio Model Building  ##########################################
############################################################################################################


###### Linear model #########
#############################

HomeAudio_linear <- HomeAudio_df
HomeAudio_linear[,-25]<-scale(HomeAudio_linear[,-25]) 

# Divide data in 70:30 
set.seed(100)
indices= sample(1:nrow(HomeAudio_linear), 0.7*nrow(HomeAudio_linear))

train_home_ln=HomeAudio_linear[indices,]
test_home_ln = HomeAudio_linear[-indices,]

# Develop the first model 
home_linear1 <-lm(gmv~.,data=train_home_ln)
summary(home_linear1)

#-------------------------------------------------------------------------------------------
# Apply the stepwise approach

step_home_ln <- stepAIC(home_linear1, direction="both")
step_home_ln

#-------------------------------------------------------------------------------------------
#home_linear2
home_linear2 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                     TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + sla + product_procurement_sla, data = train_home_ln)
summary(home_linear2)
vif(home_linear2)

#Affiliates_weekly_adstock which is insignificant and has high VIF. Thus, removing
home_linear3 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                     TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + sla + product_procurement_sla, data = train_home_ln)
summary(home_linear3)
vif(home_linear3)

#product_procurement_sla which is insignificant and has high VIF. Thus, removing
home_linear4 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                     TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + sla, data = train_home_ln)
summary(home_linear4)
vif(home_linear4)

#Other_weekly which has high VIF. Thus, removing
home_linear5 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + 
                     TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + sla, data = train_home_ln)
summary(home_linear5)
vif(home_linear5)

#SEM_weekly_adstock which is insignificant and has high VIF. Thus, removing
home_linear6 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     Total_Investment_weekly_adstock + 
                     TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Radio_weekly_adstock + NPS_Score_weekly +
                     units + sla, data = train_home_ln)
summary(home_linear6)
vif(home_linear6)

# week_number is insignicant and has high VIF, removing
home_linear7 <- lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     Total_Investment_weekly_adstock + 
                     TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Radio_weekly_adstock + NPS_Score_weekly +
                     units + sla, data = train_home_ln)
summary(home_linear7)
vif(home_linear7)

#Content_Marketing_weekly_adstock is insignicant
home_linear8 <- lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     Total_Investment_weekly_adstock + 
                     TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Online_marketing_weekly_adstock + 
                     Radio_weekly_adstock + NPS_Score_weekly +
                     units + sla, data = train_home_ln)
summary(home_linear8)
vif(home_linear8)

#TV_weekly_adstock which is insignicant
home_linear9 <- lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                     Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                     Total_Investment_weekly_adstock + 
                     Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                     Online_marketing_weekly_adstock + 
                     Radio_weekly_adstock + NPS_Score_weekly +
                     units + sla, data = train_home_ln)
summary(home_linear9)
vif(home_linear9)

# Sponsorship_weekly_adstock which is insignicant and has high VIF
home_linear10 <- lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                      Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                      Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                      Total_Investment_weekly_adstock + 
                      Digital_weekly_adstock +  
                      Online_marketing_weekly_adstock + 
                      Radio_weekly_adstock + NPS_Score_weekly +
                      units + sla, data = train_home_ln)
summary(home_linear10)
vif(home_linear10)

# Radio_weekly_adstock is insignicant and has high VIF
home_linear11 <- lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                      Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                      Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                      Total_Investment_weekly_adstock + 
                      Digital_weekly_adstock +  
                      Online_marketing_weekly_adstock + 
                      NPS_Score_weekly +
                      units + sla, data = train_home_ln)
summary(home_linear11)
vif(home_linear11)

#Total_Investment_weekly which is having very high VIF
home_linear12 <- lm(formula = gmv ~ TV_weekly + 
                      Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                      Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                      Total_Investment_weekly_adstock + 
                      Digital_weekly_adstock +  
                      Online_marketing_weekly_adstock + 
                      NPS_Score_weekly +
                      units + sla, data = train_home_ln)
summary(home_linear12)
vif(home_linear12)

# Digital_weekly is insignicant and has high VIF
home_linear13 <- lm(formula = gmv ~ TV_weekly + 
                      Sponsorship_weekly + Content_Marketing_weekly + 
                      Online_marketing_weekly + Affiliates_weekly + Radio_weekly + 
                      Total_Investment_weekly_adstock + 
                      Digital_weekly_adstock +  
                      Online_marketing_weekly_adstock + 
                      NPS_Score_weekly +
                      units + sla, data = train_home_ln)
summary(home_linear13)
vif(home_linear13)

#Online_marketing_weekly is insignicant and has high VIF
home_linear14 <- lm(formula = gmv ~ TV_weekly + 
                      Sponsorship_weekly + Content_Marketing_weekly + 
                      Affiliates_weekly + Radio_weekly + 
                      Total_Investment_weekly_adstock + 
                      Digital_weekly_adstock +  
                      Online_marketing_weekly_adstock + 
                      NPS_Score_weekly +
                      units + sla, data = train_home_ln)
summary(home_linear14)
vif(home_linear14)


# Total_Investment_weekly_adstock  has high VIF
home_linear15 <- lm(formula = gmv ~ TV_weekly + 
                      Sponsorship_weekly + Content_Marketing_weekly + 
                      Affiliates_weekly + Radio_weekly +  
                      Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                      NPS_Score_weekly + units + sla, data = train_home_ln)
summary(home_linear15)
vif(home_linear15)



# TV_weekly which is insignicant and high VIF
home_linear16 <- lm(formula = gmv ~  Sponsorship_weekly + Content_Marketing_weekly + 
                      Affiliates_weekly + Radio_weekly +  
                      Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                      NPS_Score_weekly + units + sla, data = train_home_ln)
summary(home_linear16)
vif(home_linear16)

#sla is insignicant
home_linear17 <- lm(formula = gmv ~  Sponsorship_weekly + Content_Marketing_weekly + 
                      Affiliates_weekly + Radio_weekly +  
                      Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                      NPS_Score_weekly + units, data = train_home_ln)
summary(home_linear17)
vif(home_linear17)


# Affiliates_weekly is insginicant and has VIF
home_linear18 <- lm(formula = gmv ~  Sponsorship_weekly + Content_Marketing_weekly + 
                      Radio_weekly + Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                      NPS_Score_weekly + units, data = train_home_ln)
summary(home_linear18)
vif(home_linear18)

# Sponsorship_weekly has high VIF
home_linear19 <- lm(formula = gmv ~ Content_Marketing_weekly + 
                      Radio_weekly + Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                      NPS_Score_weekly + units, data = train_home_ln)
summary(home_linear19)
vif(home_linear19)

# Radio_weekly is insignicant
home_linear20 <- lm(formula = gmv ~ Content_Marketing_weekly + 
                      Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                      NPS_Score_weekly + units, data = train_home_ln)
summary(home_linear20)
vif(home_linear20)

# NPS_Score_weekly  which is insignicant
home_linear21 <- lm(formula = gmv ~ Content_Marketing_weekly + 
                      Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                      units, data = train_home_ln)
summary(home_linear21)
vif(home_linear21)

#Digital_weekly_adstock is insignicant
home_linear22 <- lm(formula = gmv ~ Content_Marketing_weekly + 
                      Online_marketing_weekly_adstock + 
                      units, data = train_home_ln)
summary(home_linear22)
vif(home_linear22)

#Online_marketing_weekly_adstock is insginicant
home_linear23 <- lm(formula = gmv ~ Content_Marketing_weekly + 
                      units, data = train_home_ln)
summary(home_linear23)
vif(home_linear23)

#-------------------------------------------------------------------------------------------

# Adjusted R-squared:  0.9728
# signicant variables : Content_Marketing_weekly, units

# We have 2 variables in the final model
# test_home_ln the model on test_home_ln dataset
Predict_homeaudio_ln <- predict(home_linear23,test_home_ln[-test_home_ln$gmv])

# Add a new column "test_home_ln_predict" into the test_home_ln dataset
test_home_ln$test_gmv <- Predict_homeaudio_ln

#-------------------------------------------------------------------------------------------

# calculate the test_home_ln R2 

cor(test_home_ln$gmv,test_home_ln$test_gmv)
cor(test_home_ln$gmv,test_home_ln$test_gmv)^2 #0.987

#Scatter plot
plot(test_home_ln$gmv, test_home_ln$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="actual", ylab="predicted")
#--------------------------------------------------------------------------------------------
#Cross validation

#Cross validation with 5 folds
CV_Homeaudio_linear<-cv.lm(data = HomeAudio_linear, form.lm = home_linear23, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

attr(CV_Homeaudio_linear,"ms") 
# mean squre : 3.15e+10

#--------------------------------------------------------------------------------------------

#### Calculating Elasticity
elas(HomeAudio_linear ,home_linear23) 



######## Multiplicative model #############
##############################################

HomeAudio_mul <- HomeAudio_df

#changing the 0s and -ve values to >0 values
HomeAudio_mul[HomeAudio_mul<=0]<-0.001 

#converting to log values
HomeAudio_mul <- log(HomeAudio_mul)

# Divide data in 70:30 
set.seed(100)
indices_mul= sample(1:nrow(HomeAudio_mul), 0.7*nrow(HomeAudio_mul))
train_home_mul=HomeAudio_mul[indices_mul,]
test_home_mul = HomeAudio_mul[-indices_mul,]

# Develop the first home_mul 
home_mul_1 <-lm(gmv~.,data=train_home_mul)
summary(home_mul_1)


#-------------------------------------------------------------------------------------------

# Apply the stepwise approach
step_home_mul <- stepAIC(home_mul_1, direction="both")
step_home_mul

#---------------------------------------------------------------------
#home_mul_2
home_mul_2 <-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Content_Marketing_weekly + Online_marketing_weekly + Affiliates_weekly + 
                  SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  discount + units + deliverybdays + deliverycdays + sla + 
                  product_procurement_sla + product_verticalSlingBox, data = train_home_mul)
summary(home_mul_2)
vif(home_mul_2)

# discount which is insignicant 
home_mul_3 <-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Content_Marketing_weekly + Online_marketing_weekly + Affiliates_weekly + 
                  SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + 
                  product_procurement_sla + product_verticalSlingBox, data = train_home_mul)
summary(home_mul_3)
vif(home_mul_3)

# product_verticalSlingBox which is insignicant
home_mul_4 <-lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + Digital_weekly + 
                  Content_Marketing_weekly + Online_marketing_weekly + Affiliates_weekly + 
                  SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + 
                  product_procurement_sla, data = train_home_mul)
summary(home_mul_4)
vif(home_mul_4)

# TV_weekly which is insignicant
home_mul_5 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                  Content_Marketing_weekly + Online_marketing_weekly + Affiliates_weekly + 
                  SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + 
                  product_procurement_sla, data = train_home_mul)
summary(home_mul_5)
vif(home_mul_5)

# Radio_weekly which is insignicant
home_mul_6 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                  Content_Marketing_weekly + Online_marketing_weekly + Affiliates_weekly + 
                  SEM_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverybdays + deliverycdays + sla + 
                  product_procurement_sla, data = train_home_mul)
summary(home_mul_6)
vif(home_mul_6)

# deliverybdays which is insignicant and has high VIF
home_mul_7 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                  Content_Marketing_weekly + Online_marketing_weekly + Affiliates_weekly + 
                  SEM_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + 
                  product_procurement_sla, data = train_home_mul)
summary(home_mul_7)
vif(home_mul_7)

# Online_marketing_weekly which is insignicant and has highVIF
home_mul_8 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                  Content_Marketing_weekly  + Affiliates_weekly + 
                  SEM_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                  units + deliverycdays + sla + 
                  product_procurement_sla, data = train_home_mul)
summary(home_mul_8)
vif(home_mul_8)

#Radio_weekly_adstock which is insignicant 
home_mul_9 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                  Content_Marketing_weekly  + Affiliates_weekly + 
                  SEM_weekly + Other_weekly + NPS_Score_weekly + 
                  Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                  Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                  Affiliates_weekly_adstock + SEM_weekly_adstock + 
                  units + deliverycdays + sla + 
                  product_procurement_sla, data = train_home_mul)
summary(home_mul_9)
vif(home_mul_9)

# Other_weekly which is insignicant
home_mul_10 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                   Content_Marketing_weekly  + Affiliates_weekly + 
                   SEM_weekly + NPS_Score_weekly + 
                   Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                   Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   Affiliates_weekly_adstock + SEM_weekly_adstock + 
                   units + deliverycdays + sla + 
                   product_procurement_sla, data = train_home_mul)
summary(home_mul_10)
vif(home_mul_10)

# NPS_Score_weekly which is insignicant and has high VIF
home_mul_11 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                   Content_Marketing_weekly  + Affiliates_weekly + 
                   SEM_weekly +Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                   Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   Affiliates_weekly_adstock + SEM_weekly_adstock + 
                   units + deliverycdays + sla + 
                   product_procurement_sla, data = train_home_mul)
summary(home_mul_11)
vif(home_mul_11)

# Affiliates_weekly_adstock have high VIF
home_mul_12 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                   Content_Marketing_weekly  + Affiliates_weekly + 
                   SEM_weekly +Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                   Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverycdays + sla + 
                   product_procurement_sla, data = train_home_mul)
summary(home_mul_12)
vif(home_mul_12)

# Affiliates_weekly which have high VIF
home_mul_13 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                   Content_Marketing_weekly + 
                   SEM_weekly +Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                   Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverycdays + sla + 
                   product_procurement_sla, data = train_home_mul)
summary(home_mul_13)
vif(home_mul_13)

# Total_Investment_weekly_adstock which is insignicant
home_mul_14 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                   Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                   Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverycdays + sla + 
                   product_procurement_sla, data = train_home_mul)
summary(home_mul_14)
vif(home_mul_14)


# product_procurement_sla which is insignicant and has high VIF
home_mul_15 <-lm(formula = gmv ~ Total_Investment_weekly + Digital_weekly + 
                   Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                   Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverycdays + sla  , data = train_home_mul)
summary(home_mul_15)
vif(home_mul_15)


# Digital_weekly which is insignicant and has high VIF
home_mul_16 <-lm(formula = gmv ~ Total_Investment_weekly +  Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Digital_weekly_adstock + 
                   Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverycdays + sla  , data = train_home_mul)
summary(home_mul_16)
vif(home_mul_16)

# Digital_weekly_adstock which is insignicant
home_mul_17 <-lm(formula = gmv ~ Total_Investment_weekly +  Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + deliverycdays + sla  , data = train_home_mul)
summary(home_mul_17)
vif(home_mul_17)

# deliverycdays which is insginicant and has high VIF
home_mul_18 <-lm(formula = gmv ~ Total_Investment_weekly +  Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   SEM_weekly_adstock + units + sla  , data = train_home_mul)
summary(home_mul_18)
vif(home_mul_18)

# SEM_weekly_adstock which has high VIF
home_mul_19 <-lm(formula = gmv ~ Total_Investment_weekly +  Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                   units + sla  , data = train_home_mul)
summary(home_mul_19)
vif(home_mul_19)

# Online_marketing_weekly_adstock which is insignicant and has high VIF
home_mul_20 <-lm(formula = gmv ~ Total_Investment_weekly +  Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Content_Marketing_weekly_adstock + 
                   units + sla  , data = train_home_mul)
summary(home_mul_20)
vif(home_mul_20)

# Total_Investment_weekly which is insignicant
home_mul_21 <-lm(formula = gmv ~ Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + Content_Marketing_weekly_adstock + 
                   units + sla  , data = train_home_mul)
summary(home_mul_21)
vif(home_mul_21)

# Content_Marketing_weekly_adstock which has high VIF
home_mul_22 <-lm(formula = gmv ~ Content_Marketing_weekly + 
                   SEM_weekly + TV_weekly_adstock + units + sla  , data = train_home_mul)
summary(home_mul_22)
vif(home_mul_22)

# SEM_weekly which is insignicant
home_mul_23 <-lm(formula = gmv ~ Content_Marketing_weekly + TV_weekly_adstock + units + sla  , data = train_home_mul)
summary(home_mul_23)
vif(home_mul_23)

# TV_weekly_adstock which is insignicant
home_mul_24 <-lm(formula = gmv ~ Content_Marketing_weekly + units + sla  , data = train_home_mul)
summary(home_mul_24)
vif(home_mul_24)

# sla which is insignicant
home_mul_25 <-lm(formula = gmv ~ Content_Marketing_weekly + units  , data = train_home_mul)
summary(home_mul_25)
vif(home_mul_25)

final_mul_linear_model <- home_mul_25

#--------------------------------------------------------------------

summary(final_mul_linear_model)

# Adjusted R-squared:  0.985
# signicant variables : Content_Marketing_weekly, units

#----------------------------------------------------------------------

# test the model on test_home_ln dataset
Predict_homeaudio_mul <- predict(final_mul_linear_model,test_home_mul[-test_home_mul$gmv])

# Add a new column "test_home_mul_predict" into the test_home_mul dataset
test_home_mul$test_gmv <- Predict_homeaudio_mul

#-------------------------------------------------------------------------------------------

# calculate the test_home_mul R2 

cor(test_home_mul$gmv,test_home_mul$test_gmv) #0.993
cor(test_home_mul$gmv,test_home_mul$test_gmv)^2 #0.987

#Scatter plot
plot(test_home_mul$gmv, test_home_mul$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="actual", ylab="predicted")
#--------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_Homeaudio_mul_linear<-cv.lm(data = HomeAudio_mul, form.lm = final_mul_linear_model, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

attr(CV_Homeaudio_mul_linear,"ms") 
# mean square error : 0.00305

#--------------------------------------------------------------------------------------------

#### Calculating the elasticity
elas(HomeAudio_mul ,final_mul_linear_model) 


########## Kyock model ############
###################################

HomeAudio_Kyock <- HomeAudio_df

#Creating a lag variable for gmv
HomeAudio_Kyock<-slide(HomeAudio_Kyock,Var = "gmv",slideBy = -1)
HomeAudio_Kyock$lag_gmv <- HomeAudio_Kyock$`gmv-1`
HomeAudio_Kyock$`gmv-1` <- NULL

#removing NA from the lag variable
HomeAudio_Kyock <- na.omit(HomeAudio_Kyock)

#Scaling the data
HomeAudio_Kyock[,-25]<-scale(HomeAudio_Kyock[,-25])

# Divide data in 70:30 
set.seed(100)
indices_koyck= sample(1:nrow(HomeAudio_Kyock), 0.7*nrow(HomeAudio_Kyock))
train_home_kyock=HomeAudio_Kyock[indices_koyck,]
test_home_kyock = HomeAudio_Kyock[-indices_koyck,]

# Develop the first model 
home_kyock_1 <-lm(gmv~.,data=train_home_kyock)
summary(home_kyock_1)


#-------------------------------------------------------------------------------------------

# Apply the stepwise approach

step_home_kyock <- stepAIC(home_kyock_1, direction="both")
step_home_kyock


#------------------------------------------------------------------------------
home_kyock_2 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + Affiliates_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     mrp + units + deliverycdays + sla + product_procurement_sla + 
                     product_verticalDJController + product_verticalKaraokePlayer + 
                     product_verticalVoiceRecorder + lag_gmv, data = train_home_kyock)
summary(home_kyock_2)
vif(home_kyock_2)

# product_verticalVoiceRecorder which is insignicant
home_kyock_3 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + Affiliates_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     mrp + units + deliverycdays + sla + product_procurement_sla + 
                     product_verticalDJController + product_verticalKaraokePlayer + 
                     lag_gmv, data = train_home_kyock)
summary(home_kyock_3)
vif(home_kyock_3)

# product_verticalDJController which is insignicant
home_kyock_4 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + Affiliates_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     mrp + units + deliverycdays + sla + product_procurement_sla + 
                     product_verticalKaraokePlayer + 
                     lag_gmv, data = train_home_kyock)
summary(home_kyock_4)
vif(home_kyock_4)

# mrp which is insignicant
home_kyock_5 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + Affiliates_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + deliverycdays + sla + product_procurement_sla + 
                     product_verticalKaraokePlayer + 
                     lag_gmv, data = train_home_kyock)
summary(home_kyock_5)
vif(home_kyock_5)

# product_verticalKaraokePlayer which is insignicant
home_kyock_6 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + Affiliates_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + deliverycdays + sla + product_procurement_sla +lag_gmv, data = train_home_kyock)
summary(home_kyock_6)
vif(home_kyock_6)

# deliverycdays which is insignicant 
home_kyock_7 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + Affiliates_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + sla + product_procurement_sla +lag_gmv, data = train_home_kyock)
summary(home_kyock_7)
vif(home_kyock_7)

# Affiliates_weekly which is insignicant 
home_kyock_8 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + sla + product_procurement_sla +lag_gmv, data = train_home_kyock)
summary(home_kyock_8)
vif(home_kyock_8)

# Total_Investment_weekly which has high VIF
home_kyock_9 <- lm(formula = gmv ~ week_number + TV_weekly + 
                     Sponsorship_weekly + Online_marketing_weekly + 
                     SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                     Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                     Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                     Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                     units + sla + product_procurement_sla +lag_gmv, data = train_home_kyock)
summary(home_kyock_9)
vif(home_kyock_9)

# product_procurement_sla which is insignificant
home_kyock_10 <- lm(formula = gmv ~ week_number + TV_weekly + 
                      Sponsorship_weekly + Online_marketing_weekly + 
                      SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                      Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_10)
vif(home_kyock_10)

# Sponsorship_weekly which is insiginificant
home_kyock_11 <- lm(formula = gmv ~ week_number + TV_weekly + 
                      Online_marketing_weekly + 
                      SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                      Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_11)
vif(home_kyock_11)

# Online_marketing_weekly which is insignicant 
home_kyock_12 <- lm(formula = gmv ~ week_number + TV_weekly + 
                      SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                      Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_12)
vif(home_kyock_12)

# TV_weekly which is insignicant
home_kyock_13 <- lm(formula = gmv ~ week_number + SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                      Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_13)
vif(home_kyock_13)

# week_number which is insignicant and has high VIF
home_kyock_14 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + Content_Marketing_weekly_adstock + 
                      Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_14)
vif(home_kyock_14)

# Content_Marketing_weekly_adstock which is insignicant 
home_kyock_15 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock +Online_marketing_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_15)
vif(home_kyock_15)

# Online_marketing_weekly_adstock which is insignicant 
home_kyock_16 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock + TV_weekly_adstock + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_16)
vif(home_kyock_16)

# TV_weekly_adstock which is insignicant 
home_kyock_17 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock  + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_17)
vif(home_kyock_17)

# Radio_weekly_adstock which is insignicant
home_kyock_18 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Total_Investment_weekly_adstock  + Digital_weekly_adstock + 
                      Sponsorship_weekly_adstock + SEM_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_18)
vif(home_kyock_18)

# Total_Investment_weekly_adstock which is insignicant
home_kyock_19 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Digital_weekly_adstock +Sponsorship_weekly_adstock + SEM_weekly_adstock + 
                      units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_19)
vif(home_kyock_19)

# SEM_weekly_adstock which has high VIF
home_kyock_20 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + NPS_Score_weekly + 
                      Digital_weekly_adstock +Sponsorship_weekly_adstock +units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_20)
vif(home_kyock_20)

# NPS_Score_weekly which is insignicant 
home_kyock_21 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly + Other_weekly + 
                      Digital_weekly_adstock +Sponsorship_weekly_adstock +units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_21)
vif(home_kyock_21)

# Other_weekly which is insignicant 
home_kyock_22 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly +Digital_weekly_adstock 
                    +Sponsorship_weekly_adstock +units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_22)
vif(home_kyock_22)

# Radio_weekly which is insignicant 
home_kyock_23 <- lm(formula = gmv ~ SEM_weekly +Digital_weekly_adstock 
                    +Sponsorship_weekly_adstock +units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_23)
vif(home_kyock_23)

# Digital_weekly_adstock which is insignificant 
home_kyock_24 <- lm(formula = gmv ~ SEM_weekly+Sponsorship_weekly_adstock +units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_24)
vif(home_kyock_24)

# Sponsorship_weekly_adstock is insignificant 
home_kyock_25 <-  lm(formula = gmv ~ SEM_weekly+units + sla + lag_gmv, data = train_home_kyock)
summary(home_kyock_25)
vif(home_kyock_25)

# sla which is insignificant
home_kyock_26 <- lm(formula = gmv ~ SEM_weekly+units + lag_gmv, data = train_home_kyock)
summary(home_kyock_26)
vif(home_kyock_26)

final_kyock_model <- home_kyock_26

#--------------------------------------------------------------------

summary(final_kyock_model)

# Adjusted R-squared:  0.979
# signicant variables : SEM_weekly, units,lag_gmv

#----------------------------------------------------------------------

# test the model on test_home_kyock dataset
Predict_homeaudio_kyock <- predict(final_kyock_model,test_home_kyock[-test_home_kyock$gmv])

# Add a new column "test_gmv" into the test_home_kyock dataset
test_home_kyock$test_gmv <- Predict_homeaudio_kyock

#-------------------------------------------------------------------------------------------

# calculate the test_home_kyock R2 

cor(test_home_kyock$gmv,test_home_kyock$test_gmv) #0.992
cor(test_home_kyock$gmv,test_home_kyock$test_gmv)^2 #0.985

#Scatter plot
plot(test_home_kyock$gmv, test_home_kyock$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="actual", ylab="predicted")
#--------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_Homeaudio_kyock <-cv.lm(data = HomeAudio_Kyock, form.lm = final_kyock_model, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

attr(CV_Homeaudio_kyock,"ms") 
# mean squre : 2.92e+10

#--------------------------------------------------------------------------------------------

#### Deriving elasticity
elas(HomeAudio_Kyock ,final_kyock_model) 




######## Distributed lag  model ###########
###########################################

homeaudio_dis_lag <- HomeAudio_df

# Lag variables for Adstock (2 lags)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Total_Investment_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Total_Investment_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "TV_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "TV_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Digital_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Digital_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Sponsorship_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Sponsorship_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Content_Marketing_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Content_Marketing_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Online_marketing_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Online_marketing_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Affiliates_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Affiliates_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "SEM_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "SEM_weekly_adstock",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Radio_weekly_adstock",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "Radio_weekly_adstock",slideBy = -2)

# Lag variables for Pricing features
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "mrp",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "mrp",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "list_price",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "list_price",slideBy = -2)

homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "discount",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "discount",slideBy = -2)

# Lag variables for gmv (2lags)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "gmv",slideBy = -1)
homeaudio_dis_lag<-slide(homeaudio_dis_lag,Var = "gmv",slideBy = -2)

#renaming lag variables created
colnames(homeaudio_dis_lag)[42:67] <- c('Total_Investment_weekly_adstock_1',
                                        'Total_Investment_weekly_adstock_2',
                                        'TV_weekly_adstock_1','TV_weekly_adstock_2',
                                        'Digital_weekly_adstock_1','Digital_weekly_adstock_2',
                                        'Sponsorship_weekly_adstock_1','Sponsorship_weekly_adstock_2',
                                        'Content_Marketing_weekly_adstock_1',
                                        'Content_Marketing_weekly_adstock_2',
                                        'Online_marketing_weekly_adstock_1',
                                        'Online_marketing_weekly_adstock_2',
                                        'Affiliates_weekly_adstock_1',
                                        'Affiliates_weekly_adstock_2','SEM_weekly_adstock_1',
                                        'SEM_weekly_adstock_2','Radio_weekly_adstock_1',
                                        'Radio_weekly_adstock_2','mrp_1','mrp_2','list_price_1',
                                        'list_price_2','discount_1','discount_2','gmv_1','gmv_2')

#removing NAs from the lag variables
homeaudio_dis_lag <- na.omit(homeaudio_dis_lag)

#scaling
homeaudio_dis_lag[,-25]<-scale(homeaudio_dis_lag[,-25])

#modeling
# Divide data in 70:30 
set.seed(100)
indices_dis_lag= sample(1:nrow(homeaudio_dis_lag), 0.7*nrow(homeaudio_dis_lag))
train_dis_lag=homeaudio_dis_lag[indices_dis_lag,]
test_dis_lag = homeaudio_dis_lag[-indices_dis_lag,]

# Develop the first model 
home_dl_1 <-lm(gmv~.,data=train_dis_lag)
summary(home_dl_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach
step_home_dl <- stepAIC(home_dl_1, direction="both")
step_home_dl

#------------------------------------------------------------------------------
dis_lag_home_dl_2 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Digital_weekly + Sponsorship_weekly + Content_Marketing_weekly + 
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                          Radio_weekly_adstock_2 + mrp_1 + list_price_1 + discount_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_2)
vif(dis_lag_home_dl_2)

# Digital_weekly which is insignicant 
dis_lag_home_dl_3 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Sponsorship_weekly + Content_Marketing_weekly + 
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                          Radio_weekly_adstock_2 + mrp_1 + list_price_1 + discount_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_3)
vif(dis_lag_home_dl_3)

# Content_Marketing_weekly which is insignicant
dis_lag_home_dl_4 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Sponsorship_weekly +  
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          Affiliates_weekly_adstock + SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                          Radio_weekly_adstock_2 + mrp_1 + list_price_1 + discount_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_4)
vif(dis_lag_home_dl_4)

# Affiliates_weekly_adstock which is insignicant
dis_lag_home_dl_5 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Sponsorship_weekly +  
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                          Radio_weekly_adstock_2 + mrp_1 + list_price_1 + discount_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_5)
vif(dis_lag_home_dl_5)

# list_price_1 which is insignicant
dis_lag_home_dl_6 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Sponsorship_weekly +  
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                          Radio_weekly_adstock_2 + mrp_1 + discount_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_6)
vif(dis_lag_home_dl_6)

# discount_1 which is insignicant
dis_lag_home_dl_7 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Sponsorship_weekly +  
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 + Radio_weekly_adstock_1 + 
                          Radio_weekly_adstock_2 + mrp_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_7)
vif(dis_lag_home_dl_7)

# Radio_weekly_adstock_1 which is insignicant
dis_lag_home_dl_8 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Sponsorship_weekly +  
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_1 + Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                          Radio_weekly_adstock_2 + mrp_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_8)
vif(dis_lag_home_dl_8)

# Sponsorship_weekly_adstock_1 which is insignicant
dis_lag_home_dl_9 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                          Sponsorship_weekly +  
                          Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                          Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                          TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                          Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                          SEM_weekly_adstock + Radio_weekly_adstock + 
                          mrp + list_price + discount + units + deliverycdays + sla + 
                          product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                          product_verticalDock + Total_Investment_weekly_adstock_1 + 
                          Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                          TV_weekly_adstock_2 + Digital_weekly_adstock_1 + Digital_weekly_adstock_2 + 
                          Sponsorship_weekly_adstock_2 + 
                          Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                          Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                          Radio_weekly_adstock_2 + mrp_1 + 
                          discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_9)
vif(dis_lag_home_dl_9)

# Digital_weekly_adstock_1 which is insignicant
dis_lag_home_dl_10 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock + Total_Investment_weekly_adstock_1 + 
                           Total_Investment_weekly_adstock_2 + TV_weekly_adstock_1 + 
                           TV_weekly_adstock_2 + Digital_weekly_adstock_2 + 
                           Sponsorship_weekly_adstock_2 + 
                           Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_10)
vif(dis_lag_home_dl_10)


# TV_weekly_adstock_1 which is insignicant
dis_lag_home_dl_11 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock + Total_Investment_weekly_adstock_1 + 
                           Total_Investment_weekly_adstock_2 + 
                           TV_weekly_adstock_2 + Digital_weekly_adstock_2 + 
                           Sponsorship_weekly_adstock_2 + 
                           Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_11)
vif(dis_lag_home_dl_11)

# Total_Investment_weekly_adstock_1 which is insignicant
dis_lag_home_dl_12 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock +  
                           Total_Investment_weekly_adstock_2 + 
                           TV_weekly_adstock_2 + Digital_weekly_adstock_2 + 
                           Sponsorship_weekly_adstock_2 + 
                           Online_marketing_weekly_adstock_1 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_12)
vif(dis_lag_home_dl_12)

# Online_marketing_weekly_adstock_1 which is insignicant
dis_lag_home_dl_13 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock +  
                           Total_Investment_weekly_adstock_2 + 
                           TV_weekly_adstock_2 + Digital_weekly_adstock_2 + 
                           Sponsorship_weekly_adstock_2 + 
                           Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_13)
vif(dis_lag_home_dl_13)

# Sponsorship_weekly_adstock_2 which is insignicant
dis_lag_home_dl_14 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock +  
                           Total_Investment_weekly_adstock_2 + 
                           TV_weekly_adstock_2 + Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_14)
vif(dis_lag_home_dl_14)

# TV_weekly_adstock_2 which is insignicant
dis_lag_home_dl_15 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock +  
                           Total_Investment_weekly_adstock_2 + 
                           Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_15)
vif(dis_lag_home_dl_15)

# Total_Investment_weekly_adstock_2 which is insignicant
dis_lag_home_dl_16 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock + Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           discount_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_16)
vif(dis_lag_home_dl_16)

# discount_2 which is insignicant
dis_lag_home_dl_17 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + special_sale_calendar + product_verticalDJController + 
                           product_verticalDock + Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_17)
vif(dis_lag_home_dl_17)

# special_sale_calendar which is insignicant
dis_lag_home_dl_18 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + product_verticalDJController + 
                           product_verticalDock + Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           Affiliates_weekly_adstock_2 + SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_18)
vif(dis_lag_home_dl_18)

# Affiliates_weekly_adstock_2 which is insignicant
dis_lag_home_dl_19 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + product_verticalDJController + 
                           product_verticalDock + Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 +  
                           Radio_weekly_adstock_2 + mrp_1 + 
                           gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_19)
vif(dis_lag_home_dl_19)

# mrp_1 which is insignicant
dis_lag_home_dl_20 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + product_verticalDJController + 
                           product_verticalDock + Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_20)
vif(dis_lag_home_dl_20)

# product_verticalDock which is insignicant
dis_lag_home_dl_21 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + discount + units + deliverycdays + sla + 
                           product_procurement_sla + product_verticalDJController + 
                           Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_21)
vif(dis_lag_home_dl_21)

# discount which is insignicant
dis_lag_home_dl_22 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + units + deliverycdays + sla + 
                           product_procurement_sla + product_verticalDJController + 
                           Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_22)
vif(dis_lag_home_dl_22)

# product_verticalDJController which is insignicant
dis_lag_home_dl_23 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           mrp + list_price + units + deliverycdays + sla + 
                           product_procurement_sla + 
                           Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_23)
vif(dis_lag_home_dl_23)

# mrp which is insignicant
dis_lag_home_dl_24 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           list_price + units + deliverycdays + sla + 
                           product_procurement_sla + 
                           Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_24)
vif(dis_lag_home_dl_24)

# list_price which is insignicant
dis_lag_home_dl_25 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + deliverycdays + sla + 
                           product_procurement_sla + 
                           Digital_weekly_adstock_2 + Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_25)
vif(dis_lag_home_dl_25)

# Digital_weekly_adstock_2 which is insignicant
dis_lag_home_dl_26 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + deliverycdays + sla +product_procurement_sla + 
                           Online_marketing_weekly_adstock_2 + 
                           SEM_weekly_adstock_2 + Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_26)
vif(dis_lag_home_dl_26)

# SEM_weekly_adstock_2 which is insignicant
dis_lag_home_dl_27 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + deliverycdays + sla +product_procurement_sla + 
                           Online_marketing_weekly_adstock_2 + 
                           Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_27)
vif(dis_lag_home_dl_27)

# Online_marketing_weekly_adstock_2 which is insignicant
dis_lag_home_dl_28 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + deliverycdays + sla +product_procurement_sla + 
                           Radio_weekly_adstock_2 + gmv_1, data = train_dis_lag)
summary(dis_lag_home_dl_28)
vif(dis_lag_home_dl_28)

# gmv_1 which is insignicant
dis_lag_home_dl_29 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + deliverycdays + sla +product_procurement_sla + 
                           Radio_weekly_adstock_2, data = train_dis_lag)
summary(dis_lag_home_dl_29)
vif(dis_lag_home_dl_29)

# Radio_weekly_adstock_2 which is insignicant
dis_lag_home_dl_30 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + deliverycdays + sla +product_procurement_sla, data = train_dis_lag)
summary(dis_lag_home_dl_30)
vif(dis_lag_home_dl_30)

# deliverycdays which is insignicant
dis_lag_home_dl_31 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + Affiliates_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla +product_procurement_sla, data = train_dis_lag)
summary(dis_lag_home_dl_31)
vif(dis_lag_home_dl_31)

# Affiliates_weekly which is insignicant
dis_lag_home_dl_32 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla +product_procurement_sla, data = train_dis_lag)
summary(dis_lag_home_dl_32)
vif(dis_lag_home_dl_32)

# product_procurement_sla which is insignicant
dis_lag_home_dl_33 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_33)
vif(dis_lag_home_dl_33)

# Total_Investment_weekly  which has high VIF
dis_lag_home_dl_34 <- lm(formula = gmv ~ week_number + TV_weekly + 
                           Sponsorship_weekly +  
                           Online_marketing_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_34)
vif(dis_lag_home_dl_34)

# Online_marketing_weekly which is insignicant
dis_lag_home_dl_35 <- lm(formula = gmv ~ week_number + TV_weekly + 
                           Sponsorship_weekly +  SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_35)
vif(dis_lag_home_dl_35)

# Sponsorship_weekly which is insignicant
dis_lag_home_dl_36 <- lm(formula = gmv ~ week_number + TV_weekly + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                         units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_36)
vif(dis_lag_home_dl_36)

# TV_weekly which is insignicant
dis_lag_home_dl_37 <- lm(formula = gmv ~ week_number + SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_37)
vif(dis_lag_home_dl_37)

# week_number which is insignicant
dis_lag_home_dl_38 <- lm(formula = gmv ~ SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Sponsorship_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_38)
vif(dis_lag_home_dl_38)

# Sponsorship_weekly_adstock which has high VIF
dis_lag_home_dl_39 <- lm(formula = gmv ~ SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + 
                           Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_39)
vif(dis_lag_home_dl_39)

# Content_Marketing_weekly_adstock which is insignicant
dis_lag_home_dl_40 <- lm(formula = gmv ~ SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           TV_weekly_adstock + Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_40)
vif(dis_lag_home_dl_40)

# TV_weekly_adstock which is insignicant
dis_lag_home_dl_41 <- lm(formula = gmv ~ SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           Digital_weekly_adstock + Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_41)
vif(dis_lag_home_dl_41)

# Digital_weekly_adstock which is insignicant
dis_lag_home_dl_42 <- lm(formula = gmv ~ SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + Total_Investment_weekly_adstock + 
                           Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_42)
vif(dis_lag_home_dl_42)

# Total_Investment_weekly_adstock which has high VIF
dis_lag_home_dl_43 <- lm(formula = gmv ~ SEM_weekly + 
                           Radio_weekly + Other_weekly + NPS_Score_weekly + 
                           Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_43)
vif(dis_lag_home_dl_43)

# Radio_weekly which is insignicant
dis_lag_home_dl_44 <- lm(formula = gmv ~ SEM_weekly + Other_weekly + NPS_Score_weekly + 
                           Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_44)
vif(dis_lag_home_dl_44)

# Other_weekly which is insignicant
dis_lag_home_dl_45 <- lm(formula = gmv ~ SEM_weekly + NPS_Score_weekly + 
                           Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_45)
vif(dis_lag_home_dl_45)

# NPS_Score_weekly which is insignicant
dis_lag_home_dl_46 <- lm(formula = gmv ~ SEM_weekly + 
                           Online_marketing_weekly_adstock + 
                           SEM_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_46)
vif(dis_lag_home_dl_46)

# SEM_weekly_adstock which is insignicant
dis_lag_home_dl_47 <- lm(formula = gmv ~ SEM_weekly + 
                           Online_marketing_weekly_adstock + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_47)
vif(dis_lag_home_dl_47)

# Online_marketing_weekly_adstock which is insignicant
dis_lag_home_dl_48 <- lm(formula = gmv ~ SEM_weekly + Radio_weekly_adstock + 
                           units + sla, data = train_dis_lag)
summary(dis_lag_home_dl_48)
vif(dis_lag_home_dl_48)

final_dis_lag_model <- dis_lag_home_dl_48

#--------------------------------------------------------------------------------

summary(final_dis_lag_model)

# Adjusted R-squared:  0.982 
# Signicant variables : SEM_weekly, Radio_weekly_adstock, units, sla

#----------------------------------------------------------------------

# test_dis_lag the model on test_dis_lag dataset
Predict_dis_lag <- predict(final_dis_lag_model,test_dis_lag[-test_dis_lag$gmv])

# Add a new column "test_gmv" into the test_dis_lag dataset
test_dis_lag$test_gmv <- Predict_dis_lag

#-------------------------------------------------------------------------------------------

# calculate the test R2 
cor(test_dis_lag$gmv,test_dis_lag$test_gmv) #0.9943448
cor(test_dis_lag$gmv,test_dis_lag$test_gmv)^2 #0.9887216

#Scatter plot
plot(test_dis_lag$gmv, test_dis_lag$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="actual", ylab="predicted")
#--------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_Homeaudio_dis_lag <-cv.lm(data = homeaudio_dis_lag, form.lm = final_dis_lag_model, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)

attr(CV_Homeaudio_dis_lag,"ms") 
# mean squre : 2.31e+10

#--------------------------------------------------------------------------------------------

#### Deriving elasticity
elas(homeaudio_dis_lag ,final_dis_lag_model) 



######## Distributed Lag + Multiplicative Model #########
#########################################################

homeaudio_mul_dis_lag <- HomeAudio_df

# Lag variables for Adstock (2 lags)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Total_Investment_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Total_Investment_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "TV_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "TV_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Digital_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Digital_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Sponsorship_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Sponsorship_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Content_Marketing_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Content_Marketing_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Online_marketing_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Online_marketing_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Affiliates_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Affiliates_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "SEM_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "SEM_weekly_adstock",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Radio_weekly_adstock",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "Radio_weekly_adstock",slideBy = -2)

# Lag variables for Pricing features
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "mrp",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "mrp",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "list_price",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "list_price",slideBy = -2)

homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "discount",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "discount",slideBy = -2)

# Lag variables for gmv (2lags)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "gmv",slideBy = -1)
homeaudio_mul_dis_lag<-slide(homeaudio_mul_dis_lag,Var = "gmv",slideBy = -2)

#renaming lag variables created
colnames(homeaudio_mul_dis_lag)[42:67] <- c('Total_Investment_weekly_adstock_1',
                                            'Total_Investment_weekly_adstock_2',
                                            'TV_weekly_adstock_1','TV_weekly_adstock_2',
                                            'Digital_weekly_adstock_1','Digital_weekly_adstock_2',
                                            'Sponsorship_weekly_adstock_1','Sponsorship_weekly_adstock_2',
                                            'Content_Marketing_weekly_adstock_1',
                                            'Content_Marketing_weekly_adstock_2',
                                            'Online_marketing_weekly_adstock_1',
                                            'Online_marketing_weekly_adstock_2',
                                            'Affiliates_weekly_adstock_1',
                                            'Affiliates_weekly_adstock_2','SEM_weekly_adstock_1',
                                            'SEM_weekly_adstock_2','Radio_weekly_adstock_1',
                                            'Radio_weekly_adstock_2','mrp_1','mrp_2','list_price_1',
                                            'list_price_2','discount_1','discount_2','gmv_1','gmv_2')

#removing NAs from the lag variables
homeaudio_mul_dis_lag <- na.omit(homeaudio_mul_dis_lag)

#changing the 0s and -ve values to >0 values
homeaudio_mul_dis_lag[homeaudio_mul_dis_lag<=0]<-0.001 
homeaudio_mul_dis_lag <- log(homeaudio_mul_dis_lag)

#modeling
# Divide data in 70:30 
set.seed(100)
indices_mul_dis_lag= sample(1:nrow(homeaudio_mul_dis_lag), 0.7*nrow(homeaudio_mul_dis_lag))
train_home_dlml=homeaudio_mul_dis_lag[indices_mul_dis_lag,]
test_home_dlml = homeaudio_mul_dis_lag[-indices_mul_dis_lag,]

# Develop the first model 
home_dlml_1 <-lm(gmv~.,data=train_home_dlml)
summary(home_dlml_1)

#-------------------------------------------------------------------------------------------

# Apply the stepwise approach
step_home_dlml <- stepAIC(home_dlml_1, direction="both")
step_home_dlml

#------------------------------------------------------------------------------
home_dlml_2 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + mrp + list_price + 
                    discount + units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla + product_verticalKaraokePlayer + 
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 + Content_Marketing_weekly_adstock_2 + 
                    Online_marketing_weekly_adstock_2 + Affiliates_weekly_adstock_2 + 
                    SEM_weekly_adstock_2 + mrp_1 + discount_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_2)
vif(home_dlml_2)

# Content_Marketing_weekly_adstock_2 which is insignicant
home_dlml_3 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + mrp + list_price + 
                    discount + units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla + product_verticalKaraokePlayer + 
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 + 
                    Online_marketing_weekly_adstock_2 + Affiliates_weekly_adstock_2 + 
                    SEM_weekly_adstock_2 + mrp_1 + discount_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_3)
vif(home_dlml_3)

# Affiliates_weekly_adstock_2 which is insignicant
home_dlml_4 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + mrp + list_price + 
                    discount + units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla + product_verticalKaraokePlayer + 
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 + Online_marketing_weekly_adstock_2 +  
                    SEM_weekly_adstock_2 + mrp_1 + discount_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_4)
vif(home_dlml_4)

# Online_marketing_weekly_adstock_2 which is insignicant
home_dlml_5 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + mrp + list_price + 
                    discount + units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla + product_verticalKaraokePlayer + 
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 +   
                    SEM_weekly_adstock_2 + mrp_1 + discount_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_5)
vif(home_dlml_5)

# discount_2 which is insignicant
home_dlml_6 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + mrp + list_price + 
                    discount + units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla + product_verticalKaraokePlayer + 
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 +   
                    SEM_weekly_adstock_2 + mrp_1 + gmv_1, data = train_home_dlml)
summary(home_dlml_6)
vif(home_dlml_6)

# product_verticalKaraokePlayer which is insignicant
home_dlml_7 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + mrp + list_price + 
                    discount + units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla +  
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 +   
                    SEM_weekly_adstock_2 + mrp_1 + gmv_1, data = train_home_dlml)
summary(home_dlml_7)
vif(home_dlml_7)

# discount which is insignicant
home_dlml_8 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + mrp + list_price + 
                    units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla +  
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 +   
                    SEM_weekly_adstock_2 + mrp_1 + gmv_1, data = train_home_dlml)
summary(home_dlml_8)
vif(home_dlml_8)

# mrp which is insignicant
home_dlml_9 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                    Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                    Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                    NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                    Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                    Affiliates_weekly_adstock + SEM_weekly_adstock + list_price + 
                    units + deliverybdays + deliverycdays + sla + 
                    product_procurement_sla +  
                    product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                    TV_weekly_adstock_2 +   
                    SEM_weekly_adstock_2 + mrp_1 + gmv_1, data = train_home_dlml)
summary(home_dlml_9)
vif(home_dlml_9)

# list_price which is insignicant
home_dlml_10 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverybdays + deliverycdays + sla + 
                     product_procurement_sla +  
                     product_verticalSlingBox + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 +   
                     SEM_weekly_adstock_2 + mrp_1 + gmv_1, data = train_home_dlml)
summary(home_dlml_10)
vif(home_dlml_10)

# product_verticalSlingBox which is insignicant
home_dlml_11 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverybdays + deliverycdays + sla + 
                     product_procurement_sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 +   
                     SEM_weekly_adstock_2 + mrp_1 + gmv_1, data = train_home_dlml)
summary(home_dlml_11)
vif(home_dlml_11)

# SEM_weekly_adstock_2 which is insignicant
home_dlml_12 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverybdays + deliverycdays + sla + 
                     product_procurement_sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + mrp_1 + gmv_1, data = train_home_dlml)
summary(home_dlml_12)
vif(home_dlml_12)

# mrp_1 which is insignicant
home_dlml_13 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Digital_weekly + Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverybdays + deliverycdays + sla + 
                     product_procurement_sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_13)
vif(home_dlml_13)

# Digital_weekly which is insignicant
home_dlml_14 <- lm(formula = gmv ~ week_number + Total_Investment_weekly + TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverybdays + deliverycdays + sla + 
                     product_procurement_sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_14)
vif(home_dlml_14)

# week_number which is insignicant
home_dlml_15 <- lm(formula = gmv ~ Total_Investment_weekly + TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverybdays + deliverycdays + sla + 
                     product_procurement_sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_15)
vif(home_dlml_15)

# Total_Investment_weekly which is insignicant
home_dlml_16 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverybdays + deliverycdays + sla + 
                     product_procurement_sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_16)
vif(home_dlml_16)

# deliverybdays which is insignicant
home_dlml_17 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + 
                     product_procurement_sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_17)
vif(home_dlml_17)

# product_procurement_sla which is insignicant
home_dlml_18 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + Online_marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_18)
vif(home_dlml_18)

# Online_marketing_weekly_adstock has high VIF
home_dlml_19 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock + 
                     Content_Marketing_weekly_adstock + 
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_19)
vif(home_dlml_19)

# Content_Marketing_weekly_adstock which is insignicant
home_dlml_20 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + Total_Investment_weekly_adstock_2 + 
                     TV_weekly_adstock_2 + gmv_1, data = train_home_dlml)
summary(home_dlml_20)
vif(home_dlml_20)

# TV_weekly_adstock_2 which is insignicant
home_dlml_21 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + Total_Investment_weekly_adstock_2 + 
                     gmv_1, data = train_home_dlml)
summary(home_dlml_21)
vif(home_dlml_21)

# Total_Investment_weekly_adstock_2 which is insignicant
home_dlml_22 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + Online_marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_22)
vif(home_dlml_22)

# Online_marketing_weekly has high VIF
home_dlml_23 <- lm(formula = gmv ~ TV_weekly + 
                     Content_Marketing_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_23)
vif(home_dlml_23)

# Content_Marketing_weekly which is insignicant 
home_dlml_24 <- lm(formula = gmv ~ TV_weekly + 
                     Affiliates_weekly + SEM_weekly + Radio_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_24)
vif(home_dlml_24)

# Radio_weekly which is insignicant and has high VIF
home_dlml_25 <- lm(formula = gmv ~ TV_weekly + 
                     Affiliates_weekly + SEM_weekly + Other_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_25)
vif(home_dlml_25)

# Other_weekly which is insignicant
home_dlml_26 <- lm(formula = gmv ~ TV_weekly + 
                     Affiliates_weekly + SEM_weekly + 
                     NPS_Score_weekly + Total_Investment_weekly_adstock + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_26)
vif(home_dlml_26)

# Total_Investment_weekly_adstock has high VIF
home_dlml_27 <- lm(formula = gmv ~ TV_weekly + 
                     Affiliates_weekly + SEM_weekly + 
                     NPS_Score_weekly + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_27)
vif(home_dlml_27)

# TV_weekly which is insignicant
home_dlml_28 <- lm(formula = gmv ~ Affiliates_weekly + SEM_weekly + 
                     NPS_Score_weekly + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + SEM_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_28)
vif(home_dlml_28)

# SEM_weekly_adstock has high VIF
home_dlml_29 <- lm(formula = gmv ~ Affiliates_weekly + SEM_weekly + 
                     NPS_Score_weekly + Digital_weekly_adstock +  
                     Affiliates_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_29)
vif(home_dlml_29)

# Digital_weekly_adstock which is insignicant
home_dlml_30 <- lm(formula = gmv ~ Affiliates_weekly + SEM_weekly + 
                     NPS_Score_weekly + Affiliates_weekly_adstock + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_30)
vif(home_dlml_30)

# Affiliates_weekly_adstock which is insignicant
home_dlml_31 <- lm(formula = gmv ~ Affiliates_weekly + SEM_weekly + 
                     NPS_Score_weekly + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_31)
vif(home_dlml_31)

# NPS_Score_weekly which is insignicant
home_dlml_32 <- lm(formula = gmv ~ Affiliates_weekly + SEM_weekly + 
                     units + deliverycdays + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_32)
vif(home_dlml_32)

# deliverycdays which is insignicant
home_dlml_33 <- lm(formula = gmv ~ Affiliates_weekly + SEM_weekly + 
                     units + sla + gmv_1, data = train_home_dlml)
summary(home_dlml_33)
vif(home_dlml_33)

final_mul_dis_lag_model <- home_dlml_33

#--------------------------------------------------------------------

summary(final_mul_dis_lag_model)
# Adjusted R-squared:  0.971 
# signicant variables : Affiliates_weekly, SEM_weekly, units, sla, gmv_1

#----------------------------------------------------------------------

# test_home_dlml the model on test dataset
Predict_mul_dis_lag <- predict(final_mul_dis_lag_model,test_home_dlml[-test_home_dlml$gmv])
# Add a new column "test_gmv" into the test_home_dlml dataset
test_home_dlml$test_gmv <- Predict_mul_dis_lag

#-------------------------------------------------------------------------------------------

# calculate the test R2 

cor(test_home_dlml$gmv,test_home_dlml$test_gmv) #0.994
cor(test_home_dlml$gmv,test_home_dlml$test_gmv)^2 #0.988

#Scatter plot
scatter.smooth(test_home_dlml$gmv, test_home_dlml$test_gmv, main="Correlation between Predicted and Actual Values", 
     xlab="actual", ylab="predicted")

#--------------------------------------------------------------------------------------------

#Cross validation with 5 folds
CV_Homeaudio_mul_dis_lag<-cv.lm(data = homeaudio_mul_dis_lag, form.lm = final_mul_dis_lag_model, m=5, dots = FALSE, seed=50, plotit="Residual", printit=TRUE)
attr(CV_Homeaudio_mul_dis_lag,"ms")
# mean squre : 0.00299

#--------------------------------------------------------------------------------------------

#### Elastic function
elas(homeaudio_mul_dis_lag ,final_mul_dis_lag_model) 
