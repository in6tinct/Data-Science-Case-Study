library(tidyr)
library(ggplot2)
library(dplyr)
df <- read.csv('Uber Request Data.csv',stringsAsFactors = FALSE)
#distinct(df,Status) #OK
#distinct(df,Driver.id) #NA present for cancelled trips
#distinct(df,Pickup.point) #OK

#Fixing multiple date formats in request and drop
date1 <- strptime(df$Request.timestamp,format = '%d-%m-%Y %H:%M:%S')
date2 <- strptime(df$Request.timestamp,format = '%d/%m/%Y %H:%M')
date1[is.na(date1)] <- date2[!is.na(date2)]
df$Request.timestamp <- date1

df[is.na(df$Drop.timestamp),'Drop.timestamp'] <- '11-11-1111 11:11:11'
date1 <- strptime(df$Drop.timestamp,format = '%d-%m-%Y %H:%M:%S')
date2 <- strptime(df$Drop.timestamp,format = '%d/%m/%Y %H:%M')
date1[is.na(date1)] <- date2[!is.na(date2)]
df$Drop.timestamp <- date1

df[which(df$Status == 'Cancelled' | df$Status == 'No Cars Available'),'Drop.timestamp'] <-NA

#Getting hourly derived metrics
df$Request.hour <- format(df$Request.timestamp,'%H')
df$Drop.hour <- format(df$Drop.timestamp,'%H')

#Finding problems for Uber

#Issues throughout day for no cars, cancelled and trip completed
ggplot(df,aes(x=Request.hour,fill=Status))+geom_bar() + labs(x='Request Hour of day', y = 'Total Requests')

require(gridExtra)
#For City to Airport
city2airport <- ggplot(df[which(df$Pickup.point=='City'),],aes(x=Request.hour,fill=Status))+geom_bar()
#For Airport to City
airport2city <- ggplot(df[which(df$Pickup.point=='Airport'),],aes(x=Request.hour,fill=Status))+geom_bar()
grid.arrange(city2airport,airport2city,ncol=2)
grid.arrange(arrangeGrob(city2airport,top = 'City to airport'),
             arrangeGrob(airport2city,top = 'Airport to city'),
             ncol=2)

#Adding new derived metrics and dataframes by summarizing
#Time slots and flows
df$flow <- ifelse(df$Pickup.point=='Airport','outflow','inflow')
df$Request.hour <-as.integer(df$Request.hour)
df$time <- ifelse(df$Request.hour<4,'midnight',
                  ifelse(df$Request.hour>=4 & df$Request.hour<8,'early morning',
                         ifelse(df$Request.hour>=8 & df$Request.hour<12,'morning',
                                ifelse(df$Request.hour>=12 & df$Request.hour<16,'noon',
                                       ifelse(df$Request.hour>=16 & df$Request.hour<20,'evening',
                                              ifelse(df$Request.hour>=20 & df$Request.hour<24,'late evening',NA)
                                              )
                                       )
                                )
                        )
                  )

ggplot(df,aes(x=time,fill=flow))+geom_bar()

#write.csv(df,'df.csv')

#Supply demand ratio

inflow_ratio <- ggplot(df[which(df$Pickup.point=='City'),],aes(x=time,fill=Status))+geom_bar()
outflow_ratio <-ggplot(df[which(df$Pickup.point=='Airport'),],aes(x=time,fill=Status))+geom_bar()
grid.arrange(arrangeGrob(inflow_ratio,top = 'inflow'),
             arrangeGrob(outflow_ratio,top = 'outflow'),
             ncol=2)

#Supply demand : Supply: Trip completed, Demand: All
#For outflow from airport
df_gap_outflow <- df[which(df$flow=='outflow'),c('Status','time')]
df_gap_outflow_group <- group_by(df_gap_outflow,time)
all_outflow <- summarise(df_gap_outflow_group,
                                    x=n())
outflow <- summarise(df_gap_outflow_group[which(df_gap_outflow_group$Status=='Trip Completed'),],
                                                   x=n())
gap_outflow <- data.frame(time=all_outflow$time,n=all_outflow$x-outflow$x)
plot_outflow <- ggplot(gap_outflow,aes(x=time,y=n))+geom_col()+
  scale_x_discrete(limits=c('early morning','morning','noon','evening','late evening','midnight'
))

#For inflow to airport
df_gap_inflow <- df[which(df$flow=='inflow'),c('Status','time')]
df_gap_inflow_group <- group_by(df_gap_inflow,time)
all_inflow <- summarise(df_gap_inflow_group,
                        x=n())
inflow <- summarise(df_gap_inflow_group[which(df_gap_inflow_group$Status=='Trip Completed'),],
                    x=n())
gap_inflow <- data.frame(time=all_inflow$time,n=all_inflow$x-inflow$x)
plot_inflow <- ggplot(gap_inflow,aes(x=time,y=n))+geom_col()+
  scale_x_discrete(limits=c('early morning','morning','noon','evening','late evening','midnight'
))

#Gap plot
grid.arrange(arrangeGrob(plot_inflow,top = 'inflow'),
             arrangeGrob(plot_outflow,top = 'outflow'),
             ncol=2)
