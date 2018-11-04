library(dplyr)
library(tidyr)
library(stringr)
#Checkpoint 1
companies <- read.delim('companies.txt',header = TRUE,stringsAsFactors = FALSE)
rounds2 <- read.csv('rounds2.csv',stringsAsFactors = FALSE)

#Checkpoint 2
##Unique companies in rounds2
rounds2$company_permalink <- tolower(rounds2$company_permalink)
#sum(is.na(rounds2$company_permalink)) is 0 so no NA's
n_distinct(rounds2$company_permalink) #66368

##Unique companies in companies
companies$permalink <- tolower(companies$permalink)
#sum(is.na(companies$permalink)) is 0 so no NA's
n_distinct(companies$permalink) #66368

##Unique key in companies dataframe
nrow(companies["permalink"]) #66368 total rows
companies$permalink <- tolower(companies$permalink)
n_distinct(companies$permalink) #66368 so permalink is a unique key

####Are there any companies in the rounds2 file which are not present in companies?
rounds2_unique<-distinct(rounds2,company_permalink)
companies_diff<-!(rounds2_unique$company_permalink %in% companies$permalink)
sum(companies_diff) #0 so all are present

#install.packages("imputeTS")
#library(imputeTS)

##DATA CLEANING##NA values removed for raised_amt_usd & removed rows with no country code##
master_frame <- merge(rounds2,companies,by.x='company_permalink',by.y='permalink')
#114949 obs
#Replacing NA's with 0s as they were part of a funding round but did not get funded
master_frame[which(is.na(master_frame$raised_amount_usd)),"raised_amount_usd"] <- 0
#We do not need country code = blank
master_frame <- master_frame[master_frame$country_code != "",]

#Average fundings


group_data <- group_by(master_frame,funding_round_type)
average <- summarise(group_data,mean = mean(raised_amount_usd,na.rm = T))
average_sorted <- arrange(average,desc(mean))

#average for venture - 10730347
round(as.double(average_sorted[which(average_sorted$funding_round_type=='venture'),2]))

#average for angel - 782591
round(as.double(average_sorted[which(average_sorted$funding_round_type=='angel'),2]))

#average for seed - 591227
round(as.double(average_sorted[which(average_sorted$funding_round_type=='seed'),2]))

#average for private_equity - 63993399
round(as.double(average_sorted[which(average_sorted$funding_round_type=='private_equity'),2]))

#######################

#install.packages("data.table")
library(data.table)

master_frame_as_table = as.data.table(master_frame)
master_frame_as_table <- master_frame_as_table[order(country_code),]
summarized.country = master_frame_as_table[,sum(raised_amount_usd),by="country_code"]

#####checkpoint 3
venture <- filter(master_frame, funding_round_type == "venture")
mean(venture$raised_amount_usd, trim = 0, na.rm = FALSE)
venture_as_table = as.data.table(venture)
summarized.country.venture = venture_as_table[,sum(raised_amount_usd),by="country_code"]
summarized.country.venture <- arrange(summarized.country.venture,desc(V1))
top9 <- head(summarized.country.venture[order(summarized.country.venture$V1, decreasing=TRUE), ], 9)
names(top9)[2]<-paste("total_amount_raised")
#USA,GBR and IND

######Checkpoint 4.....merging
mapping<-read.csv("mapping.csv",stringsAsFactors = FALSE,check.names = FALSE)
newdata <- gather(mapping,main_sector,my_val,'Automotive & Sports':'Social, Finance, Analytics, Advertising')
newdata <- newdata[!(newdata$my_val== 0),]
newdata <- newdata[newdata$category_list != "",]
newdata$my_val<-NULL
master_frame_mapping <- master_frame[master_frame$category_list != "",]
venture_mapping<-venture
venture_mapping$primary_sector<-str_split(venture_mapping$category_list,"\\|",simplify = TRUE)[,1]

merged_frame <- merge(venture_mapping,newdata,by.x='primary_sector',by.y='category_list')

####Checkpoint 5.....5 to 15 and D1 D2 D3
funding_5to15<-subset(merged_frame,merged_frame$raised_amount_usd>=5000000 & merged_frame$raised_amount_usd<=15000000 )

D1<-subset(funding_5to15,funding_5to15$country_code=="USA")
D2<-subset(funding_5to15,funding_5to15$country_code=="GBR")
D3<-subset(funding_5to15,funding_5to15$country_code=="IND")

####Question 1
#Total number of investments for each country c1,c2,c3
nrow(D1)
nrow(D2)
nrow(D3)

####Question 2
#Total amount of investment for each sector
sum(D1$raised_amount_usd)
sum(D2$raised_amount_usd)
sum(D3$raised_amount_usd)



###(Question 3)Top sector (based on count of investments)
#(Question 6)D1 Group by main_sector along with count of investment
# & total investment made.
D1_group_by<-group_by(D1,main_sector)
D1_group<-summarise(D1_group_by,sum = sum(raised_amount_usd),count = n())

# For Country1- Total number of investments (count), Total amount of investment (USD), Top sector (based on count of investments) 
D1_arrange<-arrange(D1_group,desc(count)) #Others
D1_arrange[1,1]

#D2 Group by main_sector along with count of investments and total investment made.
D2_group_by<-group_by(D2,main_sector)
D2_group<-summarise(D2_group_by,sum = sum(raised_amount_usd),count = n())

# For Country2- Total number of investments (count), Total amount of investment (USD), Top sector (based on count of investments) D2_arrange<-arrange(D2_count_sum,desc(`length(main_sector)`))
D2_arrange<-arrange(D2_group,desc(count)) #Others
D2_arrange[1,1]

#D2 Group by main_sector along with count of investments and total investment made.
D3_group_by<-group_by(D3,main_sector)
D3_group<-summarise(D3_group_by,sum = sum(raised_amount_usd),count = n())

# For Country3- Total number of investments (count), Total amount of investment (USD), Top sector (based on count of investments) D2_arrange<-arrange(D2_count_sum,desc(`length(main_sector)`))
D3_arrange<-arrange(D3_group,desc(count)) #Others
D3_arrange[1,1]

## Second sector (no of investements)
D1_arrange[2,1]
D2_arrange[2,1]
D3_arrange[2,1]

## Third sector (no of investements)
D1_arrange[3,1]
D2_arrange[3,1]
D3_arrange[3,1]

# Number of investments in top sector - Others

D1_group[which(D1_group$main_sector=='Others'),3] #2923
D2_group[which(D2_group$main_sector=='Others'),3] #143
D3_group[which(D3_group$main_sector=='Others'),3] #109

# Number of investments:second sector - Cleantech / Semiconductors(D1,D2),News, Search and Messaging(D3)

D1_group[which(D1_group$main_sector=='Cleantech / Semiconductors'),3] #2297
D2_group[which(D2_group$main_sector=='Cleantech / Semiconductors'),3] #127
D3_group[which(D3_group$main_sector=='News, Search and Messaging'),3] #52

# Number of investments:third sector - Social, Finance, Analytics, Advertising(D1,D2),Entertainment(D3)

D1_group[which(D1_group$main_sector=='Social, Finance, Analytics, Advertising'),3] #1912
D2_group[which(D2_group$main_sector=='Social, Finance, Analytics, Advertising'),3] #98
D3_group[which(D3_group$main_sector=='Entertainment'),3] #33

########Question 9 c1
#For point 3 (top sector count-wise),
###company receiving the highest investment for country C1?

s1<-subset(D1,D1$main_sector=='Others')
s1_group_by<-group_by(s1,company_permalink)
s1_count<-summarise(s1_group_by,count=n())
s1_arrange<-arrange(s1_count,desc(count))
#top company key
c1_s1_top_company<-toString(s1_arrange[1,1])

#top company name
companies[which(companies$permalink==c1_s1_top_company),"name"]


######Question 9 c2
# company receiving the highest investment for country C2?
s2<-subset(D2,D2$main_sector=='Others')
s2_group_by<-group_by(s2,company_permalink)
s2_sum<-summarise(s2_group_by,count=n())
s2_arrange<-arrange(s2_sum,desc(count))
#top company key
c2_s2_top_company<-toString(s2_arrange[1,1])

#top company name
companies[which(companies$permalink==c2_s2_top_company),"name"]


####question 9 c3
#company receiving the highest investment for country C3?
s3<-subset(D3,D3$main_sector=='Others')
s3_group_by<-group_by(s3,company_permalink)
s3_sum<-summarise(s3_group_by,count=n())
s3_arrange<-arrange(s3_sum,desc(count))
#top company key
c3_s3_top_company<-toString(s3_arrange[1,1])

#top company name
companies[which(companies$permalink==c3_s3_top_company),"name"]





############
####Question 10 C1######
#For point 4 (second best sector count-wise),
#####company receiving the highest investment for country1?

s4<-subset(D1,D1$main_sector=='Cleantech / Semiconductors')
s4_group_by<-group_by(s4,company_permalink)
s4_sum<-summarise(s4_group_by,count=n())
s4_arrange<-arrange(s4_sum,desc(count))
#top company key
c1_s4_top_company<-toString(s4_arrange[1,1])

#top company name
companies[which(companies$permalink==c1_s4_top_company),"name"]



#####Question 10 C2
#For point 4 (second best sector count-wise)
#####company receiving the highest investment for country2?
s5<-subset(D2,D2$main_sector=='Cleantech / Semiconductors')
s5_group_by<-group_by(s5,company_permalink)
s5_sum<-summarise(s5_group_by,count=n())
s5_arrange<-arrange(s5_sum,desc(count))
#top company key
c2_s5_top_company<-toString(s5_arrange[1,1])

#top company name
companies[which(companies$permalink==c2_s5_top_company),"name"]



#######Question 10 C3
##For point 4 (second best sector count-wise)
#####company receiving the highest investment for country3?
s6<-subset(D3,D3$main_sector=='News, Search and Messaging')
s6_group_by<-group_by(s6,company_permalink)
s6_sum<-summarise(s6_group_by,count=n())
s6_arrange<-arrange(s6_sum,desc(count))
#top company key
c3_s6_top_company<-toString(s6_arrange[1,1])

#top company name
companies[which(companies$permalink==c3_s6_top_company),"name"]

#write.csv(master_frame,file='master_data.csv') #For presentation
