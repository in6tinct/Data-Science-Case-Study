##INITIALIZE!

spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

# Initialise the sparkR session
sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

#Check the data first just in case
#hdfs dfs -ls /common_folder/nyc_parking/
#Found 3 items
#-rwxrwxr-x   2 hdfs trainers 2864071408 2018-09-03 06:06 /common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv
#-rwxrwxr-x   2 hdfs trainers 2151937808 2018-09-03 06:07 /common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv
#-rwxrwxr-x   2 hdfs trainers 2086913576 2018-09-03 06:09 /common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv

#Time to load the data : 2015-2017
pv15 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2015.csv",
                        "CSV", header="true", inferSchema = "true")
head(pv15) #Lots of nas in some columns
nrow(pv15) 
#Examine the data: Ans: 2015 -> 11809233 rows
ncol(pv15) #51
colnames(pv15) #Will need later

pv16 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2016.csv",
                        "CSV", header="true", inferSchema = "true")
#head(pv16)
nrow(pv16)
#Examine the data: Ans: 2016 -> 10626899 rows
ncol(pv16) #51

pv17 <- SparkR::read.df("/common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",
                        "CSV", header="true", inferSchema = "true")
#head(pv17)
nrow(pv17) 
#Examine the data: Ans: 2017 -> 10803028 rows
ncol(pv17) #43 rows only
colnames(pv17)

#Creating views for sql
createOrReplaceTempView(pv15,"pv15_view")
createOrReplaceTempView(pv16,"pv16_view")
createOrReplaceTempView(pv17,"pv17_view")

##2: Unique states
pv15_state <- SparkR::sql("select distinct `Registration State` from pv15_view")
head(arrange(pv15_state,asc(pv15_state$`Registration State`))) #99 : A numeric registration state

pv16_state <- SparkR::sql("select distinct `Registration State` from pv16_view")
head(arrange(pv16_state,asc(pv16_state$`Registration State`))) #99 : A numeric registration state

pv17_state <- SparkR::sql("select distinct `Registration State` from pv17_view")
head(arrange(pv17_state,asc(pv17_state$`Registration State`))) #99 : A numeric registration state

#State having max entries
head(SparkR::sql("select `Registration State`,count(*) from pv15_view group by `Registration State`
                 order by count(*) desc")) #NY has the most numbers

head(SparkR::sql("select `Registration State`,count(*) from pv16_view group by `Registration State`
                 order by count(*) desc")) #NY has the most numbers

head(SparkR::sql("select `Registration State`,count(*) from pv17_view group by `Registration State`
                 order by count(*) desc")) #NY has the most numbers

#I got 99 problems hence time to change 99 with NY
pv15$`Registration State` <-ifelse(pv15$`Registration State`=='99','NY',pv15$`Registration State`)
pv16$`Registration State` <-ifelse(pv16$`Registration State`=='99','NY',pv16$`Registration State`)
pv17$`Registration State` <-ifelse(pv17$`Registration State`=='99','NY',pv17$`Registration State`)

#ReCreating views for sql
createOrReplaceTempView(pv15,"pv15_view")
createOrReplaceTempView(pv16,"pv16_view")
createOrReplaceTempView(pv17,"pv17_view")

head(SparkR::sql("select count(distinct `Registration State`)
                              as count from pv15_view"))
#68 distinct states for 2015

head(SparkR::sql("select count(distinct `Registration State`)
                              as count from pv16_view"))
#67 distinct states for 2016

head(SparkR::sql("select count(distinct `Registration State`)
                              as count from pv17_view"))
#66 distinct states for 2017

## 3 :  Violation location
head(SparkR::sql("select count(*),count(*)*(100/11809233) as percent from pv15_view where `Violation Location` is null"))
#2015 : 1799170 records with null violation location : 15%

head(SparkR::sql("select count(*),count(*)*(100/10626899) as percent from pv16_view where `Violation Location` is null"))
#2016 : 1868656 records with null violation location : 17.6%

head(SparkR::sql("select count(*),count(*)*(100/10803028) as percent from pv17_view where `Violation Location` is null"))
#2017 : 2072400 records with null violation location : 19%

###-------Aggregation tasks------###

# Question  1 #
#2015 data
violation_code_15 <- SparkR::sql("select `Violation Code`,count(*) as count from pv15_view group by `Violation Code`
                 order by count(*) desc limit 5")
head(violation_code_15)
#Violation Code   count                                                        
#1             21 1630912
#2             38 1418627
#3             14  988469
#4             36  839197
#5             37  795918

library(ggplot2)
library(gridExtra)
plot1 <- ggplot(SparkR::as.data.frame(violation_code_15),aes(x=as.factor(`Violation Code`),y=count))+
  geom_bar(stat = "identity") + xlab("Top Violation Code : 2015") + ylab("Count")

#2016 data
violation_code_16 <- SparkR::sql("select `Violation Code`,count(*) as count from pv16_view group by `Violation Code`
                 order by count(*) desc limit 5")
head(violation_code_16)
#Violation Code   count                                                        
#1             21 1531587
#2             36 1253512
#3             38 1143696
#4             14  875614
#5             37  686610
plot2 <- ggplot(SparkR::as.data.frame(violation_code_16),aes(x=as.factor(`Violation Code`),y=count))+
  geom_bar(stat = "identity") + xlab("Top Violation Code : 2016") + ylab("Count")

#2017 data
violation_code_17 <- SparkR::sql("select `Violation Code`,count(*) as count from pv17_view group by `Violation Code`
                 order by count(*) desc limit 5")
head(violation_code_17)
#Violation Code   count                                                        
#1             21 1528588
#2             36 1400614
#3             38 1062304
#4             14  893498
#5             20  618593

plot3 <- ggplot(SparkR::as.data.frame(violation_code_17),aes(x=as.factor(`Violation Code`),y=count))+
  geom_bar(stat = "identity") + xlab("Top Violation Code : 2017") + ylab("Count")

grid.arrange(plot1,plot2,plot3)

# Question 2 #
head(SparkR::sql("select `Vehicle Body Type`,count(*) as count from pv15_view group by `Vehicle Body Type`
                 order by count(*) desc limit 5"))
#2015
#   Vehicle Body Type   count                                                     
#1              SUBN 3729346
#2              4DSD 3340014
#3               VAN 1709091
#4              DELV  892781
#5               SDN  524596

head(SparkR::sql("select `Vehicle Body Type`,count(*) as count from pv16_view group by `Vehicle Body Type`
                 order by count(*) desc limit 5"))
#2016
#   Vehicle Body Type   count                                                     
#1              SUBN 3466037
#2              4DSD 2992107
#3               VAN 1518303
#4              DELV  755282
#5               SDN  424043

head(SparkR::sql("select `Vehicle Body Type`,count(*) as count from pv17_view group by `Vehicle Body Type`
                 order by count(*) desc limit 5"))
#2017
#   Vehicle Body Type   count                                                     
#1              SUBN 3719802
#2              4DSD 3082020
#3               VAN 1411970
#4              DELV  687330
#5               SDN  438191

#Vehicle Make
head(SparkR::sql("select `Vehicle Make`,count(*) as count from pv15_view group by `Vehicle Make`
                 order by count(*) desc limit 5"))
#2015
#Vehicle Make   count                                                          
#1         FORD 1521874
#2        TOYOT 1217087
#3        HONDA 1102614
#4        NISSA  908783
#5        CHEVR  897845

head(SparkR::sql("select `Vehicle Make`,count(*) as count from pv16_view group by `Vehicle Make`
                 order by count(*) desc limit 5"))
#2016
# Vehicle Make   count                                                          
#1         FORD 1324774
#2        TOYOT 1154790
#3        HONDA 1014074
#4        NISSA  834833
#5        CHEVR  759663

head(SparkR::sql("select `Vehicle Make`,count(*) as count from pv17_view group by `Vehicle Make`
                 order by count(*) desc limit 5"))
#2017
#Vehicle Make   count                                                          
#1         FORD 1280958
#2        TOYOT 1211451
#3        HONDA 1079238
#4        NISSA  918590
#5        CHEVR  714655

## 3 : Violation precincts

#2015 : Top Violation precincts
head(SparkR::sql("select `Violation Precinct`,count(*) as count from pv15_view group by `Violation Precinct`
                 order by count(*) desc limit 6"))
#Violation Precinct   count                                                    
#1                  0 1799170 #Erroneous
#2                 19  598351
#3                 18  427510
#4                 14  409064
#5                  1  329009
#6                114  320963

#2016 : Top Violation precincts
head(SparkR::sql("select `Violation Precinct`,count(*) as count from pv16_view group by `Violation Precinct`
                 order by count(*) desc limit 6"))
#Violation Precinct   count                                                    
#1                  0 1868655 #Erroneous
#2                 19  554465
#3                 18  331704
#4                 14  324467
#5                  1  303850
#6                114  291336

#2017 : Top Violation precincts
head(SparkR::sql("select `Violation Precinct`,count(*) as count from pv17_view group by `Violation Precinct`
                 order by count(*) desc limit 6"))
#Violation Precinct   count                                                    
#1                  0 2072400 #Erroneous
#2                 19  535671
#3                 14  352450
#4                  1  331810
#5                 18  306920
#6                114  296514

violation_precinct_15 <- SparkR::sql("select `Violation Precinct`,count(*) as count from pv15_view 
                  where `Violation Precinct`!='0'
                  group by `Violation Precinct`
                 order by count(*) desc limit 5")
plot1 <- ggplot(SparkR::as.data.frame(violation_precinct_15),aes(x=as.factor(`Violation Precinct`),y=count))+
  geom_bar(stat = "identity") + xlab("Top Violation precinct : 2015") + ylab("Count")

violation_precinct_16 <- SparkR::sql("select `Violation Precinct`,count(*) as count from pv16_view 
                  where `Violation Precinct`!='0'
                  group by `Violation Precinct`
                 order by count(*) desc limit 5")
plot2 <- ggplot(SparkR::as.data.frame(violation_precinct_16),aes(x=as.factor(`Violation Precinct`),y=count))+
  geom_bar(stat = "identity") + xlab("Top Violation precinct : 2016") + ylab("Count")

violation_precinct_17 <- SparkR::sql("select `Violation Precinct`,count(*) as count from pv17_view 
                  where `Violation Precinct`!='0'
                  group by `Violation Precinct`
                 order by count(*) desc limit 5")
plot3 <- ggplot(SparkR::as.data.frame(violation_precinct_17),aes(x=as.factor(`Violation Precinct`),y=count))+
  geom_bar(stat = "identity") + xlab("Top Violation precinct : 2017") + ylab("Count")

grid.arrange(plot1,plot2,plot3)

#Observation: Top 5 violation precincts are same throughout 2015 to 2017 and the no 1 precinct
#is 0 which is errorenous and has seen increasing violations that have been inputed
#incorrectly from '15 to '17. 
#Ignoring that, the top 5 violations precincts are the same with almost similar frequency.

#2015 : Top Issuer precincts
head(SparkR::sql("select `Issuer Precinct`,count(*) as count from pv15_view group by `Issuer Precinct`
                 order by count(*) desc limit 6"))
#Issuer Precinct   count                                                       
#1               0 2037745
#2              19  579998
#3              18  417329
#4              14  392922
#5               1  318778
#6             114  314437

#2016 : Top Issuer precincts
head(SparkR::sql("select `Issuer Precinct`,count(*) as count from pv16_view group by `Issuer Precinct`
                 order by count(*) desc limit 6"))
#Issuer Precinct   count                                                       
#1               0 2140274
#2              19  540569
#3              18  323132
#4              14  315311
#5               1  295013
#6             114  286924

#2017 : Top Issuer precincts
head(SparkR::sql("select `Issuer Precinct`,count(*) as count from pv17_view group by `Issuer Precinct`
                 order by count(*) desc limit 6"))
#Issuer Precinct   count                                                       
#1               0 2388479
#2              19  521513
#3              14  344977
#4               1  321170
#5              18  296553
#6             114  289950

#Observation: Pretty much same as Violation precincts. Errors and top precincts are same.
#Violation precinct and issuer precinct is almost similar in frequency.


# Question 4

#The top 3 precincts are 19,14,1.
head(SparkR::sql("select `Violation Code`,count(*) as count from pv15_view where
                  `Violation Precinct` in (19,14,1) group by `Violation Code`
                  order by count(*) desc
                 "))
#Violation Code count                                                       
#1             14   233299
#2             38   123981
#3             16   108355
#4             69   104429
#5             37   102141
#6             46    84177

head(SparkR::sql("select `Violation Code`,count(*) as count from pv16_view where
                  `Violation Precinct` in (19,14,1) group by `Violation Code`
                  order by count(*) desc
                 "))
#Violation Code  count                                                         
#1             14 199806
#2             46 110983
#3             38 101144
#4             16  92933
#5             37  92416
#6             69  85438

head(SparkR::sql("select `Violation Code`,count(*) as count from pv17_view where
                  `Violation Precinct` in (19,14,1) group by `Violation Code`
                  order by count(*) desc
                 "))
#Violation Code  count                                                         
#1             14 210865
#2             46 129758
#3             38  97816
#4             37  89584
#5             69  73875
#6             16  72237

#Violation code 14 has unusually high numbers in all 3 years
#Lets check if its common across precincts

head(SparkR::sql("(select `Issuer Precinct`,`Violation Code`,count(*) as count from pv15_view where
                  `Issuer Precinct`=19 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
				 union all
				 (select `Issuer Precinct`,`Violation Code`,count(*) as count from pv15_view where
                  `Issuer Precinct`=14 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
				 union all
				 (select `Issuer Precinct`,`Violation Code`,count(*) as count from pv15_view where
                  `Issuer Precinct`=1 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
                 "),num=9L)
#2015
#Issuer Precinct Violation Code count
#1              19             38 97154
#2              19             37 85007
#3              19             14 64133
#4              14             69 84895
#5              14             14 81896
#6              14             31 43928
#7               1             14 79164
#8               1             16 43915
#9               1             20 25550

head(SparkR::sql("(select `Issuer Precinct`,`Violation Code`,count(*) as count from pv16_view where
                  `Issuer Precinct`=19 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
				 union all
				 (select `Issuer Precinct`,`Violation Code`,count(*) as count from pv16_view where
                  `Issuer Precinct`=14 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
				 union all
				 (select `Issuer Precinct`,`Violation Code`,count(*) as count from pv16_view where
                  `Issuer Precinct`=1 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
                 "),num=9L)
#Issuer Precinct Violation Code count
#1              19             38 77183
#2              19             37 75641
#3              19             46 73016
#4              14             69 67932
#5              14             14 62426
#6              14             31 35711
#7               1             14 69200
#8               1             16 37981
#9               1             20 24725

head(SparkR::sql("(select `Issuer Precinct`,`Violation Code`,count(*) as count from pv17_view where
                  `Issuer Precinct`=19 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
				 union all
				 (select `Issuer Precinct`,`Violation Code`,count(*) as count from pv17_view where
                  `Issuer Precinct`=14 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
				 union all
				 (select `Issuer Precinct`,`Violation Code`,count(*) as count from pv17_view where
                  `Issuer Precinct`=1 group by `Issuer Precinct`,`Violation Code` 
                 order by count(*) desc limit 3)
                 "),num=9L)
#Issuer Precinct Violation Code count
#1              19             46 86390
#2              19             37 72437
#3              19             38 72344
#4              14             14 73837
#5              14             69 58026
#6              14             31 39857
#7               1             14 73522
#8               1             16 38937
#9               1             20 27841

#Looks like 14 is pretty common across precincts and years.

# Question 5 : Nulls and timings
head(SparkR::sql("select `Violation Code`,`Violation time` from pv15_view where
                  `Violation time` is null
                 "))
#Looks like nulls are present. Lets filter out these data
pv15 <- dropna(pv15,how = "any",cols = c("Violation time"))
createOrReplaceTempView(pv15,"pv15_view")
#Rechecked : No more nulls. Lets repeat the steps for 2016 and 2017
pv16 <- dropna(pv16,how = "any",cols = c("Violation time"))
pv17 <- dropna(pv17,how = "any",cols = c("Violation time"))
#View recreation
createOrReplaceTempView(pv16,"pv16_view")
createOrReplaceTempView(pv17,"pv17_view")

#time to analyse the violation time data
head(SparkR::sql("select `Violation time` from pv15_view"))

head(SparkR::sql("select `Violation time` from pv15_view where
                  `Violation time` like '%P'"))
head(SparkR::sql("select `Violation time` from pv15_view where
                  `Violation time` like '%A'"))

head(SparkR::sql("select max(length(`Violation time`)),min(length(`Violation time`)) from pv15_view"))

head(SparkR::sql("select `Violation time` from pv15_view where
                  length(`Violation time`)=4
                 "))
head(SparkR::sql("select `Violation time` from pv15_view where
                  length(`Violation time`)=4 and `Violation time` like '2%'
                 "))
head(SparkR::sql("select `Violation time` from pv15_view where
                  length(`Violation time`)=5 and `Violation time` like '2%P'
                 "))
#Looks like we need to remove the last character in timestamp and treat the first 4
#as the time stamp also remove records greater than 2359

violation_data <- SparkR::sql("select 2015 as Year,`Violation Code`,cast(substring(`Violation time`,1,2) as int) as hour,
                  cast(substring(`Violation time`,3,2) as int) as mins
                  from pv15_view 
                  union all
                  select 2016 as Year,`Violation Code`,cast(substring(`Violation time`,1,2) as int) as hour,
                  cast(substring(`Violation time`,3,2) as int) as mins
                  from pv16_view 
                  union all
                  select 2017 as Year,`Violation Code`,cast(substring(`Violation time`,1,2) as int) as hour,
                  cast(substring(`Violation time`,3,2) as int) as mins
                  from pv17_view 
                              ")
head(arrange(violation_data,desc(violation_data$hour)))
#A lot of strange hours

createOrReplaceTempView(violation_data,"violation_view")

violation_data2 <- SparkR::sql("select Year,`Violation Code`,hour, 
				case when hour>=0 and hour<4 then '0-4'
				 when hour>=4 and hour<8 then '4-8'
				 when hour>=8 and hour<12 then '8-12'
				 when hour>=12 and hour<16 then '12-16'
				 when hour>=16 and hour<20 then '16-20'
				 when hour>=20 and hour<24 then '20-24'
        end as `Violation period`
				from violation_view where hour<24
                 ")
head(violation_data2)
createOrReplaceTempView(violation_data2,"violation_view2")

#Most commonly occuring violations for each group:

#2015
head(SparkR::sql("(select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '0-4' and Year=2015 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '4-8' and Year=2015 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '8-12' and Year=2015 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '12-16' and Year=2015 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '16-20' and Year=2015 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '20-24' and Year=2015 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 "),num=18L)
#2016
head(SparkR::sql("(select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '0-4' and Year=2016 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '4-8' and Year=2016 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '8-12' and Year=2016 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '12-16' and Year=2016 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '16-20' and Year=2016 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '20-24' and Year=2016 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 "),num=18L)

#2017
head(SparkR::sql("(select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '0-4' and Year=2017 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '4-8' and Year=2017 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '8-12' and Year=2017 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '12-16' and Year=2017 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '16-20' and Year=2017 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 union all
                 (select `Violation period`,`Violation Code`,count(*) from violation_view2
                 where `Violation period` = '20-24' and Year=2017 group by `Violation period`,`Violation Code`
                 order by count(*) desc limit 3)
                 "),num=18L)

#Three most commonly occuring violations:

#2015:
head(SparkR::sql("select `Violation Code`,count(*) from violation_view2
                 where Year=2015 group by `Violation Code`
                 order by count(*) desc limit 3"))
#14,38,21

#2016:
head(SparkR::sql("select `Violation Code`,count(*) from violation_view2
                 where Year=2016 group by `Violation Code`
                 order by count(*) desc limit 3"))
#21,36,38

#2017:
head(SparkR::sql("select `Violation Code`,count(*) from violation_view2
                 where Year=2017 group by `Violation Code`
                 order by count(*) desc limit 3"))
#21,36,38

#Most common time of day:
#2015
head(SparkR::sql("(select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2015 and `Violation Code` = 14 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 union all
				 (select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2015 and `Violation Code` = 38 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 union all
				 (select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2015 and `Violation Code` = 21 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 "))
#Violation Code Violation period count(1)
#1             14             8-12   366515
#2             38             8-12   546381
#3             21             8-12  1292226

#2016
head(SparkR::sql("(select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2016 and `Violation Code` = 21 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 union all
				 (select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2016 and `Violation Code` = 36 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 union all
				 (select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2016 and `Violation Code` = 38 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 "))
#Violation Code Violation period count(1)
#1             21             8-12  1209672
#2             36             8-12   586791
#3             38             8-12   441273

#2017
head(SparkR::sql("(select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2017 and `Violation Code` = 21 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 union all
				 (select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2017 and `Violation Code` = 36 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 union all
				 (select `Violation Code`,`Violation period`,count(*) from violation_view2
                 where Year=2017 and `Violation Code` = 38 group by `Violation Code`,`Violation period`
                 order by count(*) desc limit 1)
				 "))
#2017
#Violation Code Violation period count(1)
#1             21             8-12  1183054
#2             36             8-12   751422
#3             38             8-12   393547


# Question : 6
head(pv15)
head(SparkR::sql("select `Issue Date` from pv15_view"))
head(SparkR::sql("select `Issue Date` from pv15_view where `Issue Date` is null"))
head(SparkR::sql("select distinct length(`Issue Date`) from pv15_view"))

head(pv15)

#Extract year from issue date
pv15$`Issue Year` <- SparkR::substr(pv15$`Issue Date`,7,10)
pv15$`Issue month` <- SparkR::substr(pv15$`Issue Date`,1,2)
head(pv15)
pv16$`Issue Year` <- SparkR::substr(pv16$`Issue Date`,7,10)
pv16$`Issue month` <- SparkR::substr(pv16$`Issue Date`,1,2)
pv17$`Issue Year` <- SparkR::substr(pv17$`Issue Date`,7,10)
pv17$`Issue month` <- SparkR::substr(pv17$`Issue Date`,1,2)

createOrReplaceTempView(pv15,"pv15_view")
createOrReplaceTempView(pv16,"pv16_view")
createOrReplaceTempView(pv17,"pv17_view")

#Lets create 4 seasons according to new york weather
#Fall Season. September, October, November.
#Winter Season. December, January, February.
#Spring Season. March, April, May.
#Summer Season. June, July, August


head(SparkR::sql("select distinct `Issue Year` from pv15_view
                 "))
#a lot of years data

#For 2015
season <- SparkR::sql("select `Issue month`,
                 case when `Issue month` in ('09','10','11') then 'Fall'
                      when `Issue month` in ('12','01','02') then 'Winter'
                      when `Issue month` in ('03','04','05') then 'Spring'
                      when `Issue month` in ('06','07','08') then 'Summer'
                end as `season`,`Violation Code` from pv15_view
                 ")
createOrReplaceTempView(season,'season_view')

head(SparkR::sql("select season,count(*) as count from season_view group by season"
))
#season count                                                               
#1 Spring  2956623
#2 Summer  3157641
#3   Fall  2794515
#4 Winter  2898739

head(SparkR::sql("(select season,`Violation Code`,count(*) as count from season_view
                  where season='Spring'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                 (select season,`Violation Code`,count(*) as count from season_view
                  where season='Summer'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                (select season,`Violation Code`,count(*) as count from season_view
                  where season='Fall'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                (select season,`Violation Code`,count(*) as count from season_view
                  where season='Winter'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 "),num=12L)

#For 2016
season <- SparkR::sql("select `Issue month`,
                      case when `Issue month` in ('09','10','11') then 'Fall'
                      when `Issue month` in ('12','01','02') then 'Winter'
                      when `Issue month` in ('03','04','05') then 'Spring'
                      when `Issue month` in ('06','07','08') then 'Summer'
                      end as `season`,`Violation Code` from pv16_view
                      ")
createOrReplaceTempView(season,'season_view')

head(SparkR::sql("select season,count(*) as count from season_view group by season"
))
#season count                                                               
#1 Spring  2956623
#2 Summer  3157641
#3   Fall  2794515
#4 Winter  2898739

head(SparkR::sql("(select season,`Violation Code`,count(*) as count from season_view
                 where season='Spring'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                 (select season,`Violation Code`,count(*) as count from season_view
                 where season='Summer'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                 (select season,`Violation Code`,count(*) as count from season_view
                 where season='Fall'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                 (select season,`Violation Code`,count(*) as count from season_view
                 where season='Winter'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 "),num=12L)

#For 2017
season <- SparkR::sql("select `Issue month`,
                      case when `Issue month` in ('09','10','11') then 'Fall'
                      when `Issue month` in ('12','01','02') then 'Winter'
                      when `Issue month` in ('03','04','05') then 'Spring'
                      when `Issue month` in ('06','07','08') then 'Summer'
                      end as `season`,`Violation Code` from pv17_view
                      ")
createOrReplaceTempView(season,'season_view')

head(SparkR::sql("select season,count(*) as count from season_view group by season"
))
#season   count                                                                
#1 Spring 2880675
#2 Summer 2606196
#3   Fall 2830770
#4 Winter 2485324

head(SparkR::sql("(select season,`Violation Code`,count(*) as count from season_view
                 where season='Spring'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                 (select season,`Violation Code`,count(*) as count from season_view
                 where season='Summer'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                 (select season,`Violation Code`,count(*) as count from season_view
                 where season='Fall'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 union all
                 (select season,`Violation Code`,count(*) as count from season_view
                 where season='Winter'
                 group by season,`Violation Code`
                 order by season,count(*) desc limit 3)
                 "),num=12L)

# Question : 7

#For 2015
head(SparkR::sql("select `Violation Code` ,count(*) as count from pv15_view group by `Violation Code`
                 order by count(*) desc limit 3"))
#21,38,14
#21 fine : (65+45)/2 : 55$
#38 fine : (65+35)/2 : 50$
#14 fine : 115$

#Lets check the total amount collected
head(SparkR::sql("select `Violation Code` ,count(*) as count,count(*)*55 as Fine from pv15_view 
                  where `Violation Code`=21
                  group by `Violation Code` 
                 union all
                 select `Violation Code` ,count(*) as count,count(*)*50 as Fine from pv15_view 
                  where `Violation Code`=38
                  group by `Violation Code` 
                 union all
                 select `Violation Code` ,count(*) as count,count(*)*115 as Fine from pv15_view 
                  where `Violation Code`=14
                  group by `Violation Code` 
                 "))
#Violation Code   count      Fine                                              
#1             21 1630912  89700160
#2             38 1418627  70931350
#3             14  988463 113673245

#For 2016
head(SparkR::sql("select `Violation Code` ,count(*) as count from pv16_view group by `Violation Code`
                 order by count(*) desc limit 3"))
#21,36,38
#21 fine : 55$
#36 fine :50$
#38 fine : 50$

#Lets check the total amount collected
head(SparkR::sql("select `Violation Code` ,count(*) as count,count(*)*55 as Fine from pv16_view 
                 where `Violation Code`=21
                 group by `Violation Code` 
                 union all
                 select `Violation Code` ,count(*) as count,count(*)*50 as Fine from pv16_view 
                 where `Violation Code`=36
                 group by `Violation Code` 
                 union all
                 select `Violation Code` ,count(*) as count,count(*)*50 as Fine from pv16_view 
                 where `Violation Code`=38
                 group by `Violation Code` 
                 "))
#Violation Code   count     Fine                                               
#1             21 1530787 84193285
#2             36 1253511 62675550
#3             38 1143438 57171900

#For 2017
head(SparkR::sql("select `Violation Code` ,count(*) as count from pv17_view group by `Violation Code`
                 order by count(*) desc limit 3"))
#21,36,38
#21 fine : 55$
#36 fine :50$
#38 fine : 50$

#Lets check the total amount collected
head(SparkR::sql("select `Violation Code` ,count(*) as count,count(*)*55 as Fine from pv17_view 
                 where `Violation Code`=21
                 group by `Violation Code` 
                 union all
                 select `Violation Code` ,count(*) as count,count(*)*50 as Fine from pv17_view 
                 where `Violation Code`=36
                 group by `Violation Code` 
                 union all
                 select `Violation Code` ,count(*) as count,count(*)*50 as Fine from pv17_view 
                 where `Violation Code`=38
                 group by `Violation Code` 
                 "))
#Violation Code   count     Fine                                               
#1             21 1528588 84072340
#2             36 1400614 70030700
#3             38 1062304 53115200