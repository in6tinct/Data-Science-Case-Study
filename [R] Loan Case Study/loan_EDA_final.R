#Load Libraries
#install.packages('lubridate')
library(lubridate)
library(ggplot2)

loan <- read.csv('loan.csv',stringsAsFactors = FALSE)

#need year/quarter wise loan details
loan$issue_date <- paste('01-',loan$issue_d,sep = '')
loan$issue_date <- as.Date(loan$issue_date,"%d-%b-%y")
loan$issue_quarter <- quarters(loan$issue_date)
loan$issue_year <- format(loan$issue_date,'%Y')
loan$subgrade_no <- substring(loan$sub_grade,2)

#remove URL
loan$url <- NULL

#remove column with duplicate values
loan <- loan[, !apply(loan , 2 , function(x)
  length(unique(x)) == 1)]

#rounding off the funded_amt_inv, annual_inc
loan$funded_amnt_inv <- round(loan$funded_amnt_inv)
loan$installment <- round(loan$installment)
loan$annual_inc <- round(loan$annual_inc)
loan$dti <- round(loan$dti)
loan$total_pymnt <- round(loan$total_pymnt)
loan$total_pymnt_inv <- round(loan$total_pymnt_inv)
loan$total_rec_prncp <- round(loan$total_rec_prncp)
loan$total_rec_int <- round(loan$total_rec_int)
loan$last_pymnt_amnt <- round(loan$last_pymnt_amnt)

#removing the months from the term
loan$term <- substr(loan$term, 2, 3)

#removing percent symbol from int_rate and revol_util
loan$int_rate <- as.character(loan$int_rate)
loan$int_rate <-
  as.numeric(substr(loan$int_rate, 0, nchar(loan$int_rate) - 1))

loan$revol_util <- as.character(loan$revol_util)
loan$revol_util <-
  as.numeric(substr(loan$revol_util, 0, nchar(loan$revol_util) - 1))

### Derived Column loan_amnt_annual_inc_ratio rounded off %
loan['loan_amnt_annual_inc_ratio'] = round((loan$loan_amnt / loan$annual_inc) * 100)

loan['monthly_income'] <- (loan$annual_inc / 12)
loan['monthly_installment_monthly_inc_perc'] <-
  round((loan$installment / loan$monthly_income) * 100)

####Data cleaning completes here so we are writing all the data into a new file modified_loan
write.csv(loan, "modified_loan.csv")

#univariate analysis
#on home_ownership column
ggplot(loan, aes(x = loan_status)) + geom_histogram(binwidth = 1 , stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)

#home ownership with loan statuses, Charged off is high with Mortgage,Rent
ggplot(loan, aes(x = loan_status,)) + geom_bar(stat = "count") + facet_wrap(~ loan$home_ownership)

#loan amount
average_loan_amount <- mean(loan$loan_amnt)
average_loan_amount
#average loan_amount is 11219.44
#number of members who want loan amount more than this average
more_than_average_loan_amount <-
  length(loan$loan_amnt[loan$loan_amnt > average_loan_amount])
more_than_average_loan_amount
#hence 16188 members have requested loan amount more than average amount

#funded_amt
average_funded_amt <- mean(loan$funded_amnt)
average_funded_amt
#so the average funded amount is 10947.71
#So the average of the loan_amnt is greater than the average of funded_amt
#number of members who have funded_amt more than average
more_than_average_funded_amount <-
  length(loan$funded_amnt[loan$funded_amnt > average_funded_amt])
more_than_average_funded_amount
#So 16284 members have funded_amt more than its average

#funded_amt_inv
average_funded_amt_inv <- mean(loan$funded_amnt_inv)
average_funded_amt_inv
#the average funded amount investment is 10397.45
#number of members who have funded_amt more than average
more_than_average_funded_amt_inv <-
  length(loan$funded_amnt_inv[loan$funded_amnt_inv > mean(loan$funded_amnt_inv)])
more_than_average_funded_amt_inv
#so 15770 members have funded_amt_inv more than its average

#terms
months_36 <- length(loan$term[loan$term == 36])
months_36
#so 29096 members have 36 months of term
months_60 <- length(loan$term[loan$term == 60])
months_60
#so 10621 members have 60 months of term
ggplot(loan, aes(x = term)) + geom_bar(stat = "count") + geom_text(stat =
                                                                     'count', aes(label = ..count..), vjust = -1)

#interst rate
average_interest_rate <- round(mean(loan$int_rate), 2)
average_interest_rate
#So the average interest rate is 12.02
#members who have interest rate greater than the average
more_than_average_int_rate <-
  length(loan$int_rate[loan$int_rate > average_interest_rate])
more_than_average_int_rate
#So 18748 members have interest rate more than the average int_rate


#installment
average_installment <- round(mean(loan$installment), 2)
average_installment
#the average installment is 324.57
#number of members have more than this average installment
more_than_average_installment <-
  length(loan$installment[loan$installment > average_installment])
more_than_average_installment
#so 16415 members have more than average installment
#annual income
average_annual_income <- mean(loan$annual_inc)
average_annual_income
#the average annual income is 68968.93
more_than_average_annual_income <-
  length(loan$annual_inc[loan$annual_inc > average_annual_income])
more_than_average_annual_income
#so 15011 members have annual income more than the average annual income

######bivariate analysis

#having charged_off as loan_status
charged_off <- subset(loan, loan$loan_status == "Charged Off")
nrow(charged_off)
#So out of 39717 loans granted 5627 were charged off

#home_ownership
ggplot(charged_off, aes(x = home_ownership)) + geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)
ggplot(loan,aes(x=home_ownership ,fill=loan_status)) + geom_bar(position = 'fill')
#So the in charged of people 2839 people are rented and 2327 peoples houses are under mortgage

#Verification status
ggplot(charged_off, aes(x = verification_status)) + geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)
ggplot(loan,aes(x=verification_status ,fill=loan_status)) + geom_bar(position = 'fill')

#addr_state
ggplot(charged_off, aes(x = addr_state)) + geom_bar(stat = "count") + geom_text(stat =
                                                                                  'count', aes(label = ..count..), vjust = -1)
#state CA has more number of charged offs  1125
ggplot(loan,aes(x=addr_state ,fill=loan_status)) + geom_bar(position = 'fill')
#However the ratio is not large

# grade
ggplot(charged_off, aes(x = grade)) + geom_bar(stat = "count") + geom_text(stat =
                                                                             'count', aes(label = ..count..), vjust = -1)

#most of the charged_off cases have the loan grade as B
ggplot(loan,aes(x=grade ,fill=loan_status)) + geom_bar(position = 'fill')
#Lesser the grade, the more likely of being charged off

#term
ggplot(loan,aes(x=term ,fill=loan_status)) + geom_bar(position = 'fill')
#60 months ~ less likety perhaps due to higher loan amount

#purpose
ggplot(charged_off, aes(x = charged_off$purpose)) + geom_bar(stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)
# charged off person with purposes, debt consolidation is higher one in %

#loan count per year
ggplot(loan,aes(x=issue_year ,fill=loan_status)) + geom_bar()
#loan percent per year
ggplot(loan,aes(x=issue_year ,fill=loan_status)) + geom_bar(position = 'fill')
#loan percent per quarter-year
ggplot(loan,aes(x=paste(issue_year,'-',issue_quarter,sep='') ,fill=loan_status)) + geom_bar()

#Loan status vs loan amount
ggplot(loan,aes(x=loan_status,y=loan_amnt)) + geom_boxplot()
#amount of loan no effect

ggplot(loan,aes(y=dti,x=loan_status)) + geom_boxplot()
#Slight indication but nothing objectively true

ggplot(loan,aes(x=factor(pub_rec_bankruptcies) ,fill=loan_status)) + geom_bar(position = 'fill')
#more backruptcies ~ less likely

ggplot(loan,aes(x=verification_status ,fill=loan_status)) + geom_bar(position = 'fill')
#Data shows that more percentage of verified loans are getting defaulted than unverified.

ggplot(loan,aes(x=purpose ,fill=loan_status)) + geom_bar(position = 'fill')
#small_business less likely to pay back due to, well, business!


ggplot(loan,aes(x=sub_grade ,fill=loan_status)) + geom_bar(position = 'fill')
#Grade and subgrade a strong indicator. F grade and below are very risky applicants

ggplot(loan,aes(x=emp_length ,fill=loan_status)) + geom_bar()#position = 'fill')
#not an indicator

#CA and NE looks risky
NE_data <- loan[which(loan$addr_state=='NE'),]
#only 5 records however only 2 out of 5 fully paid the loan

ggplot(loan,aes(x=delinq_2yrs ,fill=loan_status)) + geom_bar(position = 'fill')
#nothing conclusive

ggplot(loan,aes(x=installment ,fill=loan_status)) + geom_histogram()

#correlation between loan_amnt and funded_amnt_inv
plot(
  charged_off$loan_amnt,
  charged_off$funded_amnt_inv,
  main = "correlation between loan_amnt and funded_amnt_inv",
  xlab = "loan_amnt ",
  ylab = "funded_amnt_inv ",
  pch = 19
)
abline(lm(charged_off$loan_amnt ~ charged_off$funded_amnt_inv), col = "red") # regression line (y~x)
cor(loan$loan_amnt, loan$funded_amnt_inv)
#the correlation is 0.95 and clearly states that with increase in loan_amnt there is increase in funded_amnt_inv


#correlation between funded_amnt_inv and installment
plot(
  loan$funded_amnt_inv,
  loan$installment,
  main = "correlation between funded_amnt_inv and installment",
  xlab = "funded_amnt_inv ",
  ylab = "interest ",
  pch = 19
)
cor(loan$funded_amnt_inv, loan$installment)
#the correlation is 0.9 and clearly states that with increase in funded_amnt_inv the installment increses


#View(loan)

### Loan where montly installment is around 5-10% of monthly income on Mortgage/Rent are high defaulters
ggplot(charged_off,
       aes(x = charged_off$monthly_installment_monthly_inc_perc)) + geom_bar(stat = "count")
ggplot(charged_off,
       aes(x = charged_off$monthly_installment_monthly_inc_perc)) + geom_bar(stat = "count", aes(fill =
                                                                                                   charged_off$emp_length)) + facet_wrap( ~ charged_off$home_ownership)

#Status of Loan based on Grade
ggplot(loan) + geom_bar(aes(loan$grade, loan$int_rate, fill = as.factor(loan$loan_status)), 
                        position = "dodge", stat = "summary", fun.y = "mean")


#Credit Card Revolving balance
ggplot(loan) + geom_bar(aes(loan$revol_bal, loan$addr_state, fill = as.factor(loan$grade)), 
                        position = "dodge", stat = "summary", fun.y = "mean")

