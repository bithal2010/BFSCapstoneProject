rm(list = ls())
library(ggplot2)
library(gridExtra)
library(grid)
library(MASS) 
library(car)
library(e1071)
library(caret) 
library(caTools)

 
## objective : Predict the probability of default if credit card is approved? 
## Create a scorecard for each applicant.
## Decide a cut-off by balancing trade-off between approval rate and risk of credit loss. 

#Read given CSV files into individual dataframes and then merging them 
demographic_df<- read.csv(file = 'Demographic data.csv',header = T,stringsAsFactors = T, na.strings = c("NA"))
credit_df<- read.csv(file = 'Credit Bureau data.csv',header = T,stringsAsFactors = T, na.strings =c("NA"))

nrow(demographic_df)
#71295
nrow(credit_df)
#71295

## Observing duplication in unique ID column before joining.
sum(duplicated(demographic_df$Application.ID))
sum(duplicated(credit_df$Application.ID))
##so we have 3 rows in which application id is duplicated.

length(unique(tolower(demographic_df$Application.ID)))
# [1] 71292
length(unique(tolower(credit_df$Application.ID)))
# [1] 71292


demographic_df[duplicated(demographic_df$Application.ID),]
credit_df[duplicated(credit_df$Application.ID),]
### 765011468,653287861,671989187 are duplicate application id in both the datasets.


#Removing the duplicate entry for these application id
demographic_df <- demographic_df[-which(duplicated(demographic_df$Application.ID) == T), ]
credit_df <- credit_df[-which(duplicated(credit_df$Application.ID) == T), ]

nrow(credit_df)
#71292
nrow(demographic_df)
#71292

# Merging by common attributes and by unique rows  
merged_df<- merge(x = unique(demographic_df)
                  , y = unique(credit_df)
                  , by = c("Application.ID", "Performance.Tag"))

nrow(merged_df)
#71292
master_data_backup<-merged_df
# Dropping ID column as it is of no use.
merged_df<-merged_df[,-1]

#Duplicate rows in data - none found.
sum(duplicated(merged_df))

#Finding rows where dependant variable-"Performance.Tag" is not populated. 
rejected_applicants<-merged_df[which( is.na(merged_df$Performance.Tag)),]

nrow(rejected_applicants)/nrow(merged_df) 

# Only 1.9% of the rows have NA values for dependant variable - 'perfromance.tag'
# Assumption 1 - So model should be built on data where credit card was approved(0/1). 
# dependant variable - 'perfromance.tag' = NA for applicants for which credit was not issued in first place.
# So removing these rows. rejected_applicants would be used for score card verification.

data_for_eda <- merged_df[!is.na(merged_df$Performance.Tag) == TRUE,]

str(data_for_eda)

# Getting a summary of master data
summary(data_for_eda)
str(data_for_eda)


############# MISSING AND EMPTY VALUE DETECTION AND TREATMENT############################
missing_val_counts<- sapply(data_for_eda, function(x) sum(is.na(x)))

missing_val_counts

## 3 NAs found in column - "No.of.dependents"
## 1023 NAs in Avgas.CC.Utilization.in.last.12.months
## 1 NA value detected in "No.of.trades.opened.in.last.6.months" column
## 272 NAs in Presence.of.open.home.loan 
## 272 NAs in Outstanding.Balance

# For following columns we handle missing values by asssigning median value to respective NA records.
data_for_eda$No.of.dependents[which(is.na(data_for_eda$No.of.dependents)==1)]<-median(data_for_eda$No.of.dependents, na.rm = T)
data_for_eda$No.of.trades.opened.in.last.6.months[which(is.na(data_for_eda$No.of.trades.opened.in.last.6.months)==1)]=median(data_for_eda$No.of.trades.opened.in.last.6.months, na.rm = T)
data_for_eda$Presence.of.open.home.loan[which(is.na(data_for_eda$Presence.of.open.home.loan)==1)] = median(data_for_eda$Presence.of.open.home.loan,na.rm = T)
data_for_eda$Outstanding.Balance[which(is.na(data_for_eda$Outstanding.Balance)==1)] = median(data_for_eda$Outstanding.Balance,na.rm = T)

## Assumption - 2 :
## NA value in Avgas.CC.Utilization.in.last.12.months  is indicating 
## no usage of CC by user. So lets assign value 0 to these avg-cc-utilization values
data_for_eda$Avgas.CC.Utilization.in.last.12.months[which(is.na(data_for_eda$Avgas.CC.Utilization.in.last.12.months)==1)] = 0

## checking for empty values 
empty_val_counts<- sapply(data_for_eda, function(x) sum(x==" " | x==""))

empty_val_counts
#Gender - 2
#Marital.Status..at.the.time.of.application. - 6 
#Education - 118 
#Profession - 13 
#Type.of.residence - 8 

# Repalcing empty strings with values with maximum freq
data_for_eda$Gender[which(data_for_eda$Gender==" " | data_for_eda$Gender==" ")]<- 'M'
data_for_eda$Marital.Status..at.the.time.of.application.[which(data_for_eda$Marital.Status..at.the.time.of.application.==" " | data_for_eda$Marital.Status..at.the.time.of.application.==" ")]<- 'Married'
data_for_eda$Education[which(data_for_eda$Education==" " | data_for_eda$Education==" ")]<- 'Professional'
data_for_eda$Profession[which(data_for_eda$Profession==" " | data_for_eda$Profession==" ")]<- 'SAL'
data_for_eda$Type.of.residence[which(data_for_eda$Type.of.residence==" " | data_for_eda$Type.of.residence==" ")]<- 'Rented'


######################## outlier detection and treatment ##########################################
##Method to find outliers [from stackoverflow]
FindOutliers <- function(data) {
  lowerq = quantile(data,probs = seq(0,1,0.10))[3]  #20%
  upperq = quantile(data,probs = seq(0,1,0.10))[9]  #80%
  iqr = upperq - lowerq #Or use IQR(data)
  extreme.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.lower = lowerq - (iqr * 1.5)
  # we identify extreme outlier indeces
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
} 

company_recency_outliers <- data_for_eda[FindOutliers(data_for_eda$No.of.months.in.current.company),]
avg_cc_utilization_outliers <- data_for_eda[FindOutliers(data_for_eda$Avgas.CC.Utilization.in.last.12.months),]
last_6mon_trades_outliers <- data_for_eda[FindOutliers(data_for_eda$No.of.trades.opened.in.last.6.months),]
last_12mon_trades_outliers <- data_for_eda[FindOutliers(data_for_eda$No.of.trades.opened.in.last.12.months),]
last_6mon_pl_outliers <- data_for_eda[FindOutliers(data_for_eda$No.of.PL.trades.opened.in.last.6.months),]
last_12mon_pl_outliers <- data_for_eda[FindOutliers(data_for_eda$No.of.PL.trades.opened.in.last.12.months),]
last_6mon_inqr_outliers <- data_for_eda[FindOutliers(data_for_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.),]
last_12mon_inqr_outliers <- data_for_eda[FindOutliers(data_for_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.),]
total_trd_outliers <- data_for_eda[FindOutliers(data_for_eda$Total.No.of.Trades),]

#We are not ceiling outliers to ensure no info loss
# Some outlier samples 
company_recency_outliers$No.of.months.in.current.company
last_6mon_pl_outliers$No.of.PL.trades.opened.in.last.6.months
total_trd_outliers$Total.No.of.Trades


########################Handling Invalid value ###################################
#Invalid negative/zero value for age column populated for some row i.e. 0, -3
invalid_age_index <-which(data_for_eda$Age < 10)

#populating median values for all these rows
### Assumption 2 - So age value substituted with median values where invalid. 
data_for_eda$Age[invalid_age_index] <-median(data_for_eda$Age,na.rm = T)


#Invalid negative/zero value for Income column populated for some row
invalid_income_index <-which(data_for_eda$Income < 0)

#populating median values for all these rows
### Assumption 3 - So Income value substituted with 0 values where invalid. 
data_for_eda$Income[invalid_income_index] <-0

### Assumption 4 - The card has not been used by these 1023 persons,so substituting NA by 0.
data_for_eda$Avgas.CC.Utilization.in.last.12.months[is.na(data_for_eda$Avgas.CC.Utilization.in.last.12.months)] <- 0

### Assumption 5 - No.of.dependents wherever NA,is substituting  by 0.
data_for_eda$No.of.dependents[is.na(data_for_eda$No.of.dependents)] <- 0

### Assumption 6 - Presence.of.open.home.loan wherever NA,is substituting  by 0.
data_for_eda$Presence.of.open.home.loan[is.na(data_for_eda$Presence.of.open.home.loan)] <- 0

### Assumption 7 - Outstanding.Balance wherever NA,is substituting  by 0 value.
data_for_eda$Outstanding.Balance[is.na(data_for_eda$Outstanding.Balance)] <- 0


 
str(data_for_eda) 

event_col<-c("Performance.Tag")  

fact_cols <- c("Gender","Marital.Status..at.the.time.of.application." 
               ,"Education","Profession","Type.of.residence")

numeric_cols<-c('Age','Income','No.of.months.in.current.residence','No.of.months.in.current.company'
                ,'Total.No.of.Trades','Outstanding.Balance','Avgas.CC.Utilization.in.last.12.months'
                ,'No.of.times.90.DPD.or.worse.in.last.6.months','No.of.times.60.DPD.or.worse.in.last.6.months','No.of.times.30.DPD.or.worse.in.last.6.months'
                ,'No.of.times.90.DPD.or.worse.in.last.12.months','No.of.times.60.DPD.or.worse.in.last.12.months','No.of.times.30.DPD.or.worse.in.last.12.months'
                ,'No.of.trades.opened.in.last.6.months','No.of.trades.opened.in.last.12.months'
                ,'No.of.PL.trades.opened.in.last.6.months','No.of.PL.trades.opened.in.last.6.months'
                ,'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'
                ,'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'
                ,'No.of.PL.trades.opened.in.last.12.months','Presence.of.open.home.loan','Presence.of.open.auto.loan')



##################### Univariate analysis######################################

out_df<-data.frame(prop.table(table(data_for_eda$Performance.Tag)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity",col='blue')+ xlab("Performance.Tag") + ylab("Frequency") 

out_df<-data.frame(prop.table(table(data_for_eda$Gender)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")+ xlab("Gender") + ylab("Frequency") 

out_df<-data.frame(prop.table(table(data_for_eda$Education)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")+ xlab("Education") + ylab("Frequency") 

out_df<-data.frame(prop.table(table(data_for_eda$Profession)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")+ xlab("Profession") + ylab("Frequency") 

out_df<-data.frame(prop.table(table(data_for_eda$Marital.Status..at.the.time.of.application.)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")+ xlab("Marital.Status") + ylab("Frequency") 

 
out_df<-data.frame(prop.table(table(data_for_eda$Type.of.residence)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")+ xlab("Type.of.residence") + ylab("Frequency") 
 

ggplot(data_for_eda,aes(Age))+geom_histogram()
boxplot(data_for_eda$Age,horizontal = T)  
## most users are in late 30 to early 50 age range.
## Some outliers(very small age value, may be invalid) values are present.


hist(data_for_eda$Income, xlab = "Income")
boxplot(data_for_eda$Income,horizontal = T) 
## Most users are in 15 to 40 income range.
## Not many outliers found.
ggplot(data_for_eda[which(data_for_eda$Performance.Tag == 1),] , aes(x = Income)) +
  geom_density() 
 

 
hist(data_for_eda$No.of.months.in.current.company, xlab = "No.of.months.in.current.company")
boxplot(data_for_eda$No.of.months.in.current.company,horizontal = T) 
## Most users are new job holders with 0-5yr experience. 
## population size is low in high experience category.
## Some outliers do exist.

summary(data_for_eda$No.of.times.90.DPD.or.worse.in.last.6.months)
hist(data_for_eda$No.of.times.90.DPD.or.worse.in.last.6.months, xlab = "No.of.times.90.DPD.or.worse.in.last.6.months")
boxplot(data_for_eda$No.of.times.90.DPD.or.worse.in.last.6.months,horizontal = T)
# Most people have no such overdues 
# Among the very less people who have  90 days overdue, 
##repeating offenders population size is very very small.


summary(data_for_eda$No.of.times.60.DPD.or.worse.in.last.6.months)
hist(data_for_eda$No.of.times.60.DPD.or.worse.in.last.6.months, xlab = "No.of.times.60.DPD.or.worse.in.last.6.months")
boxplot(data_for_eda$No.of.times.60.DPD.or.worse.in.last.6.months,horizontal = T)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.
# compared to 90 days overdues, population size is higher

summary(data_for_eda$No.of.times.30.DPD.or.worse.in.last.6.months)
hist(data_for_eda$No.of.times.30.DPD.or.worse.in.last.6.months, xlab = "No.of.times.30.DPD.or.worse.in.last.6.months")
boxplot(data_for_eda$No.of.times.30.DPD.or.worse.in.last.6.months,horizontal = T)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.

summary(data_for_eda$No.of.times.90.DPD.or.worse.in.last.12.months)
hist(data_for_eda$No.of.times.90.DPD.or.worse.in.last.12.months, xlab = "No.of.times.90.DPD.or.worse.in.last.12.months")
boxplot(data_for_eda$No.of.times.90.DPD.or.worse.in.last.12.months,horizontal = T)
# Most people have no such overdues 
# Among the very less people who have  90 days overdue, repeating offenders population size is very very small.


summary(data_for_eda$No.of.times.60.DPD.or.worse.in.last.12.months)
hist(data_for_eda$No.of.times.60.DPD.or.worse.in.last.12.months, xlab = "No.of.times.60.DPD.or.worse.in.last.12.months")
boxplot(data_for_eda$No.of.times.60.DPD.or.worse.in.last.12.months,horizontal = T)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.
# compared to 90 days overdues, population size is higher

summary(data_for_eda$No.of.times.30.DPD.or.worse.in.last.12.months)
hist(data_for_eda$No.of.times.30.DPD.or.worse.in.last.12.months, xlab = "No.of.times.30.DPD.or.worse.in.last.12.months")
boxplot(data_for_eda$No.of.times.30.DPD.or.worse.in.last.12.months,horizontal = T)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.

summary(data_for_eda$Avgas.CC.Utilization.in.last.12.months)
hist(data_for_eda$Avgas.CC.Utilization.in.last.12.months, xlab = "avg cc utilization")
boxplot(data_for_eda$Avgas.CC.Utilization.in.last.12.months,horizontal = T)
## most users are utilizing only upto 20% of card upper limit, 
## population size with proper 25 to 60 % card utilization is similar
## Left skewed ..outliers do exist.
ggplot(data_for_eda[which(data_for_eda$Performance.Tag == 1),] , aes(x = Avgas.CC.Utilization.in.last.12.months)) +
  geom_density()




summary(data_for_eda$No.of.trades.opened.in.last.6.months)
hist(data_for_eda$No.of.trades.opened.in.last.6.months, xlab = "No.of.trades.opened.in.last.6.months")
boxplot(data_for_eda$No.of.trades.opened.in.last.6.months,horizontal = T)
# most users have 0-4 trades opened in last 6 mon.
# Outlier do exist.

summary(data_for_eda$No.of.trades.opened.in.last.12.months)
hist(data_for_eda$No.of.trades.opened.in.last.12.months, xlab = "No.of.trades.opened.in.last.12.months")
boxplot(data_for_eda$No.of.trades.opened.in.last.12.months,horizontal = T)
# most users have 0-10 trades opened in last 12 mon.
# Outlier do exist.

summary(data_for_eda$No.of.PL.trades.opened.in.last.6.months)
hist(data_for_eda$No.of.PL.trades.opened.in.last.6.months, xlab = "No.of.PL.trades.opened.in.last.6.months")
boxplot(data_for_eda$No.of.PL.trades.opened.in.last.6.months,horizontal = T)
# most users have 0-3 PL opened in last 12 mon.
# Very few Outlier are there.

summary(data_for_eda$No.of.PL.trades.opened.in.last.12.months)
hist(data_for_eda$No.of.PL.trades.opened.in.last.12.months, xlab = "No.of.PL.trades.opened.in.last.12.months")
boxplot(data_for_eda$No.of.PL.trades.opened.in.last.12.months,horizontal = T)
# most users have 0-6 trades opened in last 12 mon.
# Outlier might be there.



summary(data_for_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
hist(data_for_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
boxplot(data_for_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,horizontal = T)
# most users have 0-4 trades opened in last 6 mon.
# Outlier might be there.


summary(data_for_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
hist(data_for_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., xlab = "Autoloans-6mon")
boxplot(data_for_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.,horizontal = T)
# most users have 0-5 trades opened in last 12 mon.
# Outlier are present.


str(data_for_eda)

summary(data_for_eda$Total.No.of.Trades)
hist(data_for_eda$Total.No.of.Trades, xlab = "Total.No.of.Trades")
boxplot(data_for_eda$Total.No.of.Trades,horizontal = T)
# most users have 0-10 trades in total
# Outlier are there.

summary(data_for_eda$Outstanding.Balance)
hist(data_for_eda$Outstanding.Balance, xlab = "Outstanding.Balance")
boxplot(data_for_eda$Outstanding.Balance,names = "Outstanding.Balance",horizontal = T)
# 0-200000 range higher no of users
# 300k upwards lower no of users
# most users are starting to repay their loans
ggplot(data_for_eda[which(data_for_eda$Performance.Tag == 1),] , aes(x = Outstanding.Balance)) +
  geom_density()



############################ WOE /IV analysis ############################
#WOE The weight of evidence tells the predictive power of 
# an independent variable in relation to the dependent variable.
#IV measures the strength of that relationship.
## Getting ready fro deriving WOE /IV values for all columns
# install.packages("Information")
#install.packages("gridExtra")
#install.packages("grid")
#Information Value	Predictive Power
#< 0.02	useless for prediction
#0.02 to 0.1	Weak predictor
#0.1 to 0.3	Medium predictor
#0.3 to 0.5	Strong predictor
#>0.5	Suspicious or too good to be true


library(Information)

IV <- create_infotables(data=data_for_eda, y="Performance.Tag", bins=10, parallel=T)

IV$Tables$Age

head(IV)
 
IV_Value = data.frame(IV$Summary)

grid.table(IV$Summary[seq(from=1,to=20,by=1),], rows=NULL)
 
plotFrame <- IV$Summary[order(-IV$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable, levels = plotFrame$Variable[order(-plotFrame$IV)])
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "blue", fill = "blue") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

plotFrame
 
#From above WOE analysis below parameters show relatively higher significance
# Variable                                          IV
# Avgas.CC.Utilization.in.last.12.months            2.993909e-01
#No.of.trades.opened.in.last.12.months              2.979855e-01
#No.of.PL.trades.opened.in.last.12.months           2.959382e-01
#No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. 2.953999e-01
#Outstanding.Balance                                2.427715e-01
#No.of.times.30.DPD.or.worse.in.last.6.months       2.415777e-01
#Total.No.of.Trades                                 2.366340e-01
#No.of.PL.trades.opened.in.last.6.months            2.197726e-01
#No.of.times.90.DPD.or.worse.in.last.12.months      2.139246e-01
#No.of.times.60.DPD.or.worse.in.last.6.months       2.058494e-01
#No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. 2.051641e-01
#No.of.times.30.DPD.or.worse.in.last.12.months      1.982677e-01
#No.of.trades.opened.in.last.6.months               1.860467e-01
#No.of.times.60.DPD.or.worse.in.last.12.months      1.855141e-01
#No.of.times.90.DPD.or.worse.in.last.6.months       1.601541e-01
#No.of.months.in.current.residence                  7.896308e-02
#Income                                             4.236710e-02
#No.of.months.in.current.company                    2.176502e-02
#Presence.of.open.home.loan                         1.695583e-02


##################Correlation analysis#############################

#install.packages('corrplot')

library(corrplot)

cor_df<- data_for_eda[,numeric_cols]

corr_index<- cor(cor_df) 
 
corrplot(corr_index, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.01, tl.col = 'black',
         order = "hclust", diag = FALSE)

colnames(cor_df)
 
#Inferring correlations visible from the plot>>>

#Income >> Negative >> with all other attribute
#"Total.No.of.Trades" >> Positive >>  "Outstanding.Balance"                                            
#Outstanding.Balance" >> Positive >>  "Avgas.CC.Utilization.in.last.12.months" 
#"Avgas.CC.Utilization.in.last.12.months"     >> Positive >>   "No.of.times.90.DPD.or.worse.in.last.6.months"  
#"No.of.times.90.DPD.or.worse.in.last.6.months"   >> Positive >>    "No.of.times.60.DPD.or.worse.in.last.6.months"                  
#"No.of.times.30.DPD.or.worse.in.last.12.months"  >> Positive >>  "No.of.trades.opened.in.last.6.months" 
#"No.of.trades.opened.in.last.6.months"   >> Positive >> "No.of.trades.opened.in.last.12.months" 
#"No.of.trades.opened.in.last.12.months" >> Positive >>  "No.of.PL.trades.opened.in.last.6.months" 
#"No.of.PL.trades.opened.in.last.6.months.1" >> Positive >>   "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans." 
#"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."  >> Positive >>  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans."
#"No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.">> Positive >>  "No.of.PL.trades.opened.in.last.12.months"  



#################Bi/multi-variate analysis##################################

ggplot(data_for_eda, aes(x = Avgas.CC.Utilization.in.last.12.months, y = No.of.PL.trades.opened.in.last.12.months, group=Performance.Tag, color=Performance.Tag))+
  geom_line(stat='summary', fun.y='mean') +  
  geom_point(stat='summary', fun.y='mean')
## No of PL-trades opened is relatively higher for default users. 

ggplot(data=data_for_eda, aes(x=No.of.times.90.DPD.or.worse.in.last.6.months, y=Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
 ## For default users Avg-CC-utilization is overall higher , Also CC-usage is going high with increasing DPD values. 

ggplot(data=data_for_eda, aes(x=Total.No.of.Trades, y=Outstanding.Balance, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean')
 ## Total no of trades is overall in higherno nos for default users. 
## Also outstanding balance is relatively higher for most of default users.
 
ggplot(data=data_for_eda, aes(x=Income, y=Outstanding.Balance, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean')  
##For defaulters Outstanding balance is higher.
# No upward/downward trend for outstanding balance with increasing income.
#If outstanding is more than 12.5lakh its a matter of concern.



ggplot(data_for_eda, aes(x = Income, y = Avgas.CC.Utilization.in.last.12.months, group=Performance.Tag, color=Performance.Tag))+
  geom_line(stat='summary', fun.y='mean') +  
  geom_point(stat='summary', fun.y='mean')
##With increasing income avg-cc-usage decreases for whole population.
## If avg cc usage is >40 for a low income, >30 for middle income, >25 for higher income,they should be looked at.

ggplot(data=data_for_eda, aes(x=Income, y=No.of.times.90.DPD.or.worse.in.last.6.months, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
##With increasing Income, DPD nos are decreasing. 
##Also for defaulting users DPD nos are way higher.
## High no of defaulters are in lower to medium income range. 

ggplot(data=data_for_eda, aes(x=Income, y=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
##With increase in income no of inquiries are decreasing for non defaulters.
##With increase in income no of inquiries relatively higher for defaulters.

ggplot(data=data_for_eda, aes(x=No.of.dependents, y=Income, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
##Income per no of dependants is very low for defaulters comapared to non-defaulters. 


ggplot(data=data_for_eda, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., y=Total.No.of.Trades , group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
##With increasing no of inquiries in last 12months, 
## total no of trades increases, then gradually it becomes constant.
## for default users total no of trades is higher.


################Scaling numeric columns & creating dummies for factor attributes#############

table(data_for_eda$Performance.Tag)
prop.table(table(data_for_eda$Performance.Tag))
## Only 4% of observations are under default category. 
## So it is a highly imbalanced data which would result  in-effictive models if not treated properly.

# data before scaling 
table(data_for_eda$Performance.Tag)

#creating a copy to be used during decision tree/Random forest processing.
master_df<- data_for_eda

data_for_scaling<-data.frame(sapply(data_for_eda[numeric_cols], scale))

str(data_for_scaling)

data_for_creating_dummies <- data_for_eda[fact_cols] 

str(data_for_creating_dummies)  
# creating dummy variables for factor attributes
dummies<- data.frame(sapply(data_for_creating_dummies,function(x) data.frame(model.matrix(~x-1,data =data_for_creating_dummies))[,-1])) 

# combine all relevant columns to build final training data
final_df<- cbind(data_for_eda[event_col],data_for_scaling,dummies)

final_df$Performance.Tag<-as.factor(final_df$Performance.Tag)

str(final_df)

# Above "final_df" dataset would be used for Logistic and SVM modelling both.

###############################################################
## Before going to apply ove/under/synthetic sampling only on training data.
## Hence we need to devide the main data to train and test data.
## and then apply sampling on the training data only. 
## Otherwise there is a risk of - having unreal synthetic data in the test dataset.
######splitting whole date to separate test data for model evaluation######

set.seed(100)

split_indices <- sample.split(final_df, SplitRatio = 7/10)

data_for_sampling <- final_df[split_indices, ]
 
test<- final_df[!split_indices, ]
 

###########  SMOTE(synthetic minority oversampling technique) by ROSE package
#SMOTE algorithm creates artificial data based on feature space (rather than data space) 
# similarities from minority samples. It generates a random set of minority class observations 
# to shift the classifier learning bias towards minority class.


#Generate data synthetically to avoid errors related to explicitely mentioned probability
library(ROSE)

train <- ROSE(Performance.Tag ~ ., data = data_for_sampling, seed = 1)$data

table(train$Performance.Tag)

#-----------------------------------------------------------------------------------------  
##Lets create a Logistic Model: Logistic Regression#####################
#-----------------------------------------------------------------------------------------  

logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")


logistic_3<- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                   No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.PL.trades.opened.in.last.6.months.1 + No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan + 
                   Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application..xMarried + 
                   Education.xOthers + Profession.xSE + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xOwned
                 , family = "binomial", data = train)

summary(logistic_3)

vif(logistic_3)

sort(vif(logistic_3),decreasing = T)

# removing  due to high vif value  - No.of.PL.trades.opened.in.last.6.months.1 

logistic_4<- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                   No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months + No.of.times.60.DPD.or.worse.in.last.6.months + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan + 
                   Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application..xMarried + 
                   Education.xOthers + Profession.xSE + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xOwned
                 , family = "binomial", data = train)

summary(logistic_4)

sort(vif(logistic_4),decreasing = T)

# removing  due to high vif value , relatively lower significance -   No.of.times.60.DPD.or.worse.in.last.6.months  

logistic_5<- glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                   No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan + 
                   Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application..xMarried + 
                   Education.xOthers + Profession.xSE + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xOwned
                 , family = "binomial", data = train)

summary(logistic_5)

sort(vif(logistic_5),decreasing = T)

# VIF VALUES ARE LOOKING IS ACCEPTABLE RANGE, LETS REMOVE LOW SIGNIFICANCE

# REMOVING 'Type.of.residence.xCompany.provided' due to low significance -
 

logistic_6<-  glm(Performance.Tag ~ Income + No.of.months.in.current.residence + 
                    No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                    No.of.times.90.DPD.or.worse.in.last.6.months  + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                    No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan + 
                    Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application..xMarried + 
                    Education.xOthers + Profession.xSE + 
                    Type.of.residence.xOwned
                  , family = "binomial", data = train)

summary(logistic_6)


# Removing No.of.months.in.current.residence
logistic_7 <- glm(Performance.Tag ~ Income  + 
                    No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                    No.of.times.90.DPD.or.worse.in.last.6.months  + 
                    No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                    No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    No.of.PL.trades.opened.in.last.12.months + Presence.of.open.home.loan + 
                    Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application..xMarried + 
                    Education.xOthers + Profession.xSE + 
                    Type.of.residence.xOwned
                  , family = "binomial", data = train)

summary(logistic_7)

 
# removing  Presence.of.open.home.loan 

logistic_8<-glm(Performance.Tag ~ Income  + 
                   No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months  + 
                   Presence.of.open.auto.loan + Marital.Status..at.the.time.of.application..xMarried + 
                   Education.xOthers + Profession.xSE + 
                   Type.of.residence.xOwned
                 , family = "binomial", data = train)

summary(logistic_8)
 
  
# Removing Marital.Status..at.the.time.of.application..xMarried 

logistic_9<- glm(Performance.Tag ~ Income  + 
                   No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months  + 
                   Presence.of.open.auto.loan  + 
                   Education.xOthers + Profession.xSE + 
                   Type.of.residence.xOwned
                 , family = "binomial", data = train)

summary(logistic_9)



# Removing Profession.xSE

logistic_10<-glm(Performance.Tag ~ Income  + 
                   No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months  + 
                   Presence.of.open.auto.loan  + 
                   Education.xOthers + 
                   Type.of.residence.xOwned
                 , family = "binomial", data = train)

summary(logistic_10)

# removing Type.of.residence.xOwned
logistic_11<-glm(Performance.Tag ~ Income  + 
                   No.of.months.in.current.company + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months  + 
                   Presence.of.open.auto.loan  + 
                   Education.xOthers 
                 , family = "binomial", data = train)

summary(logistic_11)

# removing Presence.of.open.auto.loan 
logistic_12<-glm(Performance.Tag ~ Income   + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months  +Education.xOthers 
                 , family = "binomial", data = train)

summary(logistic_12)


# removing Education.xOthers 
logistic_13<-glm(Performance.Tag ~ Income   + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months
                 , family = "binomial", data = train)

summary(logistic_13)

#removing No.of.trades.opened.in.last.6.months 
logistic_14<-glm(Performance.Tag ~ Income   + Avgas.CC.Utilization.in.last.12.months + 
                   No.of.times.90.DPD.or.worse.in.last.6.months  + 
                   No.of.times.30.DPD.or.worse.in.last.6.months + No.of.times.90.DPD.or.worse.in.last.12.months + 
                   No.of.times.60.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.PL.trades.opened.in.last.12.months
                 , family = "binomial", data = train)

summary(logistic_14)

##  significance is very high now for existing attributes.Lets take this model as final LR model for now.
final_lr_model <- logistic_14

 
###  #### #### Model Evaluation with Test Data #### #### ####
 
#predicted probabilities  for test data
test_pred = predict(final_lr_model, type = "response", newdata = test[,-1])
 

# Let's use the probability cutoff of 50%.
test_pred_default <- as.factor(ifelse(test_pred >= 0.50, 1,0))
test_actual_default <-  as.factor(ifelse(test$Performance.Tag==1,1,0))

conf_mtr_50_cutoff <- confusionMatrix(test_pred_default, test_actual_default)

conf_mtr_50_cutoff 
 
#     0     1     -- Actual
#0 11928   333
#1  8107   591

#Accuracy : 0.6004       
#Sensitivity : 0.59896        
#Specificity : 0.63152  

#########################################################################################
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default <- as.factor(ifelse(test_pred >= cutoff, 1,0))
  conf <- confusionMatrix(predicted_default, test_actual_default)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.95,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
grid(50, lwd = 2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col= "blue",lwd=2)
box()
legend("topright", legend=c("Sensitivity","Specificity","Accuracy"),
       col=c(2,"darkgreen",4,"darkred"), cex=0.5,lwd=c(3,3,3,3), text.font=14,y.intersp = 0.3)

test_pred_optimal<- as.factor(ifelse(test_pred >= 0.502, 1,0))
optimal_conf <- confusionMatrix(test_pred_optimal, test_actual_default)
optimal_conf

    # for 50.2% optimal threshold,  
    # accuracy = 60.52% , 
    # specificity = 61.96% , 
    # sensitivity = 60.45%
# So cutoff value is 0.502 for final model
 



####################### KS -statistic - Test Data ###################

library(ROCR)

pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)   ## 0.224

#KS-statistic is 22.4% 

#ROC Curve
 
auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
##Area under curve is : 0.612

pd <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(pd ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Logistic Regression Model",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))+
  annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc_ROCR, 3))) 

gini<-(auc_ROCR*2)-1
gini

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
 
lift_decile_info = lift(test_actual_default, test_pred, groups = 10)

print(lift_decile_info)
write.csv(lift_decile_info, "lift.csv", row.names = FALSE)

###Plotting Gain Chart
ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Gain),color='#FF6666', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Gain),color='#07843b',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")


###Plotting Lift Chart
ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#07843b', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#FF6666',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")

#-----------------------------------------------------------------------------------------  
############Linear SVM #################
#SVM needs all numeric attribubtes so reusing the SMOTE balanced train dataset.
#-----------------------------------------------------------------------------------------  
library(plyr)
library(caret)
library(kernlab)
library(readr)
library(caret)
library(caTools)

train_svm <-train
test_svm<-test

nrow(train_svm)
table(train_svm$Performance.Tag)
str(train_svm)
nrow(test_svm)
table(test_svm$Performance.Tag)
str(test_svm)

#As It would take a lot of time for modeling on the  whole train data, So we are taking 10% sample of the data and
#building the model which would make the computation faster.

train.indices = sample(2:nrow(train_svm), 0.1*nrow(train_svm))
train_svm = train_svm[train.indices, ]


# 4. Model Building

#--------------------------------------------------------------------
# 4.1 Linear model - SVM  at Cost(C) = 1
#####################################################################

model_1 <- ksvm(Performance.Tag ~ ., data = train_svm,scale = TRUE,C=1)
linear_prediction<- predict(model_1, test_svm)
confusionMatrix(linear_prediction, test_svm$Performance.Tag)


# Accuracy : 0.7927  
# Sensitivity : 0.81330         
# Specificity : 0.34457


#--------------------------------------------------------------------
# 4.2 Linear model - SVM  at Cost(C) = 10
#####################################################################

# Model with C =10. ( a high value of c will not accommodate many misclassifications -  an overfit model)

model_2 <- ksvm(Performance.Tag ~ ., data = train_svm,scale = TRUE,C=10)
linear_prediction<- predict(model_2, test_svm)
confusionMatrix(linear_prediction, test_svm$Performance.Tag)

# Accuracy : 0.9206
# Sensitivity : 0.95888        
# Specificity : 0.08587

#Improve in Sensitivity and accuracy but drop in  Specificity when we change C=1 and C=10


###############################Using Linear Kernel###############################
Model_linear <- ksvm(Performance.Tag~ ., data = train_svm, scale = FALSE, kernel = "vanilladot")
Eval_linear<- predict(Model_linear, test_svm)

#confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test_svm$Performance.Tag)

# Accuracy : 0.5966
# Sensitivity : 0.59617          
# Specificity : 0.60652 


############   Hyperparameter tuning and Cross Validation #####################

# We will use the train function from caret package to perform Cross Validation. 
#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl <- trainControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"


####   Hyperparameter tuning and Cross Validation  using Linear SVM for C = 1 to 5 ####
#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
# making a grid of C values. 
grid <- expand.grid(C=seq(1, 5, by=1))
# Performing 5-fold cross validation
fit.svmLinear <- train(Performance.Tag~., data=train_svm, method="svmLinear", metric=metric, 
                       tuneGrid=grid, trControl=trainControl)

# Printing cross validation result
print(fit.svmLinear)
# Accuracy was used to select the optimal model using  the largest value.
# The final value used for the model was C = 1.

# Plotting "fit.svm" results
plot(fit.svmLinear)

# Valdiating the model after cross validation on test_svm data
eval_svmLinear<- predict(fit.svmLinear, test_svm)
confusionMatrix(eval_svmLinear, test_svm$Performance.Tag)

# Accuracy : 0.5966  
# Sensitivity : 0.59617          
# Specificity : 0.60652
# For the linear kernel it looks the most stable and the values for sensitivity ,specificity and accuracy looks consistent



######################################Using Polynomial Kernel##############################################
# Using Polynomial Kernel : degree=2
Model_poly <- ksvm(Performance.Tag~ ., data = train_svm, scale = FALSE, kernel = "polydot",kpar=list(degree=2))
# Predicting the model results 
Eval_Poly<- predict(Model_poly, test_svm)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_Poly,test_svm$Performance.Tag)

# Accuracy : 0.9393
# Sensitivity : 0.98044         
# Specificity : 0.04348 

# The specificity looks really low

#### Hyperparameter tuning and Cross Validation : using Polynomial Kernel ####
# We will use the train function from caret package to perform Cross Validation.
#traincontrol function Controls the computational nuances of the train function.
# i.e. method =  CV means  Cross Validation.
#      Number = 2 implies Number of folds in CV.

trainControl<-trainControl(method = "cv", number = 5, returnResamp = "all")
# train function takes Target ~ Prediction, Data, Method = Algorithm
# trcontrol = Our traincontrol method.
fit.svm_poly  <- train(Performance.Tag ~ ., data = train_svm, method = "svmPoly", trControl = trainControl, preProc = c("center", "scale"))
print(fit.svm_poly)

# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were degree = 2, scale = 0.1 and C = 1.
# Accuracy 0.6730003
plot(fit.svm_poly)

# Predicting the model results 
Eval_fit.svm_poly<- predict(fit.svm_poly, test_svm)
#confusion matrix - RBF Kernel
confusionMatrix(Eval_fit.svm_poly,test_svm$Performance.Tag)

# Accuracy : 0.9333 
# Sensitivity : 0.97355         
# Specificity : 0.05761 

###############################Using RBF Kernel###############################
Model_RBF <- ksvm(Performance.Tag~ ., data = train_svm, scale = FALSE, kernel = "rbfdot")
RBF_linear<- predict(Model_RBF, test_svm)

#confusion matrix - RBF Kernel
confusionMatrix(RBF_linear,test_svm$Performance.Tag)

# Accuracy : 0.7916 
# Sensitivity : 0.81206        
# Specificity : 0.34674 


####   Hyperparameter tuning and Cross Validation  using Radial SVM for C = 1 to 5 ####
trainControl <- trainControl(method="cv", number=5)
grid <- expand.grid(.sigma=c(0.025, 0.05,0.075,0.1), .C=c(0.1,0.5,1,2,3,4,5))

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(Performance.Tag~., data=train_svm, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm)
# Accuracy was used to select the optimal model using  the largest value.
# The final values used for the model were sigma = 0.1 and C = 2.
# 0.5081800

plot(fit.svm)

# Valdiating the model after cross validation on test_svm data
eval_svmRadial<- predict(fit.svm, test_svm)
confusionMatrix(eval_svmRadial, test_svm$Performance.Tag)

# Accuracy : 0.9561
# Sensitivity : 1.0000         
# Specificity : 0.0000

# The accuracy is too high on test_svm data for RBF Kernel and specificity is too low


#####################Conclusion from SVM######################################
# After performing five fold cross validation we came to the conclusion that radial kernel performs the best in terms of accuracy
# but its specificity is too bad
# Linear kernel accuracy: ~59.66 %
# RBF kernel accuracy: ~ 95.61%
# Polynomial accuracy: ~79.16%
# Accuracy = 95.61% is the best accuracy so far amongst all the models
# The model gives the best accuracy in case of radial kernel during cross validation.
# The final values used for the model are sigma = 0.1 and C = 2.
# Though it's hard to conclude anything from svm model as the the accuracy ,specificity and sensitivity are not consistent
# As we are doing this modelling on a small dataset due to respource crunch we are not considering it our chosen model.

#########################################################################################  
####################################XGBOOST model############################################################################
## XGBOOST also works with all numeric columns 
## So SMOTE balanced data - train and test  would be resued.
#########################################################################################  

#install.packages('xgboost')
#install.packages('magrittr')
#install.packages('Matrix')
library(xgboost)
library(magrittr) 
library(Matrix)

str(train)
# Create matrix - One-Hot Encoding for Factor variables
trainm <- sparse.model.matrix(Performance.Tag ~ .-1, data = train)
train_label <- as.numeric(as.character(train$Performance.Tag)) 
str(train_label)
train_matrix <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)


testm <- sparse.model.matrix(Performance.Tag~.-1, data = test)
test_label <- as.numeric(as.character(test$Performance.Tag))
test_matrix <- xgb.DMatrix(data = as.matrix(testm), label = test_label)

# Parameters
nc <- length(unique(train_label))

watchlist <- list(train = train_matrix, test = test_matrix)

parameters <- list(
  # General Parameters
  booster            = "gbtree",          
  silent             = 0,                 
  # Booster Parameters
  eta                = 0.01,               
  gamma              = 0,                 
  max_depth          = 10,                 
  colsample_bytree   = 1,                 
  colsample_bylevel  = 1,                 
  lambda             = 1,                 
  alpha              = 0,                 
  # Task Parameters
  objective          = "binary:logistic",   
  eval_metric        = "auc",
  seed               = 1900               
)

xgb.model <- xgb.train(parameters, 
                       train_matrix,
                       nrounds = 150, 
                       watchlist)

#install.packages("reshape")
library(reshape)
melted <- melt(xgb.model$evaluation_log, id.vars="iter")
ggplot(data=melted, aes(x=iter, y=value, group=variable, color = variable)) + geom_line()

max(xgb.model$evaluation_log$test_auc)
xgb.model$evaluation_log[which(xgb.model$evaluation_log$test_auc==0.667508),]


# Calculate feature importance

imp <- xgb.importance(feature_names=trainm@Dimnames[[2]], 
                      model=xgb.model)
print(imp)
## Indicates to same set of parameters from WOE analysis or Logit.
xgb.plot.importance(imp)


# Score model
xg_probs <- predict(xgb.model, testm)

# Create a prediction object.

pred <- ROCR::prediction(predictions=xg_probs, 
                         labels=test_label)


# Draw ROC curve

perf <- ROCR::performance(pred, "tpr", "fpr")
auc <- ROCR::performance(pred, "auc")@y.values[[1]] %>% print()
pd <- data.frame(fpr=unlist(perf@x.values), tpr=unlist(perf@y.values))

pd %>%
  ggplot(aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc, 3))) +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Extreme Gradiant Boosting Model",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))

#AUC = 0.647
## Slight improvement(0.647 vs 0.61 ) in AUC over logit.
# Calculate the confusion matrix
test_pred_optimal<- as.factor(ifelse(xg_probs >= 0.55, 1,0))
optimal_conf <- confusionMatrix(test_pred_optimal, test_actual_default)

optimal_conf
#Accuracy : 0.6317   
#Sensitivity : 0.63244         
#Specificity : 0.61580

 
#########################################################################################  
########################Building  model for Decision tree################################
######################################################################################### 
library(rpart)
library(rpart.plot)
library(kernlab)
library(readr)

# Spliting the bank data in 70:30 ratio

 
set.seed(101)
master_df$Performance.Tag <- as.factor(ifelse(master_df$Performance.Tag==0,"no","yes"))

split_indices <- sample.split(master_df$Performance.Tag, SplitRatio = 0.70)
train_rf <- master_df[split_indices, ]
test_rf <- master_df[!split_indices, ]
nrow(train_rf)/nrow(master_df)
nrow(test_rf)/nrow(master_df)

train_rf<-train
test_rf<- test
train_rf$Performance.Tag<- as.factor(ifelse(train_rf$Performance.Tag==0,"no","yes"))
test_rf$Performance.Tag<- as.factor(ifelse(test_rf$Performance.Tag==0,"no","yes"))


table(train_rf$Performance.Tag)
# no   yes 
# 24451  24458 

#check classes distribution
prop.table(table(train_rf$Performance.Tag))
# no        yes 
# 0.4999284 0.5000716 




#Let's build a model on given data.Building a decision tree with default hyperparameters
tree.model <- rpart(Performance.Tag ~ .,                     # formula
                    data = train_rf,                   # training data
                    method = "class")               # classification or regression

# display decision tree
prp(tree.model)
# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "yes")

# Accuracy : 0.6658
# Sensitivity : 0.56957         
# Specificity : 0.67023



Change the algorithm to "information gain" instead of default "gini" ----------------------
tree.model <- rpart(Performance.Tag ~ .,                     # formula
                    data = train_rf,                   # training data
                    method = "class",               # classification or regression
                    parms = list(split = "information")
)

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "yes")

# Accuracy : 0.6658 
# Sensitivity : 0.56957         
# Specificity : 0.67023


#Tune the hyperparameters 
tree.model <- rpart(Performance.Tag ~ .,                                # formula
                    data = train_rf,                             # training data
                    method = "class",                         # classification or regression
                    control = rpart.control(minsplit = 1000,  # min observations for node
                                            minbucket = 1000, # min observations for leaf node
                                            cp = 0.05))       # complexity parameter

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test_rf, type = "class")

# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "yes")

# Accuracy : 0.8101
# Sensitivity : 0.33696         
# Specificity : 0.83187



# Cross test to choose CP 
# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))

# train model
tree.model <- train(Performance.Tag ~ .,
                    data = train_rf,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.control,
                    tuneGrid = tree.grid,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 20))

# look at cross validated model results
tree.model

# look at best value of hyperparameter
tree.model$bestTune

# make predictions on test set
tree.predict <- predict.train(tree.model, test_rf)

confusionMatrix(tree.predict, test_rf$Performance.Tag)  
#Accuracy : 0.7709
# Sensitivity : 0.78840        
# Specificity : 0.39022 


#In terms of probability 
tree_pred <- predict(tree.model, test_rf[, -1], type = "prob")


# Let's find out the optimal probalility cutoff 
perform_fn <- function(cutoff) 
{
  predicted_default <- as.factor(ifelse(tree_pred[,2] >= cutoff, "yes","no"))
  conf <- confusionMatrix(predicted_default, test_rf$Performance.Tag)
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(.01,.95,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
grid(50, lwd = 2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col= "blue",lwd=2)
box()
legend("topright", legend=c("Sensitivity","Specificity","Accuracy"),
       col=c(2,"darkgreen",4,"darkred"), cex=0.5,lwd=c(3,3,3,3), text.font=14,y.intersp = 0.3)


test_pred_optimal<- as.factor(ifelse(tree.pred[,2] >= 0.25, 1,0))
optimal_conf <- confusionMatrix(test_pred_optimal, test_actual_default)
optimal_conf

# Accuracy : 0.5051
# Sensitivity : 0.49351         
# Specificity : 0.75761


####################### KS -statistic - Decision Tree - Test Data######################## ###################

library(ROCR)
test_actual_default<-as.factor(ifelse(test_rf$Performance.Tag == "yes", 1,0))
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #0.251121


#KS-statistic is 25% 

#ROC Curve

auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 

# Area under curve is : 0.6255605

pd <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(pd ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Decision Tree Model",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))+
  annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc_ROCR, 3))) 

gini<-(auc_ROCR*2)-1
gini

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

lift_decile_info = lift(test_actual_default, test_pred, groups = 10)

print(lift_decile_info)
write.csv(lift_decile_info, "lift.csv", row.names = FALSE)

###Plotting Gain Chart
ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Gain),color='#FF6666', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Gain),color='#07843b',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")


###Plotting Lift Chart
ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#07843b', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#FF6666',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")




#########################################################################################
# Building a random Forest model with the synthetic data
# dataset from decision tree section would be reused here
#########################################################################################
library(randomForest)
library(ggplot2)
rf_synthetic <- randomForest(Performance.Tag ~., 
                             data = train_rf, 
                             proximity = F, 
                             do.trace = T, 
                             mtry = 5,
                             ntree=1000)
summary(rf_synthetic)

# make predictions on the test set
tree.predict <- predict(rf_synthetic, test_rf, type = "class")
# evaluate the results
confusionMatrix(tree.predict, test_rf$Performance.Tag, positive = "yes") 


#This is at a standard cut-off
#             Reference
# Prediction    no   yes
#         no  18152   737
#         yes  1886   183

#In terms of probbability
rf_pred_synthetic <- predict(rf_synthetic, test_rf, type = "prob")

#Let's find out the optimal cutoff value for probalility with synthetic data
#Cutoff for randomforest to assign yes or no
perform_fn_rf <- function(cutoff) 
{
  predicted_response <- as.factor(ifelse(rf_pred_synthetic[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$Performance.Tag, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}

# creating cutoff values from 0.01 to 0.99 for plotting and initialising a matrix of size 1000x4
s = seq(.01,.99,length=100)

OUT_rf = matrix(0,100,3)

# calculate the sens, spec and acc for different cutoff values

for(i in 1:100)
{
  OUT_rf[i,] = perform_fn_rf(s[i])
} 


# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(1,"darkgreen",2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))

cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.01)]
cutoff_rf
#The plot shows that cutoff value of around 17.8% optimises sensitivity and accuracy
## The cut off is too low.
test_pred_optimal<- factor(ifelse(rf_pred_synthetic[, 2] >= 0.22, "yes", "no"))
conf_rf <- confusionMatrix(test_pred_optimal, test_rf$Performance.Tag, positive = "yes")
conf_rf

# Accuracy : 0.6859
# Sensitivity : 0.53913       
# Specificity : 0.69263



####################### KS - statistic -Random Forest - Test Data######################## #######################
library(ROCR)
test_actual_default<-as.factor(ifelse(test_rf$Performance.Tag == "yes", 1,0))
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)  #0.2461614


#KS-statistic is 24% 

#ROC Curve

auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 

# Area under curve is : 0.6230807

pd <- data.frame(fpr=unlist(performance_measures_test@x.values), tpr=unlist(performance_measures_test@y.values))

ggplot(pd ,aes(x=fpr, y=tpr)) +
  geom_line(colour="red") +
  geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey") +
  labs(x="False Positive Rate",
       y="True Positive Rate",
       title="ROC Curve for Random Forest",
       caption="xgboost") +
  theme(axis.text.x=element_text(hjust=1))+
  annotate("text", x=0.4, y=0.00, hjust=0, vjust=0, size=5,
           label=paste("AUC =", round(auc_ROCR, 3))) 

gini<-(auc_ROCR*2)-1
gini

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

lift_decile_info = lift(test_actual_default, test_pred, groups = 10)

print(lift_decile_info)
write.csv(lift_decile_info, "lift.csv", row.names = FALSE)

###Plotting Gain Chart
ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Gain (%)")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Gain),color='#FF6666', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Gain),color='#07843b',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(20, 100, 10),labels=function(x) paste0(x,"%"))+
  ggtitle("Gain Chart")


###Plotting Lift Chart
ggplot(lift_decile_info, aes(x = bucket)) +
  labs(x = "Decile", y="Lift")+
  geom_point(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#07843b', group = 1,size=2,shape=21,stroke=2.5)+
  geom_line(data=lift_decile_info,aes(x=bucket,y=Cumlift),color='#FF6666',size=1, group = 1)+
  theme(panel.grid.minor = element_line(colour = "black", size = 0.5)) +
  scale_x_continuous(breaks = seq(1, 10, 1))+
  scale_y_continuous(breaks = seq(0.4, 4, 0.4))+
  ggtitle("Lift Chart")




#########################################################################################
########################### credit score generation process##############################
#########################################################################################
## Build an application scorecard with the good to bad odds of 10 to 1 
## at a score of 400 doubling every 20 points.  
str(final_df)
final_df$perdict_default  <- predict(final_lr_model, type = "response", newdata = final_df[,-1])
final_df$predict_NonDefault <- 1 - final_df$perdict_default
final_df$odds <-  log(final_df$predict_NonDefault/final_df$perdict_default)

Offset = 400
PDO = 20
log_odds=10
Factor = PDO/log(2)
Factor  #28.8539

final_df$Score = ceiling(Offset + (Factor*final_df$odds))

str(final_df$Score)
summary(final_df$Score)
## min - 337 to max - 424
quantile(final_df$Score,seq(0,1,0.2))  
 
## From the plot it is evident that score cut off could be set to 412.
cutoff_score =412

num_of_defaults_below_412<-length(which(final_df$Performance.Tag==1 & final_df$Score<412))
total_no_of_defaults<-length(which(final_df$Performance.Tag==1))

pc_defaults_covered_under_412<-ceiling((num_of_defaults_below_412/total_no_of_defaults)*100)

pc_defaults_covered_under_412


ggplot(final_df, aes(x = Score,color=Performance.Tag))+geom_bar()+geom_vline(aes(xintercept = cutoff_score))+labs(x="Score",y="Count",title="Score Distribution for all applicants")+annotate("text", x=350,y=4000, colour = "black",hjust=0, vjust=0, size=7,
                                                                                                                                                                                              label=paste("Defaults covered by 412 cut off : " ,pc_defaults_covered_under_412,"%"))

#########################################################################################
################# Predicting score for rejected applicants###############################
#########################################################################################
str(rejected_applicants)

rejects_for_scaling<-rejected_applicants[numeric_cols]

rejected_scaled_data<-data.frame(sapply(rejects_for_scaling, scale))
str(rejected_scaled_data)

rejects_for_dummies<-rejected_applicants[fact_cols]
 

# creating dummy variables for factor attributes
rejected_dummies<- data.frame(sapply(rejects_for_dummies,function(x) data.frame(model.matrix(~x-1,data =rejects_for_dummies))[,-1])) 

# combine all relevant columns to build final training data
rejected_final_df<- cbind(rejected_scaled_data,rejected_dummies)
str(rejected_final_df)

rejected_final_df$perdict_default  <- predict(final_lr_model, type = "response", newdata = rejected_final_df)
rejected_final_df$predict_NonDefault <- 1 - rejected_final_df$perdict_default
rejected_final_df$odds <-  log(rejected_final_df$predict_NonDefault/rejected_final_df$perdict_default)

rejected_final_df$Score = ceiling(Offset + (Factor*rejected_final_df$odds))

summary(rejected_final_df$Score)

length(which(rejected_final_df$Score<412))/nrow(rejected_final_df) #0.74
## With our decided cutoff 412 we were able to identify 74.24% actual rejected applicants.
cutoff_score= 412
correct_rejections_by_scorecard="74.24%"
length(which(rejected_final_df$Score<412))/nrow(rejected_final_df) #0.74
## With our decided cutoff 412 we were able to identify 74.24% actual rejected applicants.


ggplot(rejected_final_df, aes(x = Score)) +geom_bar()+geom_vline(aes(xintercept = cutoff_score,col="red"))+labs(x="Score",y="Count",title="Score Distribution of Actual Rejected applications")+annotate("text", x=380,y=1, colour = "white",hjust=0, vjust=0, size=7,
           label=paste("Corect rejections by score card% =", correct_rejections_by_scorecard)) 

####---------------------------------------------------------------------------------
##Approach_2 - Using scorecard package s
## >>convert whole data to woe data 
##>> model >> use scorecard package >> get scores for each row
####---------------------------------------------------------------------------------
library(woeBinning)
library(scorecard)
# woe binning ------ 
bins = woebin(final_df, "Performance.Tag")
dt_woe = woebin_ply(final_df, bins)

#modelling on woe dataframe
m = glm(Performance.Tag ~ ., family = binomial(), data = dt_woe)

m_2 <- stepAIC(m, direction = "both")

m_3 <- glm(Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.company_woe + 
             Avgas.CC.Utilization.in.last.12.months_woe + No.of.times.90.DPD.or.worse.in.last.6.months_woe + 
             No.of.times.30.DPD.or.worse.in.last.6.months_woe + No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
             No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
             No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
             Profession.xSE_woe
           , family = "binomial", data = dt_woe)

summary(m_3)

m_4 <- glm(Performance.Tag ~ Age_woe + Income_woe + No.of.months.in.current.company_woe + 
             Avgas.CC.Utilization.in.last.12.months_woe  + 
             No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
             No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.trades.opened.in.last.12.months_woe + 
             No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe 
           , family = "binomial", data = dt_woe)

summary(m_4)

vif(m_4)


# score ------
card = scorecard(bins, m_4,points0 = 400,odds0 = 1/9,pdo = 20)

# credit score for only total score
final_df$score = scorecard_ply(final_df, card)

summary(final_df$score)
## min = 393 , max = 466

final_df_ordered_by_score<-final_df[order(final_df$score,decreasing = TRUE)]

#test_score = scorecard_ply(test, card, print_step=0)

final_df_ordered_by_score$color_code<-cut(final_df_ordered_by_score$score,breaks=c(-Inf,400,430,460,Inf), labels=c("red","orange","lightGreen","green"))

out_df_totals<-data.frame(table(final_df_ordered_by_score$color_code))

final_df_ordered_by_score$color_code <- as.character(final_df_ordered_by_score$color_code)
final_df_ordered_by_score$color_code  <- factor(final_df_ordered_by_score$color_code , levels=unique(final_df_ordered_by_score$color_code ))

ggplot(final_df_ordered_by_score, aes(color_code, ..count..)) + geom_bar(aes(fill = Performance.Tag), position = "dodge")


#################### financial analysis###########################

approval_rate <-(nrow(merged_df) -nrow(rejected_applicants))/nrow(merged_df) *100

approval_rate
# current approval rate : 98%

default_users_outstanding <- data_for_eda$Outstanding.Balance[which(data_for_eda$Performance.Tag==1)]

current_credit_loss<- sum(default_users_outstanding)

current_credit_loss
#Current credit loss : 3711178158

default_users_ID <- master_data_backup$Application.ID [which(master_data_backup$Performance.Tag==1)]

t1<-final_df$Outstanding.Balance[which(final_df$Performance.Tag==1)]
t2<-final_df$Score[which(final_df$Performance.Tag==1)]    

outstanding_ref <- cbind(default_users_ID,default_users_outstanding,scale(default_users_outstanding),t1,t2)

possible_defaults_with_more_than_412_score<-data.frame(subset(outstanding_ref,t2>412))

sum(possible_defaults_with_more_than_412_score$default_users_outstanding)
# New credit loss : 540123231


nrow(data.frame(subset(final_df,Score>412)))/nrow(final_df)
# New approval rate : 43%

## Although net credit loss  is much lesser, Approval rate also goes down sharply. 
## Which in turn could cause less sales. Hence in this Trade-off between sales and risk,
## we could afford to be less conservative in terms of score cut off.
## from score distribution plot we can observe that by setting cut off to 395
## we can cover  much higher sales without risking too much credit loss.

possible_defaults_with_more_than_395_score<-data.frame(subset(outstanding_ref,t2>395))

sum(possible_defaults_with_more_than_395_score$default_users_outstanding)
# New credit loss : 1,75,54,65,850


nrow(data.frame(subset(final_df,Score>395)))/nrow(final_df)
# New approval rate : 71%

## This gives a more balanced result.
