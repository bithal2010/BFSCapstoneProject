## objective — > predict what is probability of default if credit card is approved? 

#Read given CSV files into individual dataframes and then merging them 
demographic_df<- read.csv(file = 'Demographic data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')
  
credit_buraeu_df<- read.csv(file = 'Credit Bureau data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')

# Merging by common attributes and by unique rows  
merged_df<- merge(x = unique(demographic_df)
                  , y = unique(credit_buraeu_df)
                  , by = c("Application.ID", "Performance.Tag"))

# Dropping ID column as it is of no use.
merged_df<-merged_df[,-1]

#Duplicate rows in data - none found.
sum(duplicated(merged_df))

#Finding rows where dependant variable-"Performance.Tag" is not populated. 
null_rows<-length(which( is.na(merged_df$Performance.Tag) == TRUE))

null_rows/nrow(merged_df) 

# Only 1.9% of the rows have NA values for dependant variable - 'perfromance.tag'
# Assumption 1 - So model should be built on data where credit card was approved(0/1). 
# dependant variable - 'perfromance.tag' = NA for applicants for which credit was not issued in first place.
# So removing these rows.

data_for_eda <- merged_df[!is.na(merged_df$Performance.Tag) == TRUE,]


# Getting a summary of master data
summary(data_for_eda)
str(data_for_eda)


############# MISSING VALUE DETECTION AND TREATMENT############################
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

# Some outlier samples 
company_recency_outliers$No.of.months.in.current.company
last_6mon_pl_outliers$No.of.PL.trades.opened.in.last.6.months
total_trd_outliers$Total.No.of.Trades

## Read on Cook's distance
## Go on case by case basis and check if u want to treat it .


## review the WOE code  by someone
## are they significant according to corr analysis. they both should be giving same results.
## scatter plot for all columsn.

## corr analysis will indicate the same way for multi variate analysis.
## https://www.r-bloggers.com/r-tutorial-series-scatterplots/
#http://r4ds.had.co.nz/exploratory-data-analysis.html
#https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619

#does they have diff credit card?
#home -car loans impact on default??
#income variations to default??
#Find out and Write down the insights like above.
#sub-population_statistics
 
 

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

#################Deriving range/bin variables , then factoring them##################

## Let's perform Segmentation/binning of some continuous variables such as - 
## age, income, avg-cc-utilization,  residency-duration , job-duration etc.

#creating new factor column age_group from age column

data_for_eda$age_group<-findInterval(data_for_eda$Age,c(20,30,40,50,60,70))

data_for_eda$age_group<-as.factor(data_for_eda$age_group)

data_for_eda$income_group <- findInterval(data_for_eda$Income, c(10,20,30,40,50,60))

data_for_eda$income_group <-as.factor(data_for_eda$income_group)

data_for_eda$avg_cc_utilization <- 
  findInterval(data_for_eda$Avgas.CC.Utilization.in.last.12.months, c(15,30,45,60,75))

data_for_eda$avg_cc_utilization <-as.factor(data_for_eda$avg_cc_utilization)

#creating new factor column job_recency from 'No.of.months.in.current.company' column
summary(data_for_eda$No.of.months.in.current.company)

data_for_eda$job_recency <- 
  findInterval(data_for_eda$No.of.months.in.current.company,  c(12,24,36,48,60))

data_for_eda$job_recency <-as.factor(data_for_eda$job_recency)

data_for_eda$house_recency <- 
  findInterval(data_for_eda$No.of.months.in.current.residence, c(12,24,36,48,60))

data_for_eda$house_recency <-as.factor(data_for_eda$house_recency)
data_for_eda$No.of.dependents<-as.factor(data_for_eda$No.of.dependents)
data_for_eda$Presence.of.open.auto.loan<-as.factor(data_for_eda$Presence.of.open.auto.loan)
data_for_eda$Presence.of.open.home.loan<-as.factor(data_for_eda$Presence.of.open.home.loan)

## Checking Outstanding.Balance ,Total.No.of.Trades
quantile(data_for_eda$Outstanding.Balance,probs = seq(0,1,0.20))
data_for_eda$balance_amount <- 
  findInterval(data_for_eda$Outstanding.Balance, c(26113.2,586671.0,774234.5,2960629.6))
data_for_eda$balance_amount <-as.factor(data_for_eda$balance_amount )

data_for_eda$trading_range <- 
  findInterval(data_for_eda$Total.No.of.Trades, c(1,2,3,4,5))
data_for_eda$trading_range<-as.factor(data_for_eda$trading_range)
 
str(data_for_eda)
# removing the above numeric columns
#data_for_eda<-  data_for_eda[,-which(names(data_for_eda) %in% c('Age','Income' , 'Avgas.CC.Utilization.in.last.12.months'
#                                                                        ,'No.of.months.in.current.residence'
#                                                                        , 'No.of.months.in.current.company'
#                                                                        ,'Outstanding.Balance'
#                                                                        ,'Total.No.of.Trades'))]
#
#
str(data_for_eda) 
##################### Univariate analysis######################################
library(plyr)
library(ggplot2)
table(data_for_eda$Performance.Tag)
#0     1 
#66922  2948
# Data is highly imbalanced. Can be seen from below simple barplot.
barplot(prop.table(table(data_for_eda$Performance.Tag)*100)
        ,col=c("lightcyan")
        ,xlab='Performance.Tag'
        ,ylab='% of Population'
        )

ggplot(data_for_eda, aes(age_group)) + geom_bar()
## Users are mostly in 35-55 age range

ggplot(data_for_eda, aes(income_group)) + geom_bar()
## Users are uniformly distributed in 0-40 range.
## However population size of high income category 45-60 keep on reducing.
## Box plots dont indicate towards any outliers.


ggplot(data_for_eda, aes(house_recency)) + geom_bar()
## Most users either dont have a house or have recently moved into a new house. 
## population size keep on reducing with increasing period of stay in current house.

 
hist(data_for_eda$No.of.months.in.current.company, xlab = "No.of.months.in.current.company")
boxplot(data_for_eda$No.of.months.in.current.company)
ggplot(data_for_eda, aes(job_recency)) + geom_bar()
## Most users are new job holders with 0-5yr experience. 
## population size is low in high experience category.
## Some outliers do exist.

summary(data_for_eda$No.of.times.90.DPD.or.worse.in.last.6.months)
hist(data_for_eda$No.of.times.90.DPD.or.worse.in.last.6.months, xlab = "No.of.times.90.DPD.or.worse.in.last.6.months")
boxplot(data_for_eda$No.of.times.90.DPD.or.worse.in.last.6.months)
# Most people have no such overdues 
# Among the very less people who have  90 days overdue, repeating offenders population size is very very small.


summary(data_for_eda$No.of.times.60.DPD.or.worse.in.last.6.months)
hist(data_for_eda$No.of.times.60.DPD.or.worse.in.last.6.months, xlab = "No.of.times.60.DPD.or.worse.in.last.6.months")
boxplot(data_for_eda$No.of.times.60.DPD.or.worse.in.last.6.months)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.
# compared to 90 days overdues, population size is higher

summary(data_for_eda$No.of.times.30.DPD.or.worse.in.last.6.months)
hist(data_for_eda$No.of.times.30.DPD.or.worse.in.last.6.months, xlab = "No.of.times.30.DPD.or.worse.in.last.6.months")
boxplot(data_for_eda$No.of.times.30.DPD.or.worse.in.last.6.months)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.

summary(data_for_eda$No.of.times.90.DPD.or.worse.in.last.12.months)
hist(data_for_eda$No.of.times.90.DPD.or.worse.in.last.12.months, xlab = "No.of.times.90.DPD.or.worse.in.last.12.months")
boxplot(data_for_eda$No.of.times.90.DPD.or.worse.in.last.12.months)
# Most people have no such overdues 
# Among the very less people who have  90 days overdue, repeating offenders population size is very very small.


summary(data_for_eda$No.of.times.60.DPD.or.worse.in.last.12.months)
hist(data_for_eda$No.of.times.60.DPD.or.worse.in.last.12.months, xlab = "No.of.times.60.DPD.or.worse.in.last.12.months")
boxplot(data_for_eda$No.of.times.60.DPD.or.worse.in.last.12.months)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.
# compared to 90 days overdues, population size is higher

summary(data_for_eda$No.of.times.30.DPD.or.worse.in.last.12.months)
hist(data_for_eda$No.of.times.30.DPD.or.worse.in.last.12.months, xlab = "No.of.times.30.DPD.or.worse.in.last.12.months")
boxplot(data_for_eda$No.of.times.30.DPD.or.worse.in.last.12.months)
# Most people have no such overdues  
# repeating offenders population size keep on decreasing with occurances of overdue.

summary(data_for_eda$Avgas.CC.Utilization.in.last.12.months)
hist(data_for_eda$Avgas.CC.Utilization.in.last.12.months, xlab = "avg cc utilization")
boxplot(data_for_eda$Avgas.CC.Utilization.in.last.12.months,horizontal = T)
## most users are utilizing only upto 20% of card upper limit, 
## population size with proper 25 to 60 % card utilization is similar
## Left skewed ..outliers do exist.

summary(data_for_eda$No.of.trades.opened.in.last.6.months)
hist(data_for_eda$No.of.trades.opened.in.last.6.months, xlab = "No.of.trades.opened.in.last.6.months")
boxplot(data_for_eda$No.of.trades.opened.in.last.6.months)
# most users have 0-4 trades opened in last 6 mon.
# Outlier do exist.

summary(data_for_eda$No.of.trades.opened.in.last.12.months)
hist(data_for_eda$No.of.trades.opened.in.last.12.months, xlab = "No.of.trades.opened.in.last.12.months")
boxplot(data_for_eda$No.of.trades.opened.in.last.12.months)
# most users have 0-10 trades opened in last 12 mon.
# Outlier do exist.

summary(data_for_eda$No.of.PL.trades.opened.in.last.6.months)
hist(data_for_eda$No.of.PL.trades.opened.in.last.6.months, xlab = "No.of.PL.trades.opened.in.last.6.months")
boxplot(data_for_eda$No.of.PL.trades.opened.in.last.6.months)
# most users have 0-3 PL opened in last 12 mon.
# Very few Outlier are there.

summary(data_for_eda$No.of.PL.trades.opened.in.last.12.months)
hist(data_for_eda$No.of.PL.trades.opened.in.last.12.months, xlab = "No.of.PL.trades.opened.in.last.12.months")
boxplot(data_for_eda$No.of.PL.trades.opened.in.last.12.months)
# most users have 0-6 trades opened in last 12 mon.
# Outlier might be there.



summary(data_for_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
hist(data_for_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
boxplot(data_for_eda$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.,horizontal = T)
# most users have 0-4 trades opened in last 6 mon.
# Outlier might be there.


summary(data_for_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
hist(data_for_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., xlab = "Autoloans-6mon")
boxplot(data_for_eda$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)
# most users have 0-5 trades opened in last 12 mon.
# Outlier are present.


str(data_for_eda)

summary(data_for_eda$Total.No.of.Trades)
hist(data_for_eda$Total.No.of.Trades, xlab = "Total.No.of.Trades")
boxplot(data_for_eda$Total.No.of.Trades)
# most users have 0-10 trades in total
# Outlier are there.

summary(data_for_eda$Outstanding.Balance)
hist(data_for_eda$Outstanding.Balance, xlab = "Outstanding.Balance")
boxplot(data_for_eda$Outstanding.Balance,names = "Outstanding.Balance")
# 0-200000 range higher no of users
# 300k upwards lower no of users
# most users are starting to repay their loans



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


library(gridExtra)
library(grid)
library(Information)

IV <- create_infotables(data=data_for_eda, y="Performance.Tag", bins=10, parallel=T)

head(IV)

IV_Value = data.frame(IV$Summary)
grid.table(IV$Summary[seq(from=1,to=20,by=1),], rows=NULL)
plot(data.frame(seq(1,5,by=1),seq(6,10,by=1)))

plotFrame <- IV$Summary[order(-IV$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable,
                             levels = plotFrame$Variable[order(-plotFrame$IV)])
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "darkblue", fill = "white") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))



##################Correlation analysis#############################

#install.packages('corrplot')
library(corrplot)
cor_df<-
  data_for_eda[,c('Age','Income','No.of.months.in.current.residence','No.of.months.in.current.company'
                  ,'Total.No.of.Trades','Outstanding.Balance','Avgas.CC.Utilization.in.last.12.months'
                  ,'No.of.times.90.DPD.or.worse.in.last.6.months','No.of.times.60.DPD.or.worse.in.last.6.months','No.of.times.30.DPD.or.worse.in.last.6.months'
                  ,'No.of.times.90.DPD.or.worse.in.last.12.months','No.of.times.60.DPD.or.worse.in.last.12.months','No.of.times.30.DPD.or.worse.in.last.12.months'
                  ,'No.of.trades.opened.in.last.6.months','No.of.trades.opened.in.last.12.months'
                  ,'No.of.PL.trades.opened.in.last.6.months','No.of.PL.trades.opened.in.last.6.months'
                  ,'No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.'
                  ,'No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.'
                  ,'No.of.PL.trades.opened.in.last.12.months')]

corr_index<- cor(cor_df) 

corrplot(corr_index, type = "upper", tl.pos = "td",
         method = "circle", tl.cex = 0.01, tl.col = 'black',
         order = "hclust", diag = FALSE)

colnames(cor_df)
 
#################correlation visible from the plot#####################

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

# Lets look into default users exclusively to see behaviours 
default_data_for_eda<-data_for_eda[which(data_for_eda$Performance.Tag==1),]

#default_data_for_eda<-  default_data_for_eda[,-which(names(default_data_for_eda) %in% c('Age','Income' 
#                                                                                        , 'Avgas.CC.Utilization.in.last.12.months' ,'No.of.months.in.current.residence'
#                                                                                        , 'No.of.months.in.current.company','Outstanding.Balance' 
#                                                                                        ,'Total.No.of.Trades','Performance.Tag'))]

str(default_data_for_eda)                                                              

b <- ggplot(default_data_for_eda, aes(x = avg_cc_utilization, y = No.of.PL.trades.opened.in.last.12.months))
# Scatter plot with regression line
b + geom_point()+  geom_smooth(method = "lm") 



##plot(cont. var ~ factor var, data = df)<< side by side box plot
##plot( nume~numer ,data = df)  >>scatter plot
##aggregate(num~ fact1+fact2 , data = df , FUN = mean)
##pairs(df[,]) >> df with only numeric variables

#master<-write.csv(x=data_for_eda,file='master.csv')
 

plot(data_for_eda$Outstanding.Balance~data_for_eda$Performance.Tag)#clear poistive correlation

plot(data_for_eda$Total.No.of.Trades~data_for_eda$Outstanding.Balance)#clear poistive correlation

plot(data_for_eda$Outstanding.Balance~data_for_eda$Avgas.CC.Utilization.in.last.12.months)# Non-conclusive correltion

plot(data_for_eda$Avgas.CC.Utilization.in.last.12.months~data_for_eda$No.of.times.90.DPD.or.worse.in.last.6.months)

d<-aggregate(data_for_eda$Avgas.CC.Utilization.in.last.12.months~data_for_eda$income_group+data_for_eda$Presence.of.open.home.loan , data= data_for_eda ,FUN=mean)


plot(data_for_eda$Avgas.CC.Utilization.in.last.12.months~data_for_eda$income_group+data_for_eda$Presence.of.open.home.loan)


###########  SMOTE(synthetic minority oversampling technique) by ROSE package
#SMOTE algorithm creates artificial data based on feature space (rather than data space) 
# similarities from minority samples. It generates a random set of minority class observations 
# to shift the classifier learning bias towards minority class.

# install.packages("ROSE")
library(ROSE)

table(data_for_eda$Performance.Tag)
prop.table(table(data_for_eda$Performance.Tag))
## Only 4% of observations are under default category. So it is ahighly imbalanced data which would result 
## in in-effictive models if not treated properly.

# Lets do perfrom both oversampling and undersampling i.e.
# the minority class is oversampled with replacement and majority class is undersampled without replacement.

balanced_data_both <- ovun.sample(Performance.Tag ~ ., data = data_for_eda, method = "both", p=0.5, 
                                  N= 0.7*nrow(data_for_eda) , seed = 1)$data

table(balanced_data_both$Performance.Tag)

#Generate data synthetically to avoid errors related to explicitely mentioned probability

balanced_data_synthetic <- ROSE(Performance.Tag ~ ., data = data_for_eda, seed = 1)$data

table(balanced_data_synthetic$Performance.Tag)


 
###############Picking derived factor columns against original numeric fields ###################

## WILL USE THE balanced_data_synthetic dataframe for Logistic regression modelling.
# Using the factorised derived attributes rather than original continuous variables
# age_group vs Age
# income_group vs Income
# avg_cc_utilization vs Avgas.CC.Utilization.in.last.12.months
# house_recency vs No.of.months.in.current.residence
# job_recency vs No.of.months.in.current.company
# balance_amount vs Outstanding.Balance 
# trading_range vs Total.No.of.Trades 

balanced_data_synthetic<- 
  balanced_data_synthetic[,-which(names(balanced_data_synthetic) %in% c('Age','Income'
                              , 'Avgas.CC.Utilization.in.last.12.months'
                              ,'No.of.months.in.current.residence'
                              , 'No.of.months.in.current.company'
                              ,'Outstanding.Balance'
                              ,'Total.No.of.Trades'))]

str(balanced_data_synthetic)

balanced_data_synthetic$Performance.Tag<-as.factor(balanced_data_synthetic$Performance.Tag)
 

################Scaling numeric columns & creating dummies for factor attributes#############

num_cols<-c(
  "No.of.times.90.DPD.or.worse.in.last.6.months"
  ,"No.of.times.60.DPD.or.worse.in.last.6.months" 
  , "No.of.times.30.DPD.or.worse.in.last.6.months" 
  ,"No.of.times.90.DPD.or.worse.in.last.12.months"
  ,"No.of.times.60.DPD.or.worse.in.last.12.months" 
  ,"No.of.times.30.DPD.or.worse.in.last.12.months" 
  ,"No.of.trades.opened.in.last.6.months"
  ,"No.of.trades.opened.in.last.12.months"
  ,"No.of.PL.trades.opened.in.last.6.months"
  ,"No.of.PL.trades.opened.in.last.12.months" 
  ,"No.of.Inquiries.in.last.6.months..excluding.home...auto.loans."
  , "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")

sapply(balanced_data_synthetic[num_cols], scale)

str(balanced_data_synthetic)


fact_cols <- c("Gender","Marital.Status..at.the.time.of.application.",
          "No.of.dependents","Education","Profession","Type.of.residence",
          "Presence.of.open.home.loan","Presence.of.open.auto.loan",
          "age_group","income_group","avg_cc_utilization","job_recency"
          ,"house_recency","balance_amount","trading_range")

event_col<-c("Performance.Tag") 

balanced_data_synthetic$Performance.Tag

balanced_data_synthetic_fact <- balanced_data_synthetic[fact_cols] 

str(balanced_data_synthetic_fact) 

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(balanced_data_synthetic_fact, 
                            function(x) data.frame(model.matrix(~x-1,data =balanced_data_synthetic_fact))[,-1])) 



balanced_data_synthetic_final<- cbind(balanced_data_synthetic[event_col],balanced_data_synthetic[num_cols],dummies)

str(balanced_data_synthetic_final)

#################### splitting into train and test data for modelling######################

library(caTools)


?sample.split

split_indices <- sample.split(balanced_data_synthetic_final, SplitRatio = 7/10)

train <- balanced_data_synthetic_final[split_indices, ]

test <- balanced_data_synthetic_final[!split_indices, ]
 

###################Lets create a Logistic Model: Logistic Regression#####################

library(MASS)
#install.packages('car')
library(car)

 
library(e1071)
library(caret)
library(caTools)


logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

logistic_2 <- stepAIC(logistic_1, direction = "both")


logistic_3<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 + Education.xMasters + Education.xOthers + 
                   Education.xProfessional  + Profession.xSAL+ Profession.xSE + 
                   Profession.xSE_PROF + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xLiving.with.Parents + Type.of.residence.xOwned + 
                   Type.of.residence.xRented + Presence.of.open.home.loan + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5 + job_recency.x1 + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3 + balance_amount.x4 + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3 + trading_range.x4 + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_3)

sort(vif(logistic_3),decreasing = T)

# removing  due to high vif value  - Profession.xSAL 

logistic_4<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 + Education.xMasters + Education.xOthers + 
                   Education.xProfessional + Profession.xSE + 
                   Profession.xSE_PROF + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xLiving.with.Parents + Type.of.residence.xOwned + 
                   Type.of.residence.xRented + Presence.of.open.home.loan + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5 + job_recency.x1 + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3 + balance_amount.x4 + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3 + trading_range.x4 + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_4)

sort(vif(logistic_4),decreasing = T)

# removing  due to high vif value  - Type.of.residence.xRented 

logistic_5<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 + Education.xMasters + Education.xOthers + 
                   Education.xProfessional + Profession.xSE + 
                   Profession.xSE_PROF + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xLiving.with.Parents + Type.of.residence.xOwned  + Presence.of.open.home.loan + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5 + job_recency.x1 + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3 + balance_amount.x4 + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3 + trading_range.x4 + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_5)

sort(vif(logistic_5),decreasing = T)

# Although vif value is high for   - trading_range.x5 
## due to high P-value we cant remove it.Removing this attribute causes upward swing for AIC value.
# removing next high vif attr-balance_amount.x4
logistic_6<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 + Education.xMasters + Education.xOthers + 
                   Education.xProfessional + Profession.xSE + 
                   Profession.xSE_PROF + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xLiving.with.Parents + Type.of.residence.xOwned  + Presence.of.open.home.loan + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5 + job_recency.x1 + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3  + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3 + trading_range.x4 + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_6)

sort(vif(logistic_6),decreasing = T)

# removing next high vif attr-balance_amount.x4
logistic_7<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 + Education.xMasters + Education.xOthers + 
                   Education.xProfessional + Profession.xSE + 
                   Profession.xSE_PROF + Type.of.residence.xCompany.provided + 
                   Type.of.residence.xLiving.with.Parents + Type.of.residence.xOwned  + Presence.of.open.home.loan + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5 + job_recency.x1 + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3  + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3  + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_7)

sort(vif(logistic_7),decreasing = T)

# VIF VALUES ARE LOOKING IS ACCEPTABLE RANGE, LETS REMOVE LOW SIGNIFICANCE

# REMOVING RESIDENCE TYPE ATTRIBUTE due to low significance -
#Type.of.residence.xCompany.provided  , Type.of.residence.xLiving.with.Parents ,Type.of.residence.xOwned  


logistic_8<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 + Education.xMasters + Education.xOthers + 
                   Education.xProfessional + Profession.xSE + 
                   Profession.xSE_PROF + Presence.of.open.home.loan + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5 + job_recency.x1 + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3  + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3  + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_8)

# removing education , proffession params 

logistic_9<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 +  Presence.of.open.home.loan + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5 + job_recency.x1 + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3  + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3  + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_9)


# Removing Presence.of.open.home.loan

logistic_10<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                   No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                   No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                   No.of.dependents.x2 + 
                   age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                   avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                   avg_cc_utilization.x5  + job_recency.x2 + 
                   job_recency.x3 + job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                   house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                   balance_amount.x3  + trading_range.x1 + 
                   trading_range.x2 + trading_range.x3  + 
                   trading_range.x5
                 , family = "binomial", data = train)

summary(logistic_10)



# Removing job_recency.x3

logistic_11<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. + 
                    No.of.dependents.x2 + 
                    age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                    avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                    avg_cc_utilization.x5  + job_recency.x2 + 
                    job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                    house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                    balance_amount.x3  + trading_range.x1 + 
                    trading_range.x2 + trading_range.x3  + 
                    trading_range.x5
                  , family = "binomial", data = train)

summary(logistic_11)

#removing No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.

logistic_12<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                    No.of.dependents.x2 + 
                    age_group.x3 + income_group.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                    avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                    avg_cc_utilization.x5  + job_recency.x2 + 
                    job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                    house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                    balance_amount.x3  + trading_range.x1 + 
                    trading_range.x2 + trading_range.x3  + 
                    trading_range.x5
                  , family = "binomial", data = train)

summary(logistic_12)

  #removing income_group.x2
  
  
  logistic_13<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                      No.of.PL.trades.opened.in.last.6.months + No.of.PL.trades.opened.in.last.12.months + 
                      No.of.dependents.x2 + 
                      age_group.x3 + income_group.x5 + avg_cc_utilization.x1 + 
                      avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                      avg_cc_utilization.x5  + job_recency.x2 + 
                      job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                      house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                      balance_amount.x3  + trading_range.x1 + 
                      trading_range.x2 + trading_range.x3  + 
                      trading_range.x5
                    , family = "binomial", data = train)

summary(logistic_13)  

  #removing  No.of.PL.trades.opened.in.last.6.months
  logistic_14<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                      No.of.PL.trades.opened.in.last.12.months + 
                      No.of.dependents.x2 + 
                      age_group.x3 + income_group.x5 + avg_cc_utilization.x1 + 
                      avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                      avg_cc_utilization.x5  + job_recency.x2 + 
                      job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                      house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                      balance_amount.x3  + trading_range.x1 + 
                      trading_range.x2 + trading_range.x3  + 
                      trading_range.x5
                    , family = "binomial", data = train)

summary(logistic_14)  

 
  #removing  No.of.PL.trades.opened.in.last.6.months
  logistic_15<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                      No.of.PL.trades.opened.in.last.12.months + 
                      No.of.dependents.x2 + 
                      age_group.x3 + income_group.x5 + avg_cc_utilization.x1 + 
                      avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                      avg_cc_utilization.x5  + 
                      job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                      house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                      balance_amount.x3  + trading_range.x1 + 
                      trading_range.x2 + trading_range.x3  + 
                      trading_range.x5
                    , family = "binomial", data = train)

summary(logistic_15)   
  


age_group.x3                                  0.079883   0.020607   3.876  0.000117
trading_range.x3                              -0.167677   0.044080  -3.804 0.000104 ***
  
  
  #removing  age_group.x3
  logistic_16<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                      No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                      No.of.PL.trades.opened.in.last.12.months + 
                      No.of.dependents.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                      avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                      avg_cc_utilization.x5  + 
                      job_recency.x4 + job_recency.x5 + house_recency.x3 + 
                      house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                      balance_amount.x3  + trading_range.x1 + 
                      trading_range.x2 + trading_range.x3  + 
                      trading_range.x5
                    , family = "binomial", data = train)

summary(logistic_16) 


#removing job_recency.x5
logistic_17<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.12.months + 
                    No.of.dependents.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                    avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                    avg_cc_utilization.x5  + 
                    job_recency.x4  + house_recency.x3 + 
                    house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                    balance_amount.x3  + trading_range.x1 + 
                    trading_range.x2 + trading_range.x3  + 
                    trading_range.x5
                  , family = "binomial", data = train)

summary(logistic_17) 


job_recency.x4                                -0.091114   0.026991  -3.376 0.004222 *** 
  trading_range.x3  0.000149

#removing job_recency.x4
logistic_18<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.12.months + 
                    No.of.dependents.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                    avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                    avg_cc_utilization.x5  +  house_recency.x3 + 
                    house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                    balance_amount.x3  + trading_range.x1 + 
                    trading_range.x2 + trading_range.x3  + 
                    trading_range.x5
                  , family = "binomial", data = train)

summary(logistic_18) 

#removing trading_range.x3

logistic_19<- glm(Performance.Tag ~ No.of.times.30.DPD.or.worse.in.last.6.months + 
                    No.of.times.90.DPD.or.worse.in.last.12.months + No.of.times.30.DPD.or.worse.in.last.12.months + 
                    No.of.PL.trades.opened.in.last.12.months + 
                    No.of.dependents.x2 + income_group.x5 + avg_cc_utilization.x1 + 
                    avg_cc_utilization.x2 + avg_cc_utilization.x3 + avg_cc_utilization.x4 + 
                    avg_cc_utilization.x5  +  house_recency.x3 + 
                    house_recency.x5 + balance_amount.x1 + balance_amount.x2 + 
                    balance_amount.x3  + trading_range.x1 + 
                    trading_range.x2   +  trading_range.x5
                  , family = "binomial", data = train)

summary(logistic_19) 

##  significance is very high now for existing attributes.Lets take this model as final LR model for now.
final_lr_model <- logistic_19

 
###  #### #### Model Evaluation with Test Data #### #### ####
 

#predicted probabilities  for test data
test_pred = predict(final_lr_model, type = "response", newdata = test[,-1])

# Let's see the summary 
summary(test_pred)
test$prob <- test_pred
View(test)

# Let's use the probability cutoff of 50%.
test_pred_default <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

View(test_actual_default)
View(test_pred_default)

table(test_actual_default,test_pred_default)

conf_mtr_50_cutoff <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
 
conf_mtr_50_cutoff
 
## with 50% cut off 
## accuracy = 63.3%
## specificity =  58.2%
## sensitivity = 68.49%
## So final model looks like a fairly acceptable model. 
##  We will manipulate the cut off value to ensure better performance.


#########################################################################################
# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Summary of test probability

summary(test_pred)
# Creating cutoff values from 0.0006769 to 0.9284441 for plotting and initiallizing a matrix of 100 X 3.



s = seq(.01,.95,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
grid(100,10, lwd = 2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

 
#from grid it looks like cutoff = 0.52


test_pred_default <- factor(ifelse(test_pred >= 0.52, "Yes", "No"))
test_conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
test_conf

# for 52% threshold,  accuracy = 63.26% , specificity = 62.02% , sensitivity = 64.51%
# So cutoff value is 0.52 for final model


##However in this business sceanario out of the 2 types of misclassifications -
## Someone who is defaulting , and model is saying it ll not default  
## Someone not defaulting, model says he/she is defaulting   
 
## first miscllassification lead to financial loss
## second misclassification may lead to follwo up with some customers 
## and may be some customer dissatisfaction.
## So higher the sensitivity , the better.
## from sensitivity-specificity plot grid we can infer that  
## for high sensitivity -  0.25>=cut-off

high_sensitivity_cutoff <- 0.2 
high_sens_test_pred_default <- factor(ifelse(test_pred >= high_sensitivity_cutoff, "Yes", "No"))
test_conf <- confusionMatrix(high_sens_test_pred_default, test_actual_default, positive = "Yes")
test_conf


####################### KS -statistic - Test Data ###################

test_pred_default_1 <- ifelse(test_pred_default=="Yes",1,0)
test_actual_default_1 <- ifelse(test_actual_default=="Yes",1,0)

##install.packages('ROCR')
library(ROCR)
#on testing  data
pred_object_test<- prediction(test_pred_default_1, test_actual_default_1)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)   ## 0.26

#KS-statistic is 26% 

#ROC Curve

plot(performance_measures_test, colorize = TRUE, text.adj = c(-0.2,1.7))
