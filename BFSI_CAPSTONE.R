library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(Information)
library(corrplot)
library(caTools)
library(ROSE)
library(MASS) 
library(car)
library(e1071)
library(caret) 
library(ROCR)



## objective — > predict what is probability of default if credit card is approved? 

#Read given CSV files into individual dataframes and then merging them 
demographic_df<- read.csv(file = 'Demographic data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')
  
credit_df<- read.csv(file = 'Credit Bureau data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')

## Observing duplication in unique ID column before joining.
sum(duplicated(demographic_df$Application.ID))
sum(duplicated(credit_df$Application.ID))
##so we have 3 rows in which application id is duplicated.

demographic_df[duplicated(demographic_df$Application.ID),]

credit_df[duplicated(credit_df$Application.ID),]

### 765011468,653287861,671989187 are duplicate application id in both the datasets.


demographic_df<-subset(demographic_df,Application.ID!=c(765011468,653287861,671989187)) 

credit_df<-subset(credit_df,Application.ID!=c(765011468,653287861,671989187)) 


# Merging by common attributes and by unique rows  
merged_df<- merge(x = unique(demographic_df)
                  , y = unique(credit_df)
                  , by = c("Application.ID", "Performance.Tag"))

nrow(merged_df)
# Dropping ID column as it is of no use.
merged_df<-merged_df[,-1]

#Duplicate rows in data - none found.
sum(duplicated(merged_df))

#Finding rows where dependant variable-"Performance.Tag" is not populated. 
no_perf_tag_rows<-merged_df[which( is.na(merged_df$Performance.Tag)),]

nrow(no_perf_tag_rows)/nrow(merged_df) 

# Only 1.9% of the rows have NA values for dependant variable - 'perfromance.tag'
# Assumption 1 - So model should be built on data where credit card was approved(0/1). 
# dependant variable - 'perfromance.tag' = NA for applicants for which credit was not issued in first place.
# So removing these rows.

data_for_eda <- merged_df[!is.na(merged_df$Performance.Tag) == TRUE,]

str(data_for_eda)

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

####################
## corr analysis will indicate the same way for multi variate analysis.
## https://www.r-bloggers.com/r-tutorial-series-scatterplots/
#http://r4ds.had.co.nz/exploratory-data-analysis.html
#https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619

#does they have diff credit card?
#home -car loans impact on default??
#income variations to default??
#Find out and Write down the insights like above.
#sub-population_statistics

##################### Univariate analysis######################################

out_df<-data.frame(prop.table(table(data_for_eda$Performance.Tag)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")

out_df<-data.frame(prop.table(table(data_for_eda$Gender)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")

out_df<-data.frame(prop.table(table(data_for_eda$Education)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")

out_df<-data.frame(prop.table(table(data_for_eda$Profession)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")

out_df<-data.frame(prop.table(table(data_for_eda$Marital.Status..at.the.time.of.application.)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")

 
out_df<-data.frame(prop.table(table(data_for_eda$Type.of.residence)*100))
ggplot(out_df,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")


hist(data_for_eda$Age, xlab = "Age")
boxplot(data_for_eda$Age,horizontal = T)  
## most users are in late 30 to early 50 age range.
## Some outliers(very small age value, may be invalid) values are present.


hist(data_for_eda$Income, xlab = "Income")
boxplot(data_for_eda$Income,horizontal = T) 
## Most users are in 15 to 40 income range.
## Not many outliers found.


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



IV <- create_infotables(data=data_for_eda, y="Performance.Tag", bins=10, parallel=T)

head(IV)
 
IV_Value = data.frame(IV$Summary)

grid.table(IV$Summary[seq(from=1,to=20,by=1),], rows=NULL)
 
plotFrame <- IV$Summary[order(-IV$Summary$IV), ]
plotFrame$Variable <- factor(plotFrame$Variable, levels = plotFrame$Variable[order(-plotFrame$IV)])
ggplot(plotFrame, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "darkblue", fill = "white") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))



##################Correlation analysis#############################

#install.packages('corrplot')

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

ggplot(data=data_for_eda, aes(x=Outstanding.Balance, y=Total.No.of.Trades, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean')
 ## Total no of trades is relatively lower for default users. 
## Also outstanding balance iis lower for most of default users.
 

ggplot(data=data_for_eda, aes(x=Income, y=No.of.times.90.DPD.or.worse.in.last.6.months, group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
##With increasing Income, DPD nos are decreasing. 
##Also for defaulting users DPD nos are way higher.
## High no of defaulters are in lower to medium income range. 

ggplot(data=data_for_eda, aes(x=No.of.Inquiries.in.last.12.months..excluding.home...auto.loans., y=Total.No.of.Trades , group=Performance.Tag, color=Performance.Tag))+ 
  geom_line(stat='summary', fun.y='mean') + 
  geom_point(stat='summary', fun.y='mean') 
##With increasing no of inquiries in last 12months, 
## total no of trades increases, then gradually it becomes constant.
## for default users total no of trades is higher.

table(data_for_eda$Performance.Tag)
prop.table(table(data_for_eda$Performance.Tag))
## Only 4% of observations are under default category. 
## So it is a highly imbalanced data which would result  in-effictive models if not treated properly.


################Scaling numeric columns & creating dummies for factor attributes#############

data_for_scaling<-data.frame(sapply(data_for_eda[numeric_cols], scale))

head(data_for_scaling)

str(data_for_scaling)

data_for_creating_dummies <- data_for_eda[fact_cols] 

str(data_for_creating_dummies) 

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(data_for_creating_dummies, 
                            function(x) data.frame(model.matrix(~x-1,data =data_for_creating_dummies))[,-1])) 

# combine all relevant columns to build final training data
final_df<- cbind(data_for_eda[event_col],data_for_scaling[numeric_cols],dummies)

final_df$Performance.Tag<-as.factor(final_df$Performance.Tag)

str(final_df)

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

str(test)

###########  SMOTE(synthetic minority oversampling technique) by ROSE package
#SMOTE algorithm creates artificial data based on feature space (rather than data space) 
# similarities from minority samples. It generates a random set of minority class observations 
# to shift the classifier learning bias towards minority class.


#Generate data synthetically to avoid errors related to explicitely mentioned probability

train <- ROSE(Performance.Tag ~ ., data = data_for_sampling, seed = 1)$data

table(train$Performance.Tag)

str(train)
###################Lets create a Logistic Model: Logistic Regression#####################

logistic_1 <- glm(Performance.Tag ~ ., family = "binomial", data = train)

summary(logistic_1)

#---------------------------------------------------------    

# Using stepwise algorithm for removing insignificant variables 

#logistic_2 <- stepAIC(logistic_1, direction = "both")


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

View(test_pred)

# Let's use the probability cutoff of 50%.
test_pred_default <- as.factor(ifelse(test_pred >= 0.50, 1,0))
test_actual_default <-  as.factor(ifelse(test$Performance.Tag==1,1,0))

conf_mtr_50_cutoff <- confusionMatrix(test_pred_default, test_actual_default)

conf_mtr_50_cutoff 
 
#     0     1
#0 11928   333
#1  8107   591

#Accuracy : 0.5973         
#Kappa : 0.0469         
#Sensitivity : 0.59536        
#Specificity : 0.63961  

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
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#from grid it looks like cutoff = 0.502


test_pred_optimal<- as.factor(ifelse(test_pred >= 0.502, 1,0))
optimal_conf <- confusionMatrix(test_pred_optimal, test_actual_default)
optimal_conf

# for 50.2% threshold,  accuracy = 60.27% , specificity = 60.15% , sensitivity = 62.98%
# So cutoff value is 0.502 for final model
 

####################### KS -statistic - Test Data ###################
 
pred_object_test<- prediction(as.numeric(test_pred_optimal), as.numeric(test_actual_default))

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)   ## 0.231

#KS-statistic is 23% 

#ROC Curve

plot(performance_measures_test, colorize = TRUE, text.adj = c(-0.2,1.7))

auc_ROCR <- performance(pred_object_test, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
##Area under curve is : 0.615



 