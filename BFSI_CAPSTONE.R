## objective — > predict what is probability of default if credit card is approved? 

#Read given CSV files into individual dataframes and then merging them 
demographic_df<- read.csv(file = 'Demographic data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')
  
credit_buraeu_df<- read.csv(file = 'Credit Bureau data.csv',header = T,stringsAsFactors = T, na.strings = 'NA')
  
master_df<- merge(x = demographic_df, y = credit_buraeu_df, by = 'Application.ID')

#Finding rows where dependant variable is not populated. 
null_rows<-length(which( is.na(master_df$Performance.Tag.x) == TRUE))
 
null_rows/nrow(master_df) 

# Only 1.9% of the rows have NA values for dependant variable - 'perfromance.tag'
# Assumption 1 - So model should be built on data where credit card was approved(0/1). 
# dependant variable - 'perfromance.tag' = NA for applicants for which credit was not issued in first place.
# So removing these rows.

which( is.na(master_df$Performance.Tag.x) == TRUE)

data_for_eda <- master_df[!is.na(master_df$Performance.Tag.x) == TRUE,]

data_for_eda <- master_df[!is.na(master_df$Performance.Tag.y) == TRUE,]

# Getting a summary of master data
summary(data_for_eda)

#Removing ID column and duplicate perfromance-tag column
data_for_eda<- data_for_eda[-c(1,30)]

colnames(data_for_eda)

## Let's perform Segmentation/binning of some continuous variables such as - 
## age, income, avg-cc-utilization,  residency-duration , job-duration etc.

summary(data_for_eda$Age)

#Invalid negative/zero value for age column populated for some row i.e. 0, -3
invalid_age_index <-which(data_for_eda$Age < 10)

#populating median values for all these rows
data_for_eda$Age[invalid_age_index] <-45

#creating new factor column age_group from age column
data_for_eda$age_group<- findInterval(data_for_eda$Age, c(20, 30, 40,50,60,70))

data_for_eda$age_group<-as.factor(data_for_eda$age_group)

str(data_for_eda$age_group)
summary(data_for_eda$age_group)


#creating new factor column age_group from age column
summary(data_for_eda$Income)

#Invalid negative/zero value for Income column populated for some row
invalid_income_index <-which(data_for_eda$Income < 0)

#populating median values for all these rows
data_for_eda$Income[invalid_income_index] <-27
summary(data_for_eda$Income)

data_for_eda$income_group <- findInterval(data_for_eda$Income, c(10,20,30,40,50,60))

data_for_eda$income_group <-as.factor(data_for_eda$income_group)

str(data_for_eda$income_group )
summary(data_for_eda$income_group )


#creating new factor column avg_cc_utilization_group from 'Avgas.CC.Utilization.in.last.12.months' column
summary(data_for_eda$Avgas.CC.Utilization.in.last.12.months)

data_for_eda$avg_cc_utilization <- 
  findInterval(data_for_eda$Avgas.CC.Utilization.in.last.12.months, c(15,30,45,60,75,90,105,120))

data_for_eda$avg_cc_utilization <-as.factor(data_for_eda$avg_cc_utilization)

str(data_for_eda$avg_cc_utilization )
summary(data_for_eda$avg_cc_utilization )

#creating new factor column job_recency from 'No.of.months.in.current.company' column
summary(data_for_eda$No.of.months.in.current.company)

data_for_eda$job_recency <- 
  findInterval(data_for_eda$No.of.months.in.current.company, c(24,48,72,96,120,144))

data_for_eda$job_recency <-as.factor(data_for_eda$job_recency)

str(data_for_eda$job_recency )
summary(data_for_eda$job_recency )
data_for_eda$No.of.months.in.current.residence

#creating new factor column house_recency from 'No.of.months.in.current.residence' column
summary(data_for_eda$No.of.months.in.current.residence)

data_for_eda$house_recency <- 
  findInterval(data_for_eda$No.of.months.in.current.residence, c(24,48,72,96,120,144))

data_for_eda$house_recency <-as.factor(data_for_eda$house_recency)

str(data_for_eda$house_recency )
summary(data_for_eda$house_recency )


## Getting ready fro deriving WOE /IV values for all columns
install.packages("Information")
library(Information)

