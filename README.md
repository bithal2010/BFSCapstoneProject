# BFSCapstoneProject

 Agenda for Mentor call -1
 [3rd-Jun-2018]
 
A. Understanding the business problem 
--------------------------------------
1.Identify the right customers using predictive models. Mainly focusing on “Acquisition risk analytics”
2.Determine the factors affecting credit risk – Identifying relevant demographic/credit data parameters which are impacting     Credit default behaviour. Segmentation on basis of different attributes.
3.Create strategies to mitigate the acquisition risk - Build an application scorecard with the good to bad odds 
  between 10 to 1. Come up with a cut off value /range for decision making.
4.Assess the financial benefit of your project. – Come up with Gain chart, Lift chart 


B. Understanding the DATA
--------------------------
Two datasets have been provided for the task. 
1.Demographic data – Contains customer information from their loan/credit application.
	1.1. Contains 12 attributes including ID and default-indicator. Five continuous variables are there which need to be binned into ranges and converted to relevant factor variables.
1.2 Demographic data need to be used for both demographic data modelling and master data modelling.
1.3 Total 71296 records available out of which 2948 (4.1%)records are in “default” category.
2.Credit bureau data – Data from the credit bureau. 
2.1 Contains payment behaviour of 71296 credit card holders in 19 different columns which include ID and default-indicator.
2.2 From delinquency data recorded for last 6 months and 12 months’ period we need to derive roll-over matrix which can be analysed further.
2.3 Relationship between other existing home/auto loans/credit card utilisation need to be analysed.





C. Solution Approach
---------------------------------
S1. Explore the given datasets. EDA Plots are to be done using Tableau /GGplot2.
S2. Data preparation to be done using R. Weight of Evidence (WOE) /Information value(IV) to be extracted for all attributes. Look for inconsistencies, missing values, patterns using ‘R’. We can handle missing value scenarios using WOE values.
S3. Separate dataset with WOE values only need to be extracted and analyzed.
S3. Modelling using Logistic regression, Decision tree and random forest to be done on demographic data to understand the predictive power of user application data.
S4. Modelling using Logistic regression, SVM, Decision tree and random forest to be done on master data(demographic +credit bureau data) to understand the overall set of important parameters.
S5. After relevant hyper-parameter tuning, model evaluation to be done with confusion matrix, Gain chart, KS index etc.
S6. Application score card to be developed from final model. Cutoff score/range need to be highlighted.
S7. Financial gains need to be shown from Gain chart.  	
S8. Strategic suggestions need to be presented along with set of assumptions.


D. Open Queries/Clarifications
---------------------------------
Not able to understand the application scorecard to be devised. How to implement it. Could anyone please explain a bit?
How delinquency data can be used into modelling?Do we analyse the roll over matrix as explained in previous theory sessions or Is it going to be added to models??
	2.1 These are continuous variables.  you can use them as it is. Or create a WOE value and use it.
	2.2 Once modelling is done, we will evaluate diff parameters take decisions .here roll-over metric might help.
Should we segment users explicitly or model will take care of itself. If yes for segmentation, what are pre-defined segments we should be aiming to , Do we need to some clustering before hand to modelling? 
	3.1 segment creation to be done in EDA steps. 
 What are the major assumptions we should be making ?  Will it flow from histogram/EDA steps ?
	4.1.  missing values
	4.2  graphs
	4.3 outliers
	4.4 Correlation matrix
	4.5 WOE value for all fields , replace missing values
 Make rough note from EDA with google doc and share. Imp variables would be listed and cross-validated at last step. Keep insights ready.
How should we be handling missing values with WOE or IV ?? How to identify the important variables by WOE?
Apart from lift chart/gain chart is there any other industry level  evaluation matrix we  are  supposed to use/know?
What should be approach to work together? End-to-end approach for each of us is feasible or not?

From perspective of performance tags, is the dataset balanced? Percentage of default users is far far less. How to solve this? this to be done after EDA , before modelling  .
>> Random forest / boosting would take care of it internally. Regression model needs it. 
>> Up-sampling 
or 
>> down-sampling
or 
>> smote 

Good guidline provided in below link. ROSE package can be used for handling un-balanced dataset.

https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/

objective — >  predict what is probability of default if credit card is approved?
Assumption —> 
1 . So model should be built on data where credit card was approved(0/1).  So ignore the data row where performance_tag is missing.  



