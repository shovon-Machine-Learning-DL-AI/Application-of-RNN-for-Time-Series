#***********************SMOTE*********************************

#https://www.datasciencecentral.com/profiles/blogs/handling-imbalanced-data-sets-in-supervised-learning-using-family?utm_content=buffer015ff&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer


install.packages("DMwR")
install.packages("smotefamily")
library(DMwR)


rm(list = ls())

setwd("C:\\Users\\a491441\\Desktop\\DSB Details")
data = read.csv("Dev_Sample_SMOTE.csv")
View(head(data))
str(data)
#Convert all the categorical variables to numeric one
data_new<-data.frame(lapply(data,function(x) as.integer(as.character(x))))
str(data_new)
View(data_new)

# Encoding the target feature as factor
data_new$Cluster<-factor(data_new$Cluster, levels = c(1,0))
str(data_new)
library(psych)
describe(data_new)
dim(data_new)

#Checking the Frequency Distribution
table(data_new$Cluster)/nrow(data_new)

#Feature selection based on Judgement
data_new_1 = subset(data_new, select = c(Age,DC_Asset_Current,EE_Contrib_2016,Hardship_Wd,
                                         Tot_Amt_Loan_Init_24M,XO_Ind,Male_Ind))
str(data_new_1)
colnames(data_new_1)

#Summary Checks
summary(data_new_1)

# Scale the data
x<-data.matrix(data_new_1)
range_data<-function(x){(x-min(x))/(max(x)-min(x))}
min_data<-min(x)
max_data<-max(x)
x<-range_data(x)
View(head(x))
max(x)
min(x)

colnames(data_new)
data_new_2<-as.data.frame(data_new[,60])
# data_new_2<-as.data.frame(data_new$Cluster)
View(data_new_2)
colnames(data_new_2)
# Rename the Column Name
colnames(data_new_2)[1] <- "Cluster"
colnames(data_new_2)
View(data_new_2)

# Append the column to create the Final Data Set
# Create the Overall data structure
x<-as.data.frame(x)
x<-cbind(x,data_new_2)
View(x)

# First Use the Unscaled Data
data_final<-as.data.frame(cbind(data_new_1,data_new_2))
View(data_final)

#*************Oversmapling Technique**********************

#***********************************************SMOTE********************************

#1-SMOTE: Synthetic Minority Over sampling Technique (SMOTE) algorithm applies 
#KNN approach where it selects K nearest neighbors, 
#joins them and creates the synthetic samples in the space. 
#The algorithm takes the feature vectors and its nearest neighbors, 
#computes the distance between these vectors. 
#The difference is multiplied by random number between (0, 1) and it is added back to feature. 
#SMOTE algorithm is a pioneer algorithm and many other algorithms are derived from SMOTE.

#form	
#A formula describing the prediction problem

#data	
#A data frame containing the original (unbalanced) data set

#perc.over	
#A number that drives the decision of how many extra cases from the minority class are generated (known as over-sampling).

#k	
#A number indicating the number of nearest neighbours that are used to generate the new examples of the minority class.

#perc.under	
#A number that drives the decision of how many extra cases from the majority classes are selected for each case generated from the minority class (known as under-sampling)

#learner	
#Optionally you may specify a string with the name of a function that implements a classification algorithm that will be applied to the resulting SMOTEd data set (defaults to NULL).

str(data_final)

library(DMwR)


Dev_Smote <- SMOTE(Cluster ~ ., data_final, perc.over = 200, perc.under=200)
table(Dev_Smote$Cluster) 

#**********Adaptive Synthetic Sampling Approach for Imbalanced Learning (ADASYN)**************

#2- ADASYN:  ADAptive SYNthetic (ADASYN) is based on the idea of adaptively generating minority data samples 
#according to their distributions using K nearest neighbor.
#The algorithm adaptively updates the distribution and there are no assumptions made 
#for the underlying distribution of the data.  The algorithm uses Euclidean distance for KNN Algorithm. 
#The key difference between ADASYN and SMOTE is that the former uses a density distribution, 
#as a criterion to automatically decide the number of synthetic samples that must be generated 
#for each minority sample by adaptively changing the weights of the different minority samples to compensate 
#for the skewed distributions. 
#The latter generates the same number of synthetic samples for each original minority sample.


#X	
#A data frame or matrix of numeric-attributed dataset

#target	
#A vector of a target class attribute corresponding to a dataset X.

#K	
#The number of nearest neighbors during sampling process

install.packages("FNN")
library(FNN)
library(smotefamily)
ADAS_Test = ADAS(data_final[,-8],data_final[,8],K=5)
summary(ADAS_Test)

print(ADAS_Test)


#**********ANS: Adaptive Neighbor Synthetic (ANS) **************

#3- ANS: Adaptive Neighbor Synthetic (ANS) dynamically adapts the number of neighbors needed 
#for oversampling around different minority regions. 
#This algorithm eliminates the parameter K of SMOTE for a dataset and assign different number of neighbors 
#for each positive instance. 
#Every parameter for this technique is automatically set within the algorithm making it become parameter free.


#X	
#A data frame or matrix of numeric-attributed dataset

#target	
#A vector of a target class attribute corresponding to a dataset X.

#dupSize	
#A number of vector representing the desired times of synthetic minority instances over the 
#original number of majority instances, 0 for balanced dataset.

library(FNN)
library(smotefamily)

data_final1<-data.frame(lapply(data_final,function(x) as.numeric(as.character(x))))
str(data_final1)

ANS_Test = ANS(data_final1,data_final1$Cluster,dupSize = 0)
str(data_final)
summary(ADAS_Test)

#**********4- Border SMOTE: **************

#Borderline-SMOTE generates the synthetic sample along the borderline of minority and majority classes. 
#This also helps in separating out the minority and majority classes.

#X	
#A data frame or matrix of numeric-attributed dataset

#target	
#A vector of a target class attribute corresponding to a dataset X.

#K	
#The number of nearest neighbors during sampling process

#C	
#The number of nearest neighbors during calculating safe-level process

#dupSize	
#The number or vector representing the desired times of synthetic minority instances over 
#the original number of majority instances, 0 for duplicating until balanced

#method	
#A parameter to indicate which type of Borderline-SMOTE presented in the paper is used

library(FNN)
library(smotefamily)

data_final1<-data.frame(lapply(data_final,function(x) as.numeric(as.character(x))))
str(data_final1)

BLSMOTE_Test = BLSMOTE(data_final1,data_final1$Cluster,dupSize = 0,method =c("type1","type2"))
str(BLSMOTE_Test)
summary(BLSMOTE_Test)

#**********5-Safe Level SMOTE: **************

#Safe level is defined as the number of a positive instances in k nearest neighbors.
#If the safe level of an instance is close to 0, the instance is nearly noise.
#If it is close to k, the instance is considered safe. 
#Each synthetic instance is generated in safe position by considering the safe level ratio of instances. 
#In contrast, SMOTE and Borderline-SMOTE may generate synthetic instances in unsuitable locations, 
#such as overlapping regions and noise regions.

#X	
#A data frame or matrix of numeric-attributed dataset

#target	
#A vector of a target class attribute corresponding to a dataset X.

#K	
#The number of nearest neighbors during sampling process

#C	
#The number of nearest neighbors during calculating safe-level process

#dupSize	
#The number or vector representing the desired times of synthetic minority instances over the original 
#number of majority instances

library(FNN)
library(smotefamily)

data_final1<-data.frame(lapply(data_final,function(x) as.numeric(as.character(x))))
str(data_final1)

RSLS_Test = RSLS(data_final1,data_final1$Cluster, K = 5, C = 5, dupSize = 0)

str(RSLS_Test)
summary(RSLS_Test)


#**********6- DBSMOTE:**************

#Density-Based Synthetic Minority Over-sampling Technique is based on clustering algorithm DBSCAN. 
#The clusters are discovered by DBSCAN Algorithm. DBSMOTE generates synthetic instances along a 
#shortest path from each positive instance to a pseudo-centroid of a minority-class cluster
                   

#X	
#A data frame or matrix of numeric-attributed dataset

#target	
#A vector of a target class attribute

#dupSize	
#A number of vector representing the desired times of synthetic minority instances over the original 
#number of majority instances

#MinPts	
#The minimum instance parameter to decide whether each instance inside eps is reachable, 
#the automatic algorithm is used to find the value instead if there is no positive integer value given for it.

#eps	
#The radius to consider neighbor.


library(FNN)
library(smotefamily)
install.packages("dbscan")
install.packages("igraph")

library(dbscan)
libname(igraph)

data_final1<-data.frame(lapply(data_final,function(x) as.numeric(as.character(x))))
str(data_final1)

DBSMOTE_Test = DBSMOTE(data_final1,data_final1$Cluster, MinPts = NULL, eps = NULL, dupSize = 0)

str(RSLS_Test)
summary(RSLS_Test)

#******************Intercept Adjustment in Logistic Regression (In SAS)***********************************

#http://support.sas.com/kb/22/601.html