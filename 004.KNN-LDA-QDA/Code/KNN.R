/********************Application of KNN on the BC Data*****************/
  rm(list = ls())
# First change the following directory link to where all the input files do exist
# KNN Classifier
setwd("C:\\Users\\a589565\\Desktop\\R-Files\\KNN_SFML")
breast_cancer = read.csv("B-C_dataset.csv")
View(head(breast_cancer))
str(breast_cancer)
# Column Bare.Nuclei have some missing values with "?" in place, we are replacing with median values
# As Bare.Nuclei is discrete variable
breast_cancer$Bare.Nuclei = as.character(breast_cancer$Bare.Nuclei)
breast_cancer$Bare.Nuclei[breast_cancer$Bare.Nuclei=="?"] = median(breast_cancer$Bare.Nuclei,na.rm = TRUE)
breast_cancer$Bare.Nuclei = as.integer(breast_cancer$Bare.Nuclei)

# Classes are 2 & 4 for benign & malignant respectively, we have converted
# to zero-one problem, as it is easy to convert to work around with models
breast_cancer$Cancer_Ind = 0
breast_cancer$Cancer_Ind[breast_cancer$Class==4]=1
breast_cancer$Cancer_Ind = as.factor(breast_cancer$Cancer_Ind)

# We have removed unique id number from modeling as unique numbers doesnot provide value in modeling 
# In addition, origninal class variable also will be removed as the same has been replaced with derived variable
remove_cols = c("ID_Number","Class")
breast_cancer_new = breast_cancer[,!(names(breast_cancer) %in% remove_cols)]
View(breast_cancer_new)
str(breast_cancer_new)
# Setting seed value for producing repetitive results
# 70-30 split has been made on the data
set.seed(123)
numrow = nrow(breast_cancer_new)
trnind = sample(1:numrow,size = as.integer(0.7*numrow))
train_data = breast_cancer_new[trnind,]
test_data = breast_cancer_new[-trnind,]

# Following is classical code for computing  accuracy, precision & recall
frac_trzero = (table(train_data$Cancer_Ind)[[1]])/nrow(train_data)
frac_trone = (table(train_data$Cancer_Ind)[[2]])/nrow(train_data)

frac_tszero = (table(test_data$Cancer_Ind)[[1]])/nrow(test_data)
frac_tsone = (table(test_data$Cancer_Ind)[[2]])/nrow(test_data)

prec_zero <- function(act,pred){  tble = table(act,pred)
return( round( tble[1,1]/(tble[1,1]+tble[2,1]),4)  ) }

prec_one <- function(act,pred){ tble = table(act,pred)
return( round( tble[2,2]/(tble[2,2]+tble[1,2]),4)   ) }

recl_zero <- function(act,pred){tble = table(act,pred)
return( round( tble[1,1]/(tble[1,1]+tble[1,2]),4)   ) }

recl_one <- function(act,pred){ tble = table(act,pred)
return( round( tble[2,2]/(tble[2,2]+tble[2,1]),4)  ) }

accrcy <- function(act,pred){ tble = table(act,pred)
return( round((tble[1,1]+tble[2,2])/sum(tble),4)) }

# Importing Class package in which KNN function do present
library(class)
# Chossing sample k-value as 3 & apply on train & test data respectively
k_value = 3
tr_y_pred = knn(train_data,train_data,train_data$Cancer_Ind,k=k_value)
ts_y_pred = knn(train_data,test_data,train_data$Cancer_Ind,k=k_value)

# Calculating confusion matrix, accuracy, precision & recall on train data
tr_y_act = train_data$Cancer_Ind;ts_y_act = test_data$Cancer_Ind
tr_tble = table(tr_y_act,tr_y_pred)
print(paste("Train Confusion Matrix"))
print(tr_tble)

tr_acc = accrcy(tr_y_act,tr_y_pred)
trprec_zero = prec_zero(tr_y_act,tr_y_pred); trrecl_zero = recl_zero(tr_y_act,tr_y_pred)
trprec_one = prec_one(tr_y_act,tr_y_pred); trrecl_one = recl_one(tr_y_act,tr_y_pred)

trprec_ovll = trprec_zero *frac_trzero + trprec_one*frac_trone
trrecl_ovll = trrecl_zero *frac_trzero + trrecl_one*frac_trone

print(paste("KNN Train accuracy:",tr_acc))
print(paste("KNN - Train Classification Report"))
print(paste("Zero_Precision",trprec_zero,"Zero_Recall",trrecl_zero))
print(paste("One_Precision",trprec_one,"One_Recall",trrecl_one))
print(paste("Overall_Precision",round(trprec_ovll,4),"Overall_Recall",round(trrecl_ovll,4)))

# Calculating confusion matrix, accuracy, precision & recall on test data
ts_tble = table(ts_y_act,ts_y_pred)
print(paste("Test Confusion Matrix"))
print(ts_tble)

ts_acc = accrcy(ts_y_act,ts_y_pred)
tsprec_zero = prec_zero(ts_y_act,ts_y_pred); tsrecl_zero = recl_zero(ts_y_act,ts_y_pred)
tsprec_one = prec_one(ts_y_act,ts_y_pred); tsrecl_one = recl_one(ts_y_act,ts_y_pred)

tsprec_ovll = tsprec_zero *frac_tszero + tsprec_one*frac_tsone
tsrecl_ovll = tsrecl_zero *frac_tszero + tsrecl_one*frac_tsone

print(paste("KNN Test accuracy:",ts_acc))
print(paste("KNN - Test Classification Report"))
print(paste("Zero_Precision",tsprec_zero,"Zero_Recall",tsrecl_zero))
print(paste("One_Precision",tsprec_one,"One_Recall",tsrecl_one))
print(paste("Overall_Precision",round(tsprec_ovll,4),"Overall_Recall",round(tsrecl_ovll,4)))



# Tuning of K-value on Train & Test Data
k_valchart = data.frame(matrix( nrow=5, ncol=3))
colnames(k_valchart) = c("K_value","Train_acc","Test_acc")

k_vals = c(1,2,3,4,5)

i = 1
for (kv in k_vals) {
  
  tr_y_pred = knn(train_data,train_data,train_data$Cancer_Ind,k=kv)
  ts_y_pred = knn(train_data,test_data,train_data$Cancer_Ind,k=kv)
  
  tr_y_act = train_data$Cancer_Ind;ts_y_act = test_data$Cancer_Ind
  
  tr_tble = table(tr_y_act,tr_y_pred)
  print(paste("Train Confusion Matrix"))
  print(tr_tble)
  
  tr_acc = accrcy(tr_y_act,tr_y_pred)
  trprec_zero = prec_zero(tr_y_act,tr_y_pred); trrecl_zero = recl_zero(tr_y_act,tr_y_pred)
  trprec_one = prec_one(tr_y_act,tr_y_pred); trrecl_one = recl_one(tr_y_act,tr_y_pred)
  
  trprec_ovll = trprec_zero *frac_trzero + trprec_one*frac_trone
  trrecl_ovll = trrecl_zero *frac_trzero + trrecl_one*frac_trone
  
  print(paste("KNN Train accuracy:",tr_acc))
  print(paste("KNN - Train Classification Report"))
  print(paste("Zero_Precision",trprec_zero,"Zero_Recall",trrecl_zero))
  print(paste("One_Precision",trprec_one,"One_Recall",trrecl_one))
  print(paste("Overall_Precision",round(trprec_ovll,4),"Overall_Recall",round(trrecl_ovll,4)))
  
  ts_tble = table(ts_y_act,ts_y_pred)
  print(paste("Test Confusion Matrix"))
  print(ts_tble)
  
  ts_acc = accrcy(ts_y_act,ts_y_pred)
  tsprec_zero = prec_zero(ts_y_act,ts_y_pred); tsrecl_zero = recl_zero(ts_y_act,ts_y_pred)
  tsprec_one = prec_one(ts_y_act,ts_y_pred); tsrecl_one = recl_one(ts_y_act,ts_y_pred)
  
  tsprec_ovll = tsprec_zero *frac_tszero + tsprec_one*frac_tsone
  tsrecl_ovll = tsrecl_zero *frac_tszero + tsrecl_one*frac_tsone
  
  print(paste("KNN Test accuracy:",ts_acc))
  print(paste("KNN - Test Classification Report"))
  print(paste("Zero_Precision",tsprec_zero,"Zero_Recall",tsrecl_zero))
  print(paste("One_Precision",tsprec_one,"One_Recall",tsrecl_one))
  print(paste("Overall_Precision",round(tsprec_ovll,4),"Overall_Recall",round(tsrecl_ovll,4)))
  
  
  k_valchart[i,1] =kv
  k_valchart[i,2] =tr_acc
  k_valchart[i,3] =ts_acc
  
  i = i+1
  
}


# Plotting the graph
library(ggplot2)
library(grid)

ggplot(k_valchart, aes(K_value)) + 
  geom_line(aes(y = Train_acc, colour = "Train_Acc")) + 
  geom_line(aes(y = Test_acc, colour = "Test_Acc"))+
  labs(x="K_value",y="Accuracy") +
  geom_text(aes(label = Train_acc, y = Train_acc), size = 3)+
  geom_text(aes(label = Test_acc, y = Test_acc), size = 3)

