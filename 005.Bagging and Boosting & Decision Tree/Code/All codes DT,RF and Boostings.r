##############################################################################################
##############################################################################################
##############################################################################################

# Reading Data

setwd("C:\\Users\\a594010\\Documents\\PERSONAL NOTES\\DATA SCIENCE BOOTCAMP MATERIALS")
getwd()

data <-read.csv("sample_data_classification.csv", header=TRUE)

data$Industry_Name <- as.factor(data$Industry_Name)
data$GNDR_CD <- as.factor(data$GNDR_CD)
data$ target_var<- as.factor(data$ target_var)
data$MARITAL_STATUS_DESC <- as.factor(data$MARITAL_STATUS_DESC)

# Creating Test and Train Data

set.seed(425)
data_event <- data[ which(data$target_var=='1'), ]
data_nonevent <- data[ which(data$target_var=='0'), ]
partition1 = sort(sample(nrow(data_event), nrow(data_event)*.7))
train_event <- data_event[partition1,]
test_event <- data_event[-partition1,]
partition2 = sort(sample(nrow(data_nonevent), nrow(data_nonevent)*.7))
train_nonevent <- data_nonevent[partition2,]
test_nonevent <- data_nonevent[-partition2,]
train = rbind(train_event,train_nonevent)
test = rbind(test_event,test_nonevent)

rm(data_event,data_nonevent,train_event,train_nonevent,test_event,test_nonevent)


##############################################################################################
##############################################################################################
##############################################################################################

#Model rpart

library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)

# rattle()

model <- rpart(train$target_var ~ ., data = subset(train, select=c( -1 ) )
               , method = "class", minsplit = 100, minbucket = 10 ,maxdepth = 5,  cp = 0.0001)

printcp(model)
plotcp(model)
bestcp <- model$cptable[which.min(model$cptable[,"xerror"]),"CP"]
print(bestcp)
model.pruned <- prune(model, cp = bestcp)

plot(model.pruned)
text(model.pruned, cex = 0.8, use.n = TRUE, xpd = TRUE)
summary(model.pruned)
fancyRpartPlot(model.pruned)
asRules(model.pruned)

## training - confusion matrix
pred_train <- predict(model.pruned, train ,type = 'class') 
conf.matrix <- table(train$target_var, pred_train)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

## testing - confusion matrix
pred_test <- predict(model.pruned, test ,type = 'class') 
conf.matrix <- table(test$target_var, pred_test)
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

##############################################################################################
##############################################################################################
##############################################################################################

#XGBoost Model

#install.packages('xgboost')
#install.packages('readr')
#install.packages('stringr')

library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

#Drop the Indicator Variables

Dev_Sample = subset(train, select = -c(1,2))
View(head(Dev_Sample))

Val_Sample = subset(test, select = -c(1,2))
View(head(Val_Sample))

#Convert all the categorical variables to numeric one
Dev_Sample<-data.frame(lapply(Dev_Sample,function(x) as.numeric(as.character(x))))
View(head(Dev_Sample))
str(Dev_Sample)

Val_Sample<-data.frame(lapply(Val_Sample,function(x) as.numeric(as.character(x))))
View(head(Val_Sample))
str(Val_Sample)

# Encoding the target feature as factor
Dev_Sample$segment <-factor(Dev_Sample$segment, levels = c(0,1,2,3,4,5))

str(Dev_Sample)
dim(Dev_Sample)

Val_Sample$segment<-factor(Val_Sample$segment, levels = c(0,1,2,3,4,5))

str(Val_Sample)
dim(Val_Sample)


#Renaming the Variables for Simplicity

cross.sell.dev<-Dev_Sample
View(head(cross.sell.dev))
str(cross.sell.dev)

cross.sell.val<-Val_Sample
View(head(cross.sell.val))
str(cross.sell.val)

#Checking the Frequency Distribution

table(cross.sell.dev$segment)/nrow(cross.sell.dev)
table(cross.sell.val$segment)/nrow(cross.sell.val)

#******Running the XGBoost**********

#nrounds[default=100]
#For classification, it is similar to the number of trees to grow

#eta[default=0.3][range: (0,1)]
#It controls the learning rate, i.e., the rate at which our model learns patterns in data. 
#After every round, it shrinks the feature weights to reach the best optimum.

#gamma[default=0][range: (0,Inf)]
#It controls regularization (or prevents overfitting). 
#The optimal value of gamma depends on the data set and other parameter values.

#max_depth[default=6][range: (0,Inf)]
#It controls the depth of the tree

#min_child_weight[default=1][range:(0,Inf)]
#In regression, it refers to the minimum number of instances required in a child node

#subsample[default=1][range: (0,1)]
#It controls the number of samples (observations) supplied to a tree

#colsample_bytree[default=1][range: (0,1)]
#It control the number of features (variables) supplied to a tree

#lambda[default=0]
#It controls L2 regularization (equivalent to Ridge regression) on weights. It is used to avoid overfitting

#alpha[default=1]
#It controls L1 regularization (equivalent to Lasso regression) on weights. 

#Objective[default=reg:linear]
#reg:linear - for linear regression
#binary:logistic - logistic regression for binary classification. It returns class probabilities
#multi:softmax - multiclassification using softmax objective. 
#It returns predicted class labels. It requires setting num_class parameter denoting number of unique prediction classes.
#multi:softprob - multiclassification using softmax objective. It returns predicted class probabilities.


#eval_metric [no default, depends on objective selected]
#These metrics are used to evaluate a model's accuracy on validation data. 
#For regression, default metric is RMSE. For classification, default metric is error.
#Available error functions are as follows:
#mae - Mean Absolute Error (used in regression)
#Logloss - Negative loglikelihood (used in classification)
#AUC - Area under curve (used in classification)
#RMSE - Root mean square error (used in regression)
#error - Binary classification error rate [#wrong cases/#all cases]

#https://www.hackerearth.com/practice/machine-learning/machine-learning-algorithms/beginners-tutorial-on-xgboost-parameter-tuning-r/tutorial/


library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)

xgb <- xgboost(data = data.matrix(cross.sell.dev[,-9]), 
               label = cross.sell.dev$segment, 
               eta = 0.1,
               max_depth = 8, 
               nround=100, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softmax",
               num_class = 12,
               nthread = 3)

cross.sell.dev.pred <- predict(xgb, data.matrix(cross.sell.dev[,-9]))
View(head(cross.sell.dev.pred ))

cross.sell.val.pred <- predict(xgb, data.matrix(cross.sell.val[,-9]))
View(head(cross.sell.val.pred ))

#*******Creating Confusion metrics*********

#Development Sample

mydata1 <- cbind(cross.sell.dev,cross.sell.dev.pred)

View(head(mydata1))
colnames(mydata1)

cm <-table(mydata1[,9],cross.sell.dev.pred)
cm

#Validation Sample

mydata1 <- cbind(cross.sell.val,cross.sell.val.pred)
View(head(mydata1))
colnames(mydata1)

cm <-table(mydata1[,9],cross.sell.val.pred)
cm


##############################################################################################
##############################################################################################
##############################################################################################

# Generalised Boosting Model

install.packages('gbm')
install.packages('xgboost')
library(gbm)

cross.sell.dev.gbm <- cross.sell.dev[colSums(is.na(cross.sell.dev))/nrow(cross.sell.dev) != 1]

cross.sell.val.gbm <- cross.sell.val[colSums(is.na(cross.sell.dev))/nrow(cross.sell.dev) != 1]

model=gbm(segment~.,data=cross.sell.dev.gbm,
          distribution = "multinomial",n.trees=1000,interaction.depth = 6) 

pred=predict(model,newdata=cross.sell.dev.gbm[,-4],n.trees=1000)
p.pred<-apply(pred,1,which.max) 

data.new <- cbind(p.pred, cross.sell.dev.gbm[-4])

pred_val=predict(model,newdata=cross.sell.val.gbm[,-4],n.trees=1000)
p.pred_val<-apply(pred_val,1,which.max)

#*******Creating Confusion metrics*********

#Development Sample

mydata1 <- cbind(cross.sell.dev.gbm,p.pred)

View(head(mydata1))
colnames(mydata1)

cm <-table(mydata1[,4],p.pred)
cm

#Validation Sample

mydata1 <- cbind(cross.sell.val.gbm,p.pred_val)
View(head(mydata1))
colnames(mydata1)

cm <-table(mydata1[,4],p.pred_val)
cm


##############################################################################################
##############################################################################################
##############################################################################################

#Adaboost Model
install.packages('ada')

library(rpart)
library(ada)

#Adaboost process: Currently this procedure can not directly handle > 2 class response

#loss 
#loss="exponential", "ada","e" or any variation corresponds to the default boosting under exponential loss. 
#loss="logistic","l2","l" provides boosting under logistic loss.

#type 
#type of boosting algorithm to perform. “discrete” performs discrete Boosting (default). 
#“real” performs Real Boost. “gentle” performs Gentle Boost.

#iter 
#number of boosting iterations to perform. Default = 50.

#nu 
#shrinkage parameter for boosting, default taken as 1.

#bag.frac 
#sampling fraction for samples taken out-of-bag. This allows one to use random permutation which improves performance.

#model.coef 
#flag to use stageweights in boosting. If FALSE then the procedure corresponds to epsilon-boosting.

#bag.shift 
#flag to determine whether the stageweights should go to one as nu goes to zero. This only makes since if bag.frac is small. 
#The rationale behind this parameter is discussed in (Culp et al., 2006).

#max.iter 
#number of iterations to perform in the newton step to determine the coeficient.

#delta 
#tolarence for convergence of the newton step to determine the coeficient.

#verbose 
#print the number of iterations necessary for convergence of a coeficient.

#formula 
#a symbolic description of the model to be fit.

#data 
#an optional data frame containing the variables in the model.

#subset 
#an optional vector specifying a subset of observations to be used in the fitting process.

#na.action 
#a function that indicates how to process ‘NA’ values. Default=na.rpart.

library(rpart)
library(ada)

?ada

adabst <- ada(target_var~.,data=train[-c(1)], 
              loss="exponential", type="discrete", iter=100, max.iter=20, 
              nu=0.1, bag.frac=0.5, model.coef=TRUE,bag.shift=FALSE, delta=10^(-10),
              verbose=FALSE)

train.pred <- predict(adabst, train[,-c(1,2)])
View(head(train.pred))

test.pred <- predict(adabst, test[,-c(1,2)])
View(head(test.pred ))

#*******Creating Confusion metrics*********

#Development Sample

mydata1 <- cbind(train,train.pred)

View(head(mydata1))
colnames(mydata1)

cm <-table(mydata1[,2],train.pred)
cm

#Validation Sample

mydata1 <- cbind(test,test.pred)
View(head(mydata1))
colnames(mydata1)

cm <-table(mydata1[,2],test.pred)
cm


##############################################################################################
##############################################################################################
##############################################################################################

# Bagged Model 

install.packages('adabag')

library(rpart)
library(adabag)

#Bagging Process

#mfinal an integer, the number of iterations for which boosting is run 
#or the number of trees to use. Defaults to mfinal=100 iterations.

model.bagging <- bagging(segment~.,data=cross.sell.dev.gbm, boos=TRUE,
                         mfinal=500)

#Confusion matrix

model.bagging.Dev.pred <-predict.bagging(model.bagging, newdata=cross.sell.dev.gbm[-4,])
model.bagging.Dev.pred

model.bagging.Val.pred <-predict.bagging(model.bagging, newdata=cross.sell.val.gbm[-4,])
model.bagging.Val.pred


##############################################################################################
##############################################################################################
##############################################################################################

# Random Forest

#/**********************Working on Development Sample***********************/

#Install and load packages required for random forest

install.packages("party")
install.packages("randomForest")
install.packages("ROCR")

library(party)
library(ROCR)
library(randomForest)


#Convert all the categorical variables to numeric one
cross.sell.dev.gbm<-data.frame(lapply(cross.sell.dev.gbm,function(x) as.numeric(as.character(x))))
View(head(cross.sell.dev.gbm))
str(cross.sell.dev.gbm)

cross.sell.dev.gbm<-data.frame(lapply(cross.sell.dev.gbm,function(x) as.numeric(as.character(x))))
View(head(cross.sell.dev.gbm))
str(cross.sell.dev.gbm)


# Encoding the target feature as factor

cross.sell.dev.gbm$segment <-factor(cross.sell.dev.gbm$segment, levels = c(0,1,2,3,4,5))

str(cross.sell.dev.gbm)
dim(cross.sell.dev.gbm)

cross.sell.dev.gbm2 <- cross.sell.dev.gbm[colSums(is.na(cross.sell.dev.gbm))/nrow(cross.sell.dev.gbm) == 0]

cross.sell.val.gbm2 <- cross.sell.val.gbm[colSums(is.na(cross.sell.dev.gbm))/nrow(cross.sell.dev.gbm) == 0]

# Step II : Run the following code several times with different "ntree" values

library(randomForest)
set.seed(2017)
head(Dev_Sample)
colnames(Dev_Sample)

#Random Forest - generic

classifier<-randomForest(x=cross.sell.dev.gbm2[-4],
                         y=cross.sell.dev.gbm2$segment,
                         importance=TRUE,
                         ntree=500)

# Hyper-parameter Fine tuning
# Find the optimal number of variables selected at each split
# Find the number of trees where the out of bag error rate stabilizes and reach minimum.
# Select mtry value with minimum out of bag(OOB) error

mtry<-tuneRF(cross.sell.dev.gbm2[-4],cross.sell.dev.gbm2$segment,ntreeTry = 500,stepFactor = 1.5,improve = 0.01,trace = TRUE
             ,plot = TRUE)
best.m<-mtry[mtry[,2]==min(mtry[,2]),1]
print(mtry)
print(best.m)

#Mtry - Auto Select

classifier<-randomForest(x=cross.sell.dev.gbm2[-4],
                         y=cross.sell.dev.gbm2$segment,
                         importance=TRUE,
                         mtry = best.m,
                         ntree=500)

# Hardcoded Mtry Value - Based on minimum OOB error value

classifier<-randomForest(x=cross.sell.dev.gbm2[-4],
                         y=cross.sell.dev.gbm2$segment,
                         importance=TRUE,
                         mtry=4,
                         ntree=500)

#*****Alternative Memory efficient approach******
gc()
install.packages('ranger')
library(ranger)

colnames(cross.sell.dev.gbm2)
rf<- ranger(segment)
rf<-ranger(segment~., data=cross.sell.dev.gbm2, write.forest=TRUE)
predicted<-predict(rf,data=cross.sell.dev.gbm2)
table(cross.sell.dev.gbm2$segment, predictions(predicted))

#******************************************************************

summary(classifier)
print(classifier)

# Calculate the Variable Importance Plot
importance(classifier,type = 1)
varImpPlot(classifier)
varImpPlot(classifier,sort=TRUE,main="Variable Importance",n.var=10)
var.imp <- data.frame(importance(classifier,type=2))
var.imp$Variables <- row.names(var.imp)
var.imp
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])


# Plot partial dependence of each predictor
par(mfrow = c(3, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Dev_Sample) - 1))
{
  partialPlot(classifier, cross.sell.dev.gbm2, names(cross.sell.dev.gbm2)[i], xlab = names(cross.sell.dev.gbm2)[i],
              main = NULL);
}

# Calculate Predicted Probability
pred1<-predict(classifier,type="prob")

# You can use the same code to validate a dataset (test)
pred1<-predict(classifier,type="prob",Dev_Sample)
asd <- cbind(Dev_Sample,pred1)
View(head(asd))


#********************Comparing The Results - Validation Sample************************/

y_pred<-predict(classifier,newdata=cross.sell.val.gbm2[-4])

#Create the Confusion Matrix
cm<-table(cross.sell.val.gbm2[,4],y_pred)
cm

#******************************************************************************

# Choosing the number of trees
plot(classifier)

#Evaluate the performance of the random forest for classification.
pred2=predict(classifier,type = "prob")

#prediction is ROCR function
# perf = prediction(pred2[,4], cross.sell.val.gbm2$segment)

#performance in terms of true and false positive rates

# 1. Area under curve
# auc = performance(perf, "auc")
# auc


# True Positive and Negative Rate
# pred3 = performance(perf, "tpr","fpr")
# pred3

# 2. Plot the ROC curve
# plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
# abline(a=0,b=1,lwd=2,lty=2,col="gray")

#Plot a sample tree
# cforest(Segment2~., data=data, controls=cforest_control(mtry=best.m, mincriterion=0))
# version


##############################################################################################
##############################################################################################
##############################################################################################

#conditional Trees

library(party)
set.seed(1345)
ctree_model <- ctree(segment ~ ., 
                    data = cross.sell.dev.gbm2, 
                    controls =ctree_control(minsplit = 500,  mincriterion = .99 ,maxdepth = 5,
                                            minbucket = 1000))
ctree_model
plot(ctree_model)

#Prediction and confusion matrix
pred_ctree_model=predict(ctree_model,cross.sell.val.gbm2)
table(pred_ctree_model,cross.sell.val.gbm2$segment)


##############################################################################################
##############################################################################################
##############################################################################################

#Decision Tree C4.5
#code does not run in HVD due to access problem of java.dll
# C4.5 decision tree
# load the package
library(RWeka)
# fit model
c45model <- J48(segment~., data=cross.sell.dev.gbm2)
# summarize the fit
summary(c45model)
# make predictions
predictions <- predict(c45model, cross.sell.dev.gbm2[,-4])
# summarize accuracy
table(predictions, cross.sell.dev.gbm2$segment)



##############################################################################################
##############################################################################################
##############################################################################################

#CHAID Decision Tree
# The package for CHAID is not available in CRAN and the ncessary file can be 
# downloaded from the link https://r-forge.r-project.org/R/?group_id=343 either
# in zip format or .tar.gz format and then has to be sideloaded in R.

library(CHAID)
library(help=CHAID)

Dev_Sample_chaid = subset(train, select = -c(1,11))
# View(head(Dev_Sample))

Val_Sample_chaid = subset(test, select = -c(1,11))
# View(head(Val_Sample))


# Choosing only categorical variables for representation purpose 
# since chaid package only takes categorical independent variables
# For practical projects, create buckets for continuous variables 
# and turn continuous variables into categorical ones.

Dev_Sample_chaid <- Dev_Sample_chaid[,c(1,2,3,4,8,9,18,20,31,69)]
Val_Sample_chaid <- Val_Sample_chaid[,c(1,2,3,4,8,9,18,20,31,69)]  

Dev_Sample_chaid<-data.frame(lapply(Dev_Sample_chaid,function(x) as.factor(as.character(x))))
# View(head(Dev_Sample_chaid))
# str(Dev_Sample_chaid)

Val_Sample_chaid<-data.frame(lapply(Val_Sample_chaid,function(x) as.factor(as.character(x))))
# View(head(Val_Sample_chaid))
# str(Val_Sample_chaid)

Dev_Sample_chaid2 <- Dev_Sample_chaid[colSums(is.na(Dev_Sample_chaid))/nrow(Dev_Sample_chaid) == 0]
Val_Sample_chaid2 <- Val_Sample_chaid[colSums(is.na(Dev_Sample_chaid))/nrow(Dev_Sample_chaid) == 0]

# str(Dev_Sample_chaid2)
# str(Val_Sample_chaid2)

#CHAID model development

dt.chaid  <- chaid(target_var ~ .,
                   control = chaid_control(minprob = 0.1,minsplit = 2800,
                   minbucket = 1200), data= Dev_Sample_chaid2)

#plot decision tree
plot(dt.chaid, 
     uniform = T, 
     compress = T, 
     margin = 0.2, 
     branch = 0.3)
# Label on Decision Tree
text(dt.chaid, 
     use.n = T, 
     digits = 3, 
     cex = 0.6)
summary(dt.chaid)


