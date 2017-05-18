/**********************Application of LSTM for time Series data*****************/
  setwd("C:\\Users\\a589565\\Desktop\\R-Files\\Data")
getwd()
install.packages("neuralnet")
require(neuralnet)
library(neuralnet)
version
install.packages(c("caret","e1071","MASS","fpp"))
install.packages("mlbench")
install.packages("lattice")
install.packages("ggplot2")
library(caret)
library(e1071)
library(MASS)
library(fpp)

# install.packages("installr")
# installr::updateR()
# version
# install.packages("installr")
# require(installr)
# updateR()
# 
/*******************Creating Time Series Model with DL****************/
  #In order to check the initial command history
  history(Inf)
#Time Series decomposition
data("nottem",package="datasets")
class(nottem)
y<-nottem
View(y)
#additive decomposition
y_deca<-decompose(y, type="additive")
plot(y_deca)
#Multiplicative Decomposition
y_decm<-decompose(y,type="multiplicative")
plot(y_decm)

#Application of simple Feed forward Neural Netwok
#Forecast the number of new cases of e.coli
#Try out once the STL method
View(elecequip)
fix(elecequip)
fit<-stl(elecequip,t.window=15,s.window="periodic",robust = TRUE)
eeadj<-seasadj(fit)
plot(naive(eeadj),ylab="new order index",
     main="naive fct of seasonally adjusted data")
fcast<-forecast(fit,method="naive",h=20)
View(fcast)
plot(fcast,ylab="New Order Index")
#Original Data
fcast1<-forecast(fit,method="naive")
View(fcast1)
plot(fcast1,ylab="New Order Index")

/**********************Working with Feed Forward Neural Network Model************/
  #Use the data from tscount Packg
  install.packages("prophet")
install.packages("ltsa")
# install.packages("/var/folders/ml/dzms2w3s1dqbp89khkz6p3qh0000gn/T//Rtmp3BGHc9/downloaded_packages/tscount_1.0.0.tar.gz", repos = NULL, type="source")
install.packages("C:\\Users\\a589565\\Documents\\R\\R-3.3.2\\library\\tscount_1.3.0.tar.gz", repos = NULL, type="source")
# install.packages("tscount")
#Ater downloading the package from archive, one should paste the file (tscount_1.0.0.tar.gz) in the secified location ""/var/folders/ml/dzms2w3s1dqbp89khkz6p3qh0000gn/T/Rtmp3BGHc9/downloaded_packages/tscount_1.0.0.tar.gz") . Post that from terminal should run the command - R CMD INSTALL "/var/folders/ml/dzms2w3s1dqbp89khkz6p3qh0000gn/T/Rtmp3BGHc9/downloaded_packages/tscount_1.0.0.tar.gz" -- It will automatically show dependency pckg "ltsa". Then install that using 
#install.packages() command
library(tscount)
library(ltsa)
data("ecoli", package="tscount")
#ecoli1<-ecoli
head(ecoli)
tail(ecoli)
class(ecoli)
#Explore and prepare the data
class(ecoli)
class(ecoli$cases)
typeof(ecoli$cases)  
#Need to convert this into Time Series object
data<-as.numeric(unlist(ecoli[3]))
data<-ts(matrix(data),
         start=c(2001,1),
         end=c(2013,20),
         frequency = 52)
class(data)
#Visualizing the Data
plot(data,
     xlab="Date",
     ylab="Number of Cases",
     col="darkgreen")
#Check for the Partial Autocorrelation
#Need to determine how many past obs to include in our model- So must check PACF
#PAC is the amount of correlation between an obs X(t) and lag of itself X(t-k) which is not
#explained by corr between obs in between. If x(t) is measured say at time t, then the PAC between 
#x(t) and x(t-3) is the corr that is not explained by their commo correlation with say x(t-1) and x(t-2)
#
pacf(data)
#Initial past 4 values of the PACF looks to have a more impact so should be included as intial guess
#We use quantmod pckg for lagged values calculation
#
# install.packages("quantmod")
library(xts)
library(TTR)
library(quantmod)
#It requires the data to be in zoo format
#install.packages("zoo")
require(zoo)
library(zoo)
data<-as.zoo(data)
class(data)
#We can now apply quantmod
require(quantmod)
x1<- Lag(data, k=1)
x2<- Lag(data, k=2)
x3<- Lag(data, k=3)
x4<- Lag(data, k=4)

#cbind the obs

x<- cbind(x1,x2,x3,x4,data)
View(x)
names(x$Lag.1)<- "x1"
names(x$Lag.2)<- "x2"
names(x$Lag.3)<- "x3"
names(x$Lag.4)<- "x4"
# type(x)
colnames(x)
#Remove the "NA"
x<-x[-(1:4),]
head(x)
#Normalisation : Create a custom scale function
#Create a User defined function
range_data<- function(x){(x-min(x))/(max(x)-min(x))}
x<-data.matrix(x) #To pass the data in FFN we need to convert the data in a matrix
min_data<-min(x)# required later to unscale the data
max_data<-max(x)# required later to unscale the data
x<-range_data(x)
summary(x[,1:5])
# install.packages("dplyr")
# install.packages("psych")
library(dplyr)
library(psych)
describe(x)

#Create the training and Test Data set
nrow(x)
#Create both X and Y
y <- as.numeric(x[,5])
head(y)
nrow(y)
class(y)
typeof(y)
x<-x[,-5]
nrow(x)
n_train<-600
x_train<- x[(1:n_train), ]
y_train<- y[(1:n_train)]
x_test<- x[(n_train+1):nrow(x), ]
y_test<- y[n_train+1:nrow(x)]

nrow(x_test)
head(y_test)
head(x_test)
nrow(y_test)

nrow(x_train)

version
/******************Training the model on the Data*********************/
  # We build a two hidden layer deep neural network. It contains 4 input nodes, one for each of the lagged variables; 10 nodes in the first hidden layer, 2 nodes in the second hidden layer; and 1 output node which is used to report the model forecast.
  
  #Install the MxNet pckg
  install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")
library(mlbench)
library(drat)
library(mxnet)

# install.packages("mlbench")
require(mxnet)
help(mxnet)
mx.set.seed(2018) #used for initialisation of weights/reproducability
model<-mx.mlp(x_train,y_train,
              hidden_node=c(10,2),
              out_node=1,
              activation="sigmoid",
              out_activation="rmse",
              num.round=100,#max number of iterations
              array.batch.size=20,#batch size
              learning.rate=0.07,
              momentum=0.9,#In order to avoid getting into a local minima trap
              device=mx.cpu())#You can use either CPU or GPU

summary(model)

#View(model[,1])
#Evaluate the model performance
pred1_train<-predict(model,x_train,ctx=mx.cpu())
#the first 8 values are shown here
pred1_train[,1:8]
# View(pred1_train)
# Performances on the Training Dataset
install.packages("Metrics")
library(Metrics)
round(rmse(y_train,pred1_train[,1:600]),5)

#Improving te Model performance
#Build a single layer model
model1<-mx.mlp(x_train,y_train,
               hidden_node=c(10),
               out_node=1,
               activation="sigmoid",
               out_activation="rmse",
               num.round=100,#max number of iterations
               array.batch.size=20,#batch size
               learning.rate=0.05,
               momentum=0.8,#In order to avoid getting into a local minima trap
               device=mx.cpu())#You can use either CPU or GPU
summary(model1)
# Evaluate the model performance
pred2_train<-predict(model1,x_train,ctx=mx.cpu())
#the first 8 values are shown here
pred1_train[,1:8]
# View(pred1_train)
# Performances on the Training Dataset
# install.packages("Metrics")
library(Metrics)
round(rmse(y_train,pred2_train[,1:600]),5)

/****************USe a different activation function********************/
  #Build a single layer model
  model2<-mx.mlp(x_train,y_train,
                 hidden_node=c(10),
                 out_node=1,
                 activation="tanh",
                 out_activation="rmse",
                 num.round=100,#max number of iterations
                 array.batch.size=20,#batch size
                 learning.rate=0.05,
                 momentum=0.8,#In order to avoid getting into a local minima trap
                 device=mx.cpu())#You can use either CPU or GPU
summary(model2)
# Evaluate the model performance
pred3_train<-predict(model2,x_train,ctx=mx.cpu())
#the first 8 values are shown here
pred3_train[,1:8]
# View(pred1_train)
# Performances on the Training Dataset
# install.packages("Metrics")
library(Metrics)
round(rmse(y_train,pred3_train[,1:600]),5)

#Look into the test set performnce
#
## Evaluate the model performance
pred3_test<-predict(model2,x_test,ctx=mx.cpu())
head(y_test,n=40)
#the first 8 values are shown here
pred3_test[,1:40]
# View(pred1_train)
# Performances on the Training Dataset
# install.packages("Metrics")
library(Metrics)
round(rmse(y_test,pred3_test[,1:40]),5)

#Unscaling the Data
unscale_data<- function(x,max_x,min_x){x*(max_x-min_x)+min_x}
y_actual<- unscale_data(y_test,max_data,min_data)
View(y_actual[1:40])
#COnvert the data to a Time Series
y_actual<- ts(matrix(y_actual),
              end=c(2013,20),
              frequency = 52)
View(y_actual)
y_actual_n<-ts(matrix(y_actual[1:40]),end=c(2013,20),frequency = 52)
View(y_actual_n)
#Comparison of Actual data ad unscaled data
View(data[605:644])
orig_test_data<- ts(matrix(data[605:644]),end=c(2013,20),frequency = 52)
View(orig_test_data)
#Compare
checkit<- cbind(y_actual_n,orig_test_data)
View(checkit)
head(checkit)
#Check if the cols are identical
identical(checkit['y_actual_n'],checkit['orig_test_data'])

#Unscale the Test set prediction
pred_actual<-unscale_data(pred3_test,max_data,min_data)
pred_actual<- ts(matrix(pred_actual),end = c(2013,20),frequency = 52)
View(pred_actual)
tot<-cbind(y_actual_n,pred_actual)
View(tot)
#Plot the result
ts.plot(y_actual_n,pred_actual,
        gpars = list(xlab="Year",
                     ylab="Cases",
                     lty=c(1:2),
                     col=c("blue","black")))

/***********************Application of RNN for Time Series************/
  version
install.packages("rnn")
library(rnn)
#Determine the whole bird sport price of the Chicken
install.packages("astsa")
library(astsa)
data('chicken',package = "astsa")
View(head(chicken))
class(chicken)#ts object
plot(chicken,
     xlab='date',
     ylab='price',
     col='darkblue')
#Exploring and preparing the data
data<-chicken
require(quantmod)
data<-as.zoo(data)
x1<-Lag(data,k=1)
x2<-Lag(data,k=2)
x3<-Lag(data,k=3)
x4<-Lag(data,k=4)
# Create the Overall data structure
x<-cbind(x1,x2,x3,x4,data)
class(x)
View(x)
# Convert the data with Log transforation
x<-log(x)
View(head(x))
# Remove the missing values
x<-x[-(1:4),]
View(head(x))
# Preparing the data for input of the RNN
x<-data.matrix(x)
range_data<-function(x){(x-min(x))/(max(x)-min(x))}
min_data<-min(x)
max_data<-max(x)
x<-range_data(x)
View(head(x))
max(x)
min(x)
# Creation of Test and Train Samples
# Lets make sure that the data are in proper matrix format
x1<-as.matrix(x[,1])
x2<-as.matrix(x[,2])
x3<-as.matrix(x[,3])
x4<-as.matrix(x[,4])
y<-as.matrix(x[,5])

# The model will predict last 6 weeks/months of price
n_train<-170
y_train<-as.matrix(y[1:n_train])
x1_train<-as.matrix(t(x1[1:n_train,]))#feature data are transposed to meet the requirement for RNN
x2_train<-as.matrix(t(x2[1:n_train,]))
x3_train<-as.matrix(t(x3[1:n_train,]))
x4_train<-as.matrix(t(x4[1:n_train,]))
x1_train
y_train
nrow(x)
ncol(x1_train)
ncol(x2_train)
ncol(x3_train)
ncol(x4_train)

#Test Dataset Creation
y_test<-as.matrix(y[(n_train+1):nrow(x4)])#All the features should be transposed but not the target var
nrow(y_test)
x1_test<-as.matrix(t(x1[(n_train+1):nrow(x1),]))#feature data are transposed to meet the requirement for RNN
x2_test<-as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test<-as.matrix(t(x3[(n_train+1):nrow(x3),]))
x4_test<-as.matrix(t(x4[(n_train+1):nrow(x4),]))

ncol(x1_test)
ncol(x2_test)
ncol(x3_test)
ncol(x4_test)
# dim(x_train)
#Training a model on the data
x_train<-array(c(x1_train,x2_train,x3_train,x4_train),dim=c(dim(x1_train),4))
x_test<-array(c(x1_test,x2_test,x3_test,x4_test),dim=c(dim(x1_test),4))
dim(x_train)
dim(x_test)
#Specify the Model
require(rnn)
set.seed(2018)
model1<-trainr(Y=t(y_train),
               X=x_train,
               learningrate = 0.05,
               hidden_dim = 3,
               numepochs = 500,
               network_type = 'rnn',
               sigmoid = "logistic")
#Examining Error by Epoch
error_1<-t(model1$error)
rownames(error_1)<-1:nrow(error_1)
colnames(error_1)<-"error"
plot(error_1)

#Evaluating the Model Performance

pred1_train<-t(predictr(model1,x_train))
pred1_train
#We use correlation coeffs to judge how closely the predictions are
round(cor(y_train,pred1_train),5)
plot(y_train,pred1_train,ylab="pred1_train")

#Improving the Model Performance
set.seed(2018)
model2<-trainr(Y=t(y_train),
               X=x_train,
               learningrate = 0.05,
               hidden_dim = c(3,2),
               numepochs = 500,
               network_type = "rnn",
               sigmoid = "logistic")
pred2_train<-t(predictr(model2,x_train))
round(cor(y_train,pred2_train),5)

#Test Dataset Creation
y_test<-as.matrix(y[(n_train+1):nrow(x4)])#All the features should be transposed but not the target var
nrow(y_test)
x1_test<-as.matrix(t(x1[(n_train+1):nrow(x1),]))#feature data are transposed to meet the requirement for RNN
x2_test<-as.matrix(t(x2[(n_train+1):nrow(x2),]))
x3_test<-as.matrix(t(x3[(n_train+1):nrow(x3),]))
x4_test<-as.matrix(t(x4[(n_train+1):nrow(x4),]))

ncol(x1_test)
ncol(x2_test)
ncol(x3_test)
ncol(x4_test)
# dim(x_train)
#Training a model on the data
# x_train<-array(c(x1_train,x2_train,x3_train,x4_train),dim=c(dim(x1_train),4))
x_test<-array(c(x1_test,x2_test,x3_test,x4_test),dim=c(dim(x1_test),4))
dim(x_train)
dim(x_test)

/**********************Creating the Final Prediction**************/
  pred1_test<-t(predictr(model1,x_test))
pred2_test<-t(predictr(model2,x_test))

#Unscaling the data
max_data<-max(data)
min_data<-min(data)
unscale_data<-function(x,max_x,min_x){x*(max_x-min_x)+min_x}
pred1_actual<-unscale_data(pred1_test,max_data,min_data)
pred1_actual<-exp(pred1_actual)
pred1_actual<-ts(matrix(pred1_actual),
                 end=c(2016,7),
                 frequency = 12)
pred1_actual
View(head(as.data.frame(pred1_actual)))

pred2_actual<-unscale_data(pred2_test,max_data,min_data)
pred2_actual<-exp(pred2_actual)
pred2_actual<-ts(matrix(pred2_actual),
                 end=c(2016,7),
                 frequency = 12)
pred2_actual
View(head(as.data.frame(pred2_actual)))
View(head(as.data.frame(pred1_actual)))

y_actual<-unscale_data(y_test,max_data,min_data)
y_actual<-exp(y_actual)
y_actual<- ts(matrix(y_actual),
              end=c(2016,7),
              frequency = 12)
result_all<-cbind(y_actual,round(pred1_actual,2),round(pred2_actual,2))
colnames(result_all)<-c("actual","model1","model2")
result_all
/*******************Re-estimating model for each new observations******************/
  # Often the performance of the neural network can be improved by optimally choosing the weights as the new observations become available
  #Create 6- 1 month ahead forecast
  start<-n_train
end=6
k=-1
start

for (i in start:((start+end)-1)){
  k=k+1
  cat("Working forecast for month",
      n_train+k,
      "\n")
}
#Prepare the training data
# Data needs to transformed into appropriate format()
y_train_step<-as.matrix(y[1:(n_train+k)])
x1_train_step<-as.matrix(t(x1[1:(n_train+k),]))
x2_train_step<-as.matrix(t(x2[1:(n_train+k),]))
x3_train_step<-as.matrix(t(x3[1:(n_train+k),]))
x4_train_step<-as.matrix(t(x4[1:(n_train+k),]))

x_train_step<-array(c(x1_train_step,
                      x2_train_step,
                      x3_train_step,
                      x4_train_step),
                    dim=c(dim(x1_train_step),4))
dim(x_train_step)
#Specify the Model
# We train a model with 2 nodes in a single hidden layer
set.seed(2018)
model_step<-trainr(Y=t(y_train_step),
                   X=x_train_step,
                   learningrate = 0.05,
                   hidden_dim = 2,
                   numepochs = 500,
                   network_type = "rnn",
                   sigmoid = "logistic")
#Prepare the Test Data 
x1_test_step<-as.matrix(t(x1[(n_train+k+1),]))
x2_test_step<-as.matrix(t(x2[(n_train+k+1),]))
x3_test_step<-as.matrix(t(x3[(n_train+k+1),]))
x4_test_step<-as.matrix(t(x4[(n_train+k+1),]))
y_test_step<-as.matrix(y[(n_train+k+1)])

# Final Data set
x_test_step<-array(c(x1_test_step,
                     x2_test_step,
                     x3_test_step,
                     x4_test_step),
                   dim=c(dim(x1_test_step),4))
dim(x_test_step)
#Make Prediction and close the for loops
pred_test<-(t(predictr(model_step,x_test_step)))
# pred_test
for (i in start:((start+end)-1)){
  k=k+1
  cat("Working forecast for month",
      n_train+k,
      "\n")
  
  if(i==start) forecast <-as.numeric(pred_test)
  if(i>start)  forecast <-cbind(forecast, as.numeric(pred_test))
}
forecast
#Unscale the predictions
pred_actual<-unscale_data(forecast,max_data,min_data)
pred_actual<-exp(pred_actual)
pred_actual<-ts(matrix(pred_actual),
                end=c(2016,7),
                frequency = 12)
pred_actual
#Combining with previous results
results_all<-cbind(result_all,
                   round(pred_actual,2))
colnames(results_all)<-c("actual","Model-1","Model-2","One Step")
results_all
class(results_all)

/**************Final Revised Loop Model for the RNN******************/
  start<-n_train
end=6
k=-1
start

for (i in start:((start+end)-1)){
  k=k+1
  cat("Working forecast for month",
      n_train+k,
      "\n")
  
  # Data needs to transformed into appropriate format()
  y_train_step<-as.matrix(y[1:(n_train+k)])
  x1_train_step<-as.matrix(t(x1[1:(n_train+k),]))
  x2_train_step<-as.matrix(t(x2[1:(n_train+k),]))
  x3_train_step<-as.matrix(t(x3[1:(n_train+k),]))
  x4_train_step<-as.matrix(t(x4[1:(n_train+k),]))
  
  x_train_step<-array(c(x1_train_step,
                        x2_train_step,
                        x3_train_step,
                        x4_train_step),
                      dim=c(dim(x1_train_step),4))
  dim(x_train_step)
  #Specify the Model
  # We train a model with 2 nodes in a single hidden layer
  set.seed(2018)
  model_step<-trainr(Y=t(y_train_step),
                     X=x_train_step,
                     learningrate = 0.05,
                     hidden_dim = 2,
                     numepochs = 500,
                     network_type = "rnn",
                     sigmoid = "logistic")
  #Prepare the Test Data 
  x1_test_step<-as.matrix(t(x1[(n_train+k+1),]))
  x2_test_step<-as.matrix(t(x2[(n_train+k+1),]))
  x3_test_step<-as.matrix(t(x3[(n_train+k+1),]))
  x4_test_step<-as.matrix(t(x4[(n_train+k+1),]))
  y_test_step<-as.matrix(y[(n_train+k+1)])
  
  # Final Data set
  x_test_step<-array(c(x1_test_step,
                       x2_test_step,
                       x3_test_step,
                       x4_test_step),
                     dim=c(dim(x1_test_step),4))
  dim(x_test_step)
  #Make Prediction and close the for loops
  pred_test<-(t(predictr(model_step,x_test_step)))
  
  if(i==start) forecast <-as.numeric(pred_test)
  if(i>start)  forecast <-cbind(forecast, as.numeric(pred_test))
}


#Unscale the predictions
pred_actual<-unscale_data(forecast,max_data,min_data)
pred_actual<-exp(pred_actual)
pred_actual<-ts(matrix(pred_actual),
                end=c(2016,7),
                frequency = 12)
pred_actual
#Combining with previous results
options("scipen"=-100, "digits"=4)
#options(scipen = 4)
results_all<-cbind(result_all,
                   round(pred_actual,2))
colnames(results_all)<-c("actual","Model-1","Model-2","One Step")
results_all
class(results_all)
#Convert exponent into number
results_all <- sapply(results_all, as.numeric)
results_all
