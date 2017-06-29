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
     col='darkred')
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
# x<-log(x)
View(head(x))
# Remove the missing values
x<-x[-(1:4),]
View(head(x))
# Preparing the data for input of the RNN
x<-data.matrix(x)
range_data<-function(x){(x-min(x))/(max(x)-min(x))} #Normalisation is an important step for RNN to perform better
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
# Epoch error: 7.0300296529884
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
# Epoch error: 6.60848912101128
# We can try out some other types of Activation Function
#One should also try to play around the various values of the hyper-parameters
model3<-trainr(Y=t(y_train),
               X=x_train,
               learningrate = 0.05,
               hidden_dim = c(3,2),
               numepochs = 600,
               network_type = 'rnn',
               sigmoid = "Gompertz")
# Epoch error: 9.11481970727849
pred3_train<-t(predictr(model3,x_train))
round(cor(y_train,pred3_train),5)

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
pred3_test<-t(predictr(model3,x_test))
#Unscaling the data
max_data<-max(data)
min_data<-min(data)
unscale_data<-function(x,max_x,min_x){x*(max_x-min_x)+min_x}
#Finding the Actual for Model-1
pred1_actual<-unscale_data(pred1_test,max_data,min_data)
# pred1_actual<-exp(pred1_actual)
pred1_actual<-ts(matrix(pred1_actual),
                 end=c(2016,7),
                 frequency = 12)
pred1_actual
View(head(as.data.frame(pred1_actual)))
#Finding Actual for the Model-2
pred2_actual<-unscale_data(pred2_test,max_data,min_data)
# pred2_actual<-exp(pred2_actual)
pred2_actual<-ts(matrix(pred2_actual),
                 end=c(2016,7),
                 frequency = 12)
pred2_actual
#Finding Actual for the Model-3
pred3_actual<-unscale_data(pred3_test,max_data,min_data)
# pred3_actual<-exp(pred3_actual)
pred3_actual<-ts(matrix(pred3_actual),
                 end=c(2016,7),
                 frequency = 12)
pred3_actual


View(head(as.data.frame(pred2_actual)))
View(head(as.data.frame(pred1_actual)))
View(head(as.data.frame(pred3_actual)))

#Finding the Actual for Y
y_actual<-unscale_data(y_test,max_data,min_data)
# y_actual<-exp(y_actual)
y_actual<- ts(matrix(y_actual),
              end=c(2016,7),
              frequency = 12)
result_all<-cbind(y_actual,round(pred1_actual,2),round(pred2_actual,2),round(pred3_actual,2))
colnames(result_all)<-c("actual","model1","model2","model3")
result_all
View(result_all)
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
                     hidden_dim = c(3,2),
                     numepochs = 600,
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
# pred_actual<-exp(pred_actual)
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