setwd("\\\\dmn1.fmr.com\\indfile\\INDFS353\\Common\\BusinessAnalytics\\FI\\Inbound\\Shuvayon\\Training\\Logistic Training")
data = read.csv("Dataset for regression.csv",header=T)
data2 = data[,-c(6,7,8,9,10,11,17,23,25,28,29,30,31)]

library(lmtest) # required for durbin watson
library(corrplot)# optional
library(ggplot2) #required#
library(ISLR) #required for Decision trees
library(MASS)# required#
library(faraway) # required for calculating vif
library(gvlma)

# replacing missing values with stat of the relevant column under threshold and removes the columns
# that have missing beyond the threshold
mis_impt<-function(x,t,stat){
  drop_var<-as.vector(0)
  for (i in 1:ncol(x)){
    nmiss<-(nrow(x)-sum(complete.cases(x[,i])))/nrow(x)
    print(paste(names(x)[i],'has',nmiss*100,'% missing'))
    if(nmiss<t) {for (j in 1:nrow(x))
    {if(is.na(x[j,i])){x[j,i]<-stat(x[,i],na.rm=T)} }}else {drop_var<-c(drop_var,i)}}
  drop_var
  x<-x[,-1*(drop_var)]
}
data2_t<-mis_impt(data2,0.001,median)

#correlation and exploratory data analysis
cha_var<-c()
for (i in 1:ncol(data2_t)){if(class(data2_t[,i])=="factor"|class(data2_t[,i])=="character"){cha_var<-c(cha_var,i)}}
cor_dat<-cor(data2_t[,-1*cha_var])
write.csv(cor_dat,'cor.csv')


### Checking linearity #####

## Check the outcome variable
library(lattice)
densityplot(data2$Net_Sales_12.Months)
#the graph has two peaks.. there must be some reason...

ggplot(data=data2_t,aes(as.factor(Response),Net_Sales_12.Months))+geom_boxplot()
ggplot(data=data2_t,aes(as.factor(Num_Active_Loan),Net_Sales_12.Months))+geom_boxplot()
ggplot(data=data2_t,aes(data2_t$Bank_SA_Sales_3.Months,Net_Sales_12.Months))+geom_point(aes(col=Num_Active_Loan))

#this tells us that folks living in Central have higher average sales than North.
#this tells us that folks who respond have higher on average sales.


##
set.seed(15)
rs<-sample(1:nrow(data2_t),800)
data_tr<-data2_t[rs,]
data_tv<-data2_t[-rs,]

r1<-lm(data=data_tr,Net_Sales_12.Months~.-Acct_Number)
sm_r1 = summary(r1)
plot(r1)

pred_r1_tr<-predict(r1,newdata=data_tr)
ggplot(data=data_tr,aes(Net_Sales_12.Months,pred_r1_tr))+geom_point(aes(col=as.factor(Num_Active_Loan)))+geom_line(aes(Net_Sales_12.Months,Net_Sales_12.Months))+geom_smooth()

pred_r1_tv<-predict(r1,newdata=data_tv)
data_tv<-data.frame(data_tv,pred_r1_tv)
ggplot(data=data_tv,aes(Net_Sales_12.Months,pred_r1_tv))+geom_point(aes(col=as.factor(Num_Active_Loan)))+geom_line(aes(Net_Sales_12.Months,Net_Sales_12.Months))+geom_smooth()
#No of of loans >=4 deviating a lot



###########################################
### Polynomial Reg################

attach(data2_t)
r1_p1<-update(r1,.~.-Bank_SA_Sales_3.Months +poly(Bank_SA_Sales_3.Months,degree=4,raw=T)-Current_Balance+poly(Current_Balance,4,raw=TRUE)-CC_Transaction+poly(CC_Transaction,4,raw=T)-Credit.Limit+poly(Credit.Limit,4,raw=T))
pred_r1_p1<-(predict(r1_p1,newdata=data_tv))
plot(pred_r1_p1)
data_tv<-data.frame(data_tv,pred_r1_p1)
ggplot(data=data_tv,aes(Net_Sales_12.Months,pred_r1_p1))+geom_point()+geom_line(aes(Net_Sales_12.Months,Net_Sales_12.Months))
sse_r1_p1<-mean((data_tv$Net_Sales_12.Months-data_tv$pred_r1_p1)^2)
sse_r1<-mean((data_tv$Net_Sales_12.Months-data_tv$pred_r1_tv)^2)
mean(resid(r1)^2)
mean(resid(r1_p1)^2)
