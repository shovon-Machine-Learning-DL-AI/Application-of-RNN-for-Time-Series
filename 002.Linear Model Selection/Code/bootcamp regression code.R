library(lmtest) # required for durbin watson
library(corrplot)# optional
library(ggplot2) #required#
library(ISLR) #required for Decision trees
library(MASS)# required#
library(faraway) # required for calculating vif
library(gvlma)
library(lattice)

data = read.csv(file.choose(),sep=",") # data used 'Dataset for regression.csv'
data2 = data[,-c(6,7,8,9,10,11,17,23,25,28,29,30,31)]
# replacing missing values with stat of the relevant column under threshold and removes the columns
# that have missing beyond the threshold
#we have used median for missing value imputation..u can use other approaches as discussed in previous sessions.
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


### Checking linearity #####
## Check the outcome variable
densityplot(data2$Net_Sales_12.Months)
#the graph has two peaks.. there must be some reason...

ggplot(data=data2_t,aes(as.factor(Response),Net_Sales_12.Months))+geom_boxplot()
ggplot(data=data2_t,aes(as.factor(Num_Active_Loan),Net_Sales_12.Months))+geom_boxplot()
ggplot(data=data2_t,aes(data2_t$Bank_SA_Sales_3.Months,Net_Sales_12.Months))+geom_point(aes(col=Num_Active_Loan))

#this tells us that folks who respond have higher on average sales.
#this tells that accounts with no.of loans more than 3 has significantly different behaviour than others.


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


#addressing multicollinearity----------------------------

names(vif(r1)[vif(r1)>5])

r11<-update(r1,.~.-Current_Balance)
summary(r11)

r11<-update(r1,.~.-Credit.Limit-Current_Balance)
summary(r11)
plot(r11)

data_tr$Bal_rat<-(data_tr$Current_Balance/data_tr$Credit.Limit) #adding new variables since R-square dropped drastically
r11<-update(r1,.~.-Credit.Limit-Current_Balance+Bal_rat)
summary(r11)
names(vif(r11)[vif(r11)>5])

data_tv$Bal_rat<-(data_tv$Current_Balance/data_tv$Credit.Limit)
data2_t$Bal_rat<-(data2_t$Current_Balance/data2_t$Credit.Limit)


#checking auocorrelation----------------------------

# Test for Autocorrelated Errors
library(MASS)
library(car)
durbinWatsonTest(r1)
# p- value = 0.024.. hence autocorrelation is present


#checking heteroscedasticity----------------------------

# non-constant error variance test
ncvTest(r1)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(r1)
#p-value less that a significance level of 0.05, 
#therefore we can reject the null hypothesis that 
#the variance of the residuals is constant and infer that heteroscedasticity 
#is indeed present, thereby confirming our graphical inference.


#checking normality of residuals----------------------------
library(MASS)
sresid <- studres(r11) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
# assume that linear is still probably a good model, which variables should actually stay in the model?
# that brings us to the question of model selection..


# step wise selection
r0 = lm(data=data_tr,Net_Sales_12.Months~1)
forwards = step(r0,scope=list(lower=formula(r0),upper=formula(r11)), direction="forward")
formula(forwards)

r12 = lm(data = data_tr,formula(forwards))
sm_r12 = summary(r12)
plot(r12)

pred_r12_tr<-predict(r12,newdata=data_tr)
ggplot(data=data_tr,aes(Net_Sales_12.Months,pred_r12_tr))+geom_point(aes(col=as.factor(Num_Active_Loan)))+geom_line(aes(Net_Sales_12.Months,Net_Sales_12.Months))+geom_smooth()

pred_r12_tv<-predict(r12,newdata=data_tv)
ggplot(data=data_tv,aes(Net_Sales_12.Months,pred_r12_tv))+geom_point(aes(col=as.factor(Num_Active_Loan)))+geom_line(aes(Net_Sales_12.Months,Net_Sales_12.Months))+geom_smooth()
#No of of loans >=4 deviating a lot


#Checking multicollinearity, Auto-correlation and heteroscedasticity for the new model
names(vif(r12)[vif(r12)>5])
#no multicollinearity
durbinWatsonTest(r12)
# no autocorrelation
# non-constant error variance test
ncvTest(r12)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(r11)
#still heteroscedastic


# Tackling the issue of incorrect standard errors
#heteroskedasticity-consistent standard errors or 
#simply robust standard errors
library(lmtest)
library(sandwich)
coeftest(r12, vcov = vcovHC(r12, "HC1")) 
summary(r12)
#compare the standard errors


# Using weighted least squares to bring constant variance for residuals
data_tr$resi <- r12$residuals
varfunc.ols <- lm(log(resi^2) ~ log(Bank_SA_Sales_3.Months + Bal_rat 
        + Num_Active_Loan + Response + Num_Return_3.Months), data = data_tr)
data_tr$varfunc <- exp(varfunc.ols$fitted.values)
r12.gls <- lm(Net_Sales_12.Months ~ Bank_SA_Sales_3.Months + Bal_rat 
              + Num_Active_Loan + Response + Num_Return_3.Months, 
              weights = 1/sqrt(varfunc), data = data_tr)
summary(r12.gls)
plot(r12.gls)

#outlier treatment
cooksd <- cooks.distance(r12.gls)
outlier <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])  
# outlier row nos.

data2_t_1 = data2_t[-c(458,1066,917,882,1220,672,1063,1008,287,577,
                       1016,881,1047,199,534,837,1025,165,79,127,806,
                       1191,360,1165,152,678,123,1125,524,1158,
                       1119,460,1079,156),]


#creating copy of the same data again
set.seed(15)
rs<-sample(1:nrow(data2_t_1),800)
data_tr_1<-data2_t_1[rs,]
data_tv_1<-data2_t_1[-rs,]

r13<-lm(data=data_tr_1,Net_Sales_12.Months~.-Acct_Number-Current_Balance
            -Credit.Limit-transformed_y)
sm_r13 = summary(r13)
plot(r13)

# Using weighted least squares to bring constant variance for residuals on newdata after outlier removal (Theory in Deck)
data_tr_1$resi <- r13$residuals
varfunc.ols <- lm(log(resi^2) ~ log(Bank_SA_Sales_3.Months + Bal_rat 
    + Num_Active_Loan + Response + Num_Return_3.Months), data = data_tr_1)
data_tr_1$varfunc <- exp(varfunc.ols$fitted.values)
r13.gls <- lm(Net_Sales_12.Months ~ Bank_SA_Sales_3.Months + Bal_rat 
              + Num_Active_Loan + Response + Num_Return_3.Months, 
              weights = 1/sqrt(varfunc), data = data_tr_1)
summary(r13.gls)
plot(r13.gls)

pred_r13.gls_tr<-predict(r13.gls,newdata=data_tr_1)
ggplot(data=data_tr_1,aes(Net_Sales_12.Months,pred_r13.gls_tr))+geom_point(aes(col=as.factor(Num_Active_Loan)))+geom_line(aes(Net_Sales_12.Months,Net_Sales_12.Months))+geom_smooth()

pred_r13.gls_tv<-predict(r13.gls,newdata=data_tv_1)
ggplot(data=data_tv_1,aes(Net_Sales_12.Months,pred_r13.gls_tv))+geom_point(aes(col=as.factor(Num_Active_Loan)))+geom_line(aes(Net_Sales_12.Months,Net_Sales_12.Months))+geom_smooth()

plot(data_tr_1$Net_Sales_12.Months,pred_r13.gls_tr)
plot(data_tv_1$Net_Sales_12.Months,pred_r13.gls_tv)

