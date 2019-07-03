setwd("C:\\Users\\a604283\\Desktop\\DataScience Bootcamp\\Hackathon")
#library(data.table)
library(tidyverse)
library(broom)
library(glmnet)
library(MASS)
library(car)
library(glmnet)

set.seed(1342)
DT <- fread("data_for_DSB_ridge.csv")
names(DT)
DT1 <- DT[1:120000,c(23:30,32:36)]
a <- as.data.frame(scale(DT1))
names(a)
plot(a$Claim_Amount)
claim_amount <- a$Claim_Amount
h<-hist(claim_amount, breaks=10, col="red", xlab="Income") 
summary(a$Claim_Amount)


####Linear Reg
lin_reg <- lm(a$Claim_Amount ~ ., data = a)
summary(lin_reg)
sort(vif(lin_reg),decreasing = T)
lin_fit <- predict(lin_reg)
lin_fit <- predict(lin_reg,data = x_test)
summary(lin_fit)
lin_MSE <- mean((lin_fit - a$Claim_Amount)^2)


####Ridge Reg
y <- a$Claim_Amount
x <- model.matrix(claim_amount~., a)[,-13]
lambda <- 10^seq(2, -3, by = -.1)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x, y, alpha = 0, lambda = lambda)
bestlam <- cv.out$lambda.min
#make predictions
ridge_pred <- predict(ridge.mod, s = bestlam, newx = x)
ridge_coeff <- predict(ridge.mod, s = bestlam, type = 'coefficients')
summary(ridge_pred)
summary(y)
#check MSE
ridge_MSE <- mean((ridge_pred-y)^2)


#####Lasso
lasso.mod <- glmnet(x, y, alpha = 1, lambda = lambda)
predict(lasso.mod, s = 0, exact = T, type = 'coefficients')
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x, y, alpha = 1, lambda = lambda)
bestlam <- cv.out$lambda.min
#make predictions
lasso_pred <- predict(lasso.mod, s = bestlam, newx = x)
lasso_coeff <- predict(lasso.mod, s = bestlam, type = 'coefficients')
summary(lasso_pred)
summary(y)
lasso_MSE <- mean((lasso_pred-y)^2)
