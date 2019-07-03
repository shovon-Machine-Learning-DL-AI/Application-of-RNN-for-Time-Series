# First we will simulate some data from a logistic regression model with one covariate x, 
# and then fit the correct logistic regression model. This means our model is correctly specified, 
# and we should hopefully not detect evidence of poor fit.

library(ResourceSelection)
set.seed(43657)
n <- 100
x <- rnorm(n)
xb <- x
pr <- exp(xb)/(1+exp(xb))
y <- 1*(runif(n) < pr)
data<-data.frame(y,xb,pr)
mod <- glm(y~x, family=binomial)
#Next we pass the outcome y and model fitted probabilities to the hoslem.test function, choosing g=10 groups:
hl <- hoslem.test(mod$y, fitted(mod), g=10)
hl

# Hosmer and Lemeshow goodness of fit (GOF) test
# 
# data:  mod$y, fitted(mod)
# X-squared = 7.4866, df = 8, p-value = 0.4851
# This gives p=0.49, indicating no evidence of poor fit.

# cbind(hl$observed,hl$expected)

# To help us understand the calculation, let's now perform the test ourselves manually. 
# First we calculate the model predicted probabilities, and then categorise the observations 
# according to deciles of the predicted probabilities:

pihat <- mod$fitted
pihatcat <- cut(pihat, breaks=c(0,quantile(pihat, probs=seq(0.1,0.9,0.1)),1), labels=FALSE)

# Next, we cycle through the groups 1 to 10, counting the number observed 0s and 1s, 
# and calculating the expected number of 0s and 1s. 
# To calculate the latter, we find the mean of the predicted probabilities in each group, 
# and multiply this by the group size, which here is 10:

meanprobs <- array(0, dim=c(10,2))
expevents <- array(0, dim=c(10,2))
obsevents <- array(0, dim=c(10,2))

for (i in 1:10) {
  meanprobs[i,1] <- mean(pihat[pihatcat==i])
  expevents[i,1] <- sum(pihatcat==i)*meanprobs[i,1]
  obsevents[i,1] <- sum(y[pihatcat==i])
  
  meanprobs[i,2] <- mean(1-pihat[pihatcat==i])
  expevents[i,2] <- sum(pihatcat==i)*meanprobs[i,2]
  obsevents[i,2] <- sum(1-y[pihatcat==i])
}

# data<-data.frame(y,xb,pr,p_hat=pihat,pi_cut=pihatcat)

# Lastly, we can calculate the Hosmer-Lemeshow test statistic by 
# the sum of (observed-expected)^2/expected across the 10x2 cells of the table

hosmerlemeshow <- sum((obsevents-expevents)^2 / expevents)
hosmerlemeshow
