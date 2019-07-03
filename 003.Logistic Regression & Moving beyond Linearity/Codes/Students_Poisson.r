#### R Code for Poisson Regression Model for Count Data



# In the following example we fit a generalized linear model to count data using a Poisson error structure.
# The data set consists of counts of high school students diagnosed with an infectious disease within 
# a period of days from an initial outbreak.
setwd("\\\\dmn1.fmr.com\\indfile\\INDFS353\\Common\\BusinessAnalytics\\FI\\Inbound\\Shuvayon\\Training\\Logistic Training")
cases <- read.csv("cases.csv",header=T) 
  
attach(cases)

head(cases) 
plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)
model1 <- glm(Students ~ Days, poisson)

summary(model1)
plot(model1)
# The negative coefficient for Days indicates that as days increase, 
# the mean number of students with the disease is smaller.
# 
# This coefficient is highly significant (p < 2e-16).
# 
# We also see that the residual deviance is greater than the degrees of freedom,
# so that we have over-dispersion. 
# This means that there is extra variance not accounted for by the model or 
# by the error structure.
# 
# This is a very important model assumption, so we will re-fit the model 
# using quasi poisson errors.

model2 <- glm(Students ~ Days, quasipoisson)
summary(model2)
plot(model2)

# The outcome of our attempt to account for over-dispersion is that the residual deviance has not changed.
# 
# The dispersion parameter, which was forced to be 1 in our last model, is allowed to be estimated here. In fact, it is estimated at .79.
# 
# This parameter tells us how many times larger the variance is than the mean. Since our dispersion was less than one, it turns out the conditional variance is actually smaller than the conditional mean. 
# We have under-dispersion, not over.

model2$coefficients 

timeaxis <-seq (0,150,0.1)
Y <- predict(model2, list(Days = timeaxis))

plot(Days, Students, xlab = "DAYS", ylab = "STUDENTS", pch = 16)
lines(timeaxis, exp(Y), lwd = 2, col = "blue")

coeffs <- exp(coef(model2))

