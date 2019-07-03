/***************Application of LDA and QDA************/
#Replication Requirements
#This tutorial primarily leverages the Default data provided by the ISLR package. This is a simulated data set containing information on ten thousand customers such as whether the customer defaulted, is a student, the average balance carried by the customer and the income of the customer. We'll also use a few packages that provide data manipulation, visualization, pipeline modeling functions, and model output tidying functions.
  
#The overall discussion can be found at : http://uc-r.github.io/discriminant_analysis
  
install.packages('tidyverse')
install.packages('MASS')
install.packages('ISLR')
install.packages('rlang')

library(MASS)
library(rlang)
library(tidyverse)
library(ISLR)
library(dplyr)

# Load data 
default <- as_tibble(ISLR::Default)
attach(Default)
head(Default)

set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(default), replace = T, prob = c(0.6,0.4))
train <- default[sample, ]
test <- default[!sample, ]

lda.m1 <- lda(default ~ balance + student, data = train)
summary(lda.m1)

lda.m1
(df <- tibble(balance = rep(c(1000, 2000), 2), 
               student = c("No", "No", "Yes", "Yes")))
(df.pred <- predict(lda.m1, df))

#help(tibble)
# number of non-defaulters
sum(df.pred$posterior[, 1] >= .5)
## [1] 3

# number of defaulters
sum(df.pred$posterior[, 2] > .5)
## [1] 1

# number of high-risk customers with 40% probability of defaulting
sum(df.pred$posterior[, 2] > .4)
## [1] 2


(qda.m1 <- qda(default ~ balance + student, data = train))
## Call:
## qda(default ~ balance + student, data = train)
## 
## Prior probabilities of groups:
##        No       Yes 
## 0.9677526 0.0322474 
## 
## Group means:
##      balance studentYes
## No   804.968  0.2956254
## Yes 1776.971  0.3948718

predict(qda.m1, df)

test.predicted.lda <- predict(lda.m1, newdata = test)
test.predicted.qda <- predict(qda.m1, newdata = test)

lda.cm <- table(test$default, test.predicted.lda$class)
qda.cm <- table(test$default, test.predicted.qda$class)

list(LDA_model = lda.cm %>% prop.table() %>% round(3),
     QDA_model = qda.cm %>% prop.table() %>% round(3))
## $LDA_model
##      
##          No   Yes
##   No  0.964 0.002
##   Yes 0.028 0.007
## 
## $QDA_model
##      
##          No   Yes
##   No  0.963 0.002
##   Yes 0.026 0.009

test %>%
  mutate(lda.pred = (test.predicted.lda$class),
         qda.pred = (test.predicted.qda$class)) %>%
  summarise(lda.error = mean(default != lda.pred),
            qda.error = mean(default != qda.pred))

list(LDA_model = lda.cm,
     QDA_model = qda.cm)


# create adjusted predictions
lda.pred.adj <- ifelse(test.predicted.lda$posterior[, 2] > .20, "Yes", "No")
qda.pred.adj <- ifelse(test.predicted.qda$posterior[, 2] > .20, "Yes", "No")

# create new confusion matrices
list(LDA_model = table(test$default, lda.pred.adj),
     QDA_model = table(test$default, qda.pred.adj))

# ROC curves
library(ROCR)

par(mfrow=c(1, 2))

prediction(test.predicted.lda$posterior[,2], test$default) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test.predicted.qda$posterior[,2], test$default) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()


# model 1 AUC
prediction(test.predicted.lda$posterior[,2], test$default) %>%
  performance(measure = "auc") %>%
  .@y.values
## [[1]]
## [1] 0.9420727

# model 2 AUC
prediction(test.predicted.qda$posterior[,2], test$default) %>%
  performance(measure = "auc") %>%
  .@y.values
## [[1]]
## [1] 0.9420746
plot(lda.m1)
# A comparison between LOgistic Regression and LDA/QDA
head(ISLR::Smarket)

train <- subset(ISLR::Smarket, Year < 2005)
test <- subset(ISLR::Smarket, Year == 2005)

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, 
               data = train,
               family = binomial)

summary(glm.fit)


# predictions
glm.probs <- predict(glm.fit, test, type="response")

# confusion matrix
table(test$Direction, ifelse(glm.probs > 0.5, "Up", "Down"))
##       
##        Down Up
##   Down   77 34
##   Up     97 44

# accuracy rate
mean(ifelse(glm.probs > 0.5, "Up", "Down") == test$Direction)
## [1] 0.4801587

# error rate
mean(ifelse(glm.probs > 0.5, "Up", "Down") != test$Direction)
## [1] 0.5198413


caret::varImp(glm.fit)


glm.fit <- glm(Direction ~ Lag1 + Lag2, 
               data = train,
               family = binomial)

summary(glm.fit)

# predictions
glm.probs <- predict(glm.fit, test, type = "response")

# confusion matrix
table(test$Direction, ifelse(glm.probs > 0.5, "Up", "Down"))
##       
##        Down  Up
##   Down   35  76
##   Up     35 106

# accuracy rate
mean(ifelse(glm.probs > 0.5, "Up", "Down") == test$Direction)
## [1] 0.5595238

# error rate
mean(ifelse(glm.probs > 0.5, "Up", "Down") != test$Direction)
## [1] 0.4404762


(lda.fit <- lda(Direction ~ Lag1 + Lag2, data = train))

# predictions
test.predicted.lda <- predict(lda.fit, newdata = test)

# confusion matrix
table(test$Direction, test.predicted.lda$class)
##       
##        Down  Up
##   Down   35  76
##   Up     35 106

# accuracy rate
mean(test.predicted.lda$class == test$Direction)
## [1] 0.5595238

# error rate
mean(test.predicted.lda$class != test$Direction)
## [1] 0.4404762

(qda.fit <- qda(Direction ~ Lag1 + Lag2, data = train))
## Call:
## qda(Direction ~ Lag1 + Lag2, data = train)
## 
## Prior probabilities of groups:
##     Down       Up 
## 0.491984 0.508016 
## 
## Group means:
##             Lag1        Lag2
## Down  0.04279022  0.03389409
## Up   -0.03954635 -0.03132544

# predictions
test.predicted.qda <- predict(qda.fit, newdata = test)

# confusion matrix
table(test$Direction, test.predicted.qda$class)
##       
##        Down  Up
##   Down   30  81
##   Up     20 121

# accuracy rate
mean(test.predicted.qda$class == test$Direction)
## [1] 0.5992063

# error rate
mean(test.predicted.qda$class != test$Direction)
## [1] 0.4007937


# ROC curves
library(ROCR)

p1 <- prediction(glm.probs, test$Direction) %>%
  performance(measure = "tpr", x.measure = "fpr")

p2 <- prediction(test.predicted.lda$posterior[,2], test$Direction) %>%
  performance(measure = "tpr", x.measure = "fpr")

p3 <- prediction(test.predicted.qda$posterior[,2], test$Direction) %>%
  performance(measure = "tpr", x.measure = "fpr")

plot(p1, col = "red")
plot(p2, add = TRUE, col = "blue")
plot(p3, add = TRUE, col = "green")


# Logistic regression AUC
prediction(glm.probs, test$Direction) %>%
  performance(measure = "auc") %>%
  .@y.values
## [[1]]
## [1] 0.5584308

# LDA AUC
prediction(test.predicted.lda$posterior[,2], test$Direction) %>%
  performance(measure = "auc") %>%
  .@y.values
## [[1]]
## [1] 0.5584308

# QDA AUC
prediction(test.predicted.qda$posterior[,2], test$Direction) %>%
  performance(measure = "auc") %>%
  .@y.values
## [[1]]
## [1] 0.5620088































