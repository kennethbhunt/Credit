#Data set: credit_card.csv
#Predict the amount spent by a credit card owner (spent) using regression trees. The
#predictors are: gender, card, type and items.

Credit <-read.csv('credit_card.csv', stringsAsFactors = F)

str(Credit)
library(rpart)
library(rpart.plot)

n <-sample(26280,13000)
credit_train <- Credit[n, 1:5]
credit_test <-Credit[-n, 1:5]

## Grow the regression tree with rpart()
##rpart() has build it cross validation, 10 fold cv 

fit <- rpart(spent~., data=credit_train, method="anova")

#Plot the tree
prp(fit)

rpart.plot(fit)

## Print the complexity paremeter table 
printcp(fit)

##xerror is an approximation of the test set error 

###Compute goodness of fit for the test set 
pred<- predict(fit, credit_test)
mse <- sum((pred-credit_test$expend)^2)/26280
mse

var.y <- sum((credit_test$spent-mean(credit_test$spent))^2)/26280
var.y


Rsquared <- 1- mse/var.y
Rsquared

####Pruning 

### Extract cp value corresponding to lowest cv error x-error

ocp <- fit$cptable[which.min(fit$cptable[,"xerror"]), "cp"]

##prune the tree 
prfit <-prune(fit, ocp)

rpart.plot(prfit)

###Prune with a particular cp value

printcp(fit)

prfit2 <- prune(fit, 0.0172)
rpart.plot(prfit2)

####Bagging Regression Tree

library(randomForest)
RF_fit <-randomForest(spent~., data = credit_train, mtry=4)
##Compute predition accuracy in the test set 
RF_pred <-predict(RF_fit, credit_test)
head(bag_pred)

mse <- sum((RF_pred-credit_test$spent)^2)/26280
mse

var.y <- sum((credit_test$spent-mean(credit_test$spent))^2)/26279
var.y

Rsquared <- 1- mse/var.y
Rsquared
#88%

##Goodness of fit training set 

rf_pred2 <-predict(rf_fit, credit_train)
mse <- sum((rf_pred2-credit_train$spent)^2)/26280
mse

var.y <- sum((credit_train$spent-mean(credit_train$spent))^2)/26279
var.y


Rsquared <- 1- mse/var.y
Rsquared
#90%

plot(bag_fit)

####BoostingRegression trees 

library(gbm)

Boost_fit <-gbm(spent~., data = credit_train, distribution = "gaussian", 
                interaction.depth = 3, n.trees = 2000, shrinkage = 0.1)
summary(Boost_fit)

##Compute prediction accuracy in the Test set 

Boost_pred <- predict(Boost_fit, credit_test, n.trees = 2000)

mse <- sum((Boost_pred-credit_test$spent)^2)/26280
mse

var.y <- sum((credit_test$spent-mean(credit_test$spent))^2)/26279
var.y


Rsquared <- 1- mse/var.y
Rsquared
#87%

##Compute prediction accuracy in the Training
Boost_pred <- predict(Boost_fit, credit_train, n.trees = 2000)

mse <- sum((Boost_pred-credit_train$spent)^2)/26280
mse

var.y <- sum((credit_train$spent-mean(credit_train$spent))^2)/26279
var.y


Rsquared <- 1- mse/var.y
Rsquared

#####Bagging
library(ipred)

bag_fit <-bagging(spent~., data = credit_train)
summary(bag_fit)

##Compute prediction accuracy in the Test set 

Bag_pred <- predict(bag_fit, credit_test)

mse <- sum((Boost_pred-credit_test$spent)^2)/26280
mse

var.y <- sum((credit_test$spent-mean(credit_test$spent))^2)/26279
var.y


Rsquared <- 1- mse/var.y
Rsquared

##Compute prediction accuracy in the Training set 

Bag_pred2 <- predict(bag_fit, credit_train)

mse <- sum((Bag_pred2-credit_train$spent)^2)/26280
mse

var.y <- sum((credit_train$spent-mean(credit_train$spent))^2)/26279
var.y


Rsquared <- 1- mse/var.y
Rsquared
#84%

