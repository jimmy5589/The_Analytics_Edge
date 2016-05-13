
dat=read.csv("gerber.csv")
summary(dat)
str(dat)
table(dat$voting)
prop.table(table(dat$voting))
gerber=dat
tapply(gerber$voting, gerber$civicduty, mean)

tapply(gerber$voting, gerber$hawthorne, mean)

tapply(gerber$voting, gerber$self, mean)

tapply(gerber$voting, gerber$neighbors, mean)

logit=glm(voting ~ civicduty + hawthorne + self + neighbors, data=dat, family=binomial)
summary(logit)

pred=predict(logit, data=dat, type="response")
t=table(dat$voting, pred > 0.3)
t
accuracy=(t[1,1]+t[2,2])/nrow(dat)
accuracy

t=table(dat$voting, pred > 0.5)
t
accuracy=(t[1,1])/nrow(dat)
accuracy

library(ROCR)
ROCRpred=prediction(pred, dat$voting)
auc = as.numeric(performance(ROCRpred, "auc")@y.values)
auc

# Trees
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
summary(CARTmodel)
str(CARTmodel)
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

table(dat$control, dat$sex)
tapply(dat$voting, dat$sex, mean)

CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTcontrol, digits=6)
CARTcontsex = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTcontsex,digits=6)

abs(0.296638-0.34)

women=abs(0.290456-0.334176)
women
men=abs(0.302795-0.345818)
men
abs(men-women)<0.001

# problem 3.3

LogModel1=glm(voting ~ control + sex, data=gerber, family=binomial)
summary(LogModel1)


exp(-0.055791)=0.9457368 for women 
exp(0)=1

#If you look at the summary of the model, you can see that the coefficient for the 
#"sex" variable is -0.055791. This means that women are less likely to vote, 
#since women have a larger value in the sex variable, 
#and a negative coefficient means that larger values are predictive of 0.

# problem 3.4

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModel1, newdata=Possibilities, type="response")
Possibilities

abs(0.2908065-0.290456)

#Problem 3.5
#Interaction term sex:control added
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
# This coefficient is negative, so that means that a value of 1 in this variable 
# decreases the chance of voting. This variable will have variable 1 if the person
# is a woman and in the control group.
exp(-0.007259)
# result= 0.9927673

# Problem 3.6
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)

# This example has shown that trees can capture nonlinear relationships that logistic
# regression can not, but that we can get around this sometimes by using variables 
# that are the combination of two variables. Should we always include all possible 
# interaction terms of the independent variables when building a logistic regression model?

# We should not use all possible interaction terms in a logistic regression model due to 
# overfitting. Even in this simple problem, we have four treatment groups and two values 
# for sex. If we have an interaction term for every treatment variable with sex, 
# we will double the number of variables. In smaller data sets, this could quickly lead 
# to overfitting.

letters=read.csv("letters_ABPR.csv")

letters$isB = as.factor(letters$letter == "B")

set.seed(1000)
split=sample.split(letters$isB, SplitRatio = 0.5)
train=subset(letters, split==TRUE)
test=subset(letters, split==FALSE)

summary(letters)

table(test$isB)

accuracy=1175/(1175+383)
accuracy
# 0.754172

CARTb = rpart(isB ~ . - letter, data=train, method="class")
pred=predict(CARTb, newdata=test, type="class")

nrow(test)
table(test$isB, pred)
accuracy=(1118+340)/nrow(test)
accuracy
# 0.9358151

set.seed=1000
forest=randomForest(isB ~ . - letter, data=train)
pred=predict(forest, newdata=test)
table(test$isB, pred)
accuracy=(1164+373)/nrow(test)
accuracy
# 0.9865212


letters$letter = as.factor( letters$letter )

set.seed(2000)
split=sample.split(letters$letter, SplitRatio = 0.5)
train=subset(letters, split==TRUE)
test=subset(letters, split==FALSE)

# Baseline: predict the most frequent letter
table(test$letter)
#accuracy
401/nrow(test)
# 0.2573813

CARTl = rpart(letter ~ . - isB, data=train, method="class")
pred=predict(CARTl, newdata=test, type="class")

table(test$letter, pred)
# accuracy
(348+318+363+340)/nrow(test)
# 0.8786906

set.seed=1000

forest2=randomForest(letter ~ . - isB, data=train)
pred=predict(forest2, newdata=test)
table(test$letter, pred)

# accuracy
(390+379+393+367)/nrow(test)
# 0.9813864


### Last Exercise

census=read.csv("census.csv")

set.seed(2000)
split=sample.split(census$over50k, SplitRatio = 0.6)
train=subset(census, split==TRUE)
test=subset(census, split==FALSE)

logm=glm(over50k ~ . , data=train, family="binomial")
summary(logm)

pred=predict(logm, newdata=test, type="response")
table(test$over50k, pred >0.5)

#accuracy
(9051+1888)/nrow(test)

# 0.8552107

# now baseline accuracy for testing set
table(test$over50k)

9713/nrow(test)
# 0.7593621


ROCRtest = prediction(pred, test$over50k)

# AUC= Area Under the Curve = measures quality of prediction
auc = as.numeric(performance(ROCRtest, "auc")@y.values)
auc

ROCRperf=performance(ROCRtest, "tpr", "fpr")
plot(ROCRperf)

#0.9061598

summary(logm)

## Now CART model for same dataset
library(rpart)
library(rpart.plot)

CART3 = rpart(over50k ~ . , data=train, method="class")
prp(CART3)

pred3=predict(CART3, newdata=test, type="class")

table(test$over50k, pred3)
accuracy=(9243+1596)/nrow(test)
accuracy
# 0.8473927

pred3=predict(CART3, newdata=test)

head(pred3)
# Problem 2.5
# Need to take the secon column cause refers to >50K
ROCRtest3 = prediction(pred3[,2], test$over50k)

ROCRperf3=performance(ROCRtest3, "tpr", "fpr")

plot(ROCRperf3)

table(pred3[,2])  # for >50K shows 5 different probability values, corresponds to 5 buckets
table(pred3[,1])  # also for <=50k shows 5 different probability values

# Problem 2.6
# AUC= Area Under the Curve = measures quality of prediction
auc = as.numeric(performance(ROCRtest3, "auc")@y.values)
auc
# 0.8470256

## Problem 3
# Forest
install.packages("randomForest")
library("randomForest")

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
smallforest=randomForest(over50k ~ . , data=trainSmall)

predForest=predict(smallforest, newdata=test)

t=table(test$over50k, predForest)
t
(9586+1093)/nrow(test)
# accuracy = 0.8348839

# Problem 3.2
# One metric that we can look at is the number of times, aggregated over all of the trees 
# in the random forest model, that a certain variable is selected for a split. 
# To view this metric, run the following lines of R code (replace "MODEL" with the name 
# of your random forest model):
# MODEL=smallforest  
vu = varUsed(smallforest, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(smallforest$forest$xlevels[vusorted$ix]))

#Problem 3.3
# A different metric we can look at is related to "impurity", which measures how homogenous
# each bucket or leaf of the tree is. In each tree in the forest, whenever we select a 
# variable and perform a split, the impurity is decreased. Therefore, one way to measure 
# the importance of a variable is to average the reduction in impurity, 
# taken over all the times that variable is selected for splitting in all of the trees 
# in the forest. To compute this metric, run the following command in R 
# (replace "MODEL" with the name of your random forest model):

varImpPlot(smallforest)


# Problem 4.1
# Selectin cp by cross validation


