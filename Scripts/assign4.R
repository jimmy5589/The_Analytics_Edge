
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
