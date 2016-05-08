polling=read.csv("PollingData.csv")
summary(polling)

table(polling$Year)

install.packages("mice")
library(mice)

simple=polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
set.seed(144)
inputed=complete(mice(simple))
summary(inputed)

polling$Rasmussen=inputed$Rasmussen
polling$SurveyUSA=inputed$SurveyUSA

Train=subset(polling, Year=="2004" | Year=="2008")
Test=subset(polling, Year=="2012")

# Check the dependent variable
table(Train$Republican)

#Building a baseline

#Lets create a smart baseline
#Lets use the Rasmussen
# and use the function sign

table(sign(Train$Rasmussen))

# Lets compare now our baseline with the actual Poll results in the Train
table(Train$Republican, sign(Train$Rasmussen))

# Better baseline
# predicted more close to the actual data

# Lets check Train for colinearity
cor(Train)
# returns an error, due to the fact that State is not numeric
str(Train)

#So, we take only the numeric independent var + the dependent one
cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

# All seem to be high correlated
# so we just take one that is highly correlated with the dependent one
# in this case PropR

mod1=glm(Republican ~ PropR, data=Train, family=binomial)
summary(mod1)

# Lets do a prediction within the Train dataset
# and because of that we not going to use new data
pred1= predict(mod1, type="response")
table(Train$Republican, pred1 > 0.5)

# only 2+2=4 mistakes
#same as our baseline
# lets try to improve the model
# adding another variable
# lets choose 2 vraiables that have a low correlation between them
# they might improve the model
# so we gonna try the pair of variables (SurveyUSA and DiffCount)
mod2=glm(Republican ~ SurveyUSA+DiffCount, data=Train, family=binomial)
pred2= predict(mod2, type="response")
table(Train$Republican, pred2 > 0.5)


#less mistakes only 3
#But variables are less significant (drawback)
#but AIC is smaller making it a better model
#nevertheless we will use this model for our predictions

# time to evaluate our model in the testing set

#lets comapare our testing set dependent variable with our baseline in the Test also

table(Test$Republican, sign(Test$Rasmussen))

# 4 mistakes and 2 inconclusive results on the testing set.

TestPrediction= predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction>=0.5)

# Lets remove the only mistake out
subset(Test, TestPrediction >= 0.5 & Republican==0)

# Only 1 mistake, is outperforming the baseline so is a nice model to use


# Assignment 3

songs=read.csv("songs.csv")
summary(songs)
str(songs)
nrow(songs[songs$year==2010,])

MJ=songs[songs$artistname=="Michael Jackson",]

nrow(MJ)

MJ[MJ$Top10==TRUE,]$songtitle

table(songs$timesignature)

songs[songs$tempo==max(songs$tempo),]

SongsTrain=subset(songs, songs$year<=2009)
SongsTest=subset(songs, songs$year==2010)

# trick to exclude variables from our model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

head(SongsTrain$timesignature_confidence)

cor(SongsTrain$loudness, SongsTrain$energy)

# For numeric variables we can remove them this way
SongsLog2 = glm(Top10 ~ .-loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ .-energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

TestPrediction= predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, TestPrediction>0.45)
# Accuracy TN+TP/N
(309+19)/nrow(SongsTest)

# Accuracy of the baseline
# Baseline will be the non-Top10 because is more easy to predict
# checking the accuracy of the baseline on the test set:
1-mean(SongsTest$Top10)

# How many songs our model correctly predicts in our test set
TestPrediction


sensitivity=t[2,2]/(t[2,1]+t[2,2])
sensitivity
specificity=t[1,1]/(t[1,1]+t[1,2])
specificity

t[2,2]

t=table(SongsTest$Top10, TestPrediction >= 0.45)
t
(19/(19+40))
(309/(309+5))
sensitivity=t[2,2]/(t[2,1]+t[2,2])
sensitivity
specificity=t[1,1]/(t[1,1]+t[1,2])
specificity

# SECOND Problem

parole=read.csv('parole.csv')
summary(parole)
table(parole$violator)
str(parole)

# Convert unordered variables with at least 3 levels

parole$crime=factor(parole$crime)
parole$state=factor(parole$state)
summary(parole)

set.seed(144)
#install.packages("caTools")
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
table(split)
473/(473+202)

set.seed(144)
sample(LETTERS, 5)
#set.seed(144)
sample(LETTERS, 5)

set.seed(144)
split1 = sample.split(parole$violator, SplitRatio = 0.7)
set.seed(44)
split2 = sample.split(parole$violator, SplitRatio = 0.7)
table(split1==split2)

#
model1=glm(violator ~ ., data=train, family=binomial)
summary(model1)

# Problem 4.3

exp(1)

mean(train$multiple.offenses)
table(train$multiple.offenses)

# Odds=exp(a+b1x1+b2x2+...+bjxj)
# Odds=exp(a+b2x2+...+bjxj)*exp(b1*x1)
# for x1=1 unit increase and b1=1.6119919
exp(1.6119919)

# we have a 5.01 increase in the odds relatively to others without x1
# and otherwise identical

#Problem 4.4

summary(model1)
model1.coefficients

c=coef(model1)
c
c[1]
c[2]
coef(model1)["male"]

male=1
race=1
age=50
state2=0
state3=0
state4=0
timeserved=3
maxsentence=12
multipleoffences=0
crime2=1
crime3=0
crime4=0
logit=c[1]*1+ c[2]*male+c[3]*race+c[4]*age+c[5]*state2+c[6]*state3+c[7]*state4+
  c[8]*timeserved+c[9]*maxsentence+c[10]*multipleoffences+c[11]*crime2+c[12]*crime3+
  c[13]*crime4


c
logit
# Odds are the exp(logit)
odds=exp(logit)
odds

# Probablility=odds/(1+odds)

Prob=odds/(1+odds)
Prob

Prob/(1-Prob)

# Prediction on the Test set
pred=predict(model1, newdata=test, type="response")
pred
max(pred)

# Evaluate the model for the 0.5 threshold
t=table(test$violator, pred>0.5)
t
accuracy=(t[1,1]+t[2,2])/nrow(test)
accuracy
sensitivity=t[2,2]/(t[2,1]+t[2,2])
sensitivity
specificity=t[1,1]/(t[1,1]+t[1,2])
specificity

# A simple model that predict that everyone is a non violator is
# TN is the same + FN(False Positives), but TP is zero
((167+12)+0)/nrow(test)
str(test)
#To confirm
table(test$violator)

t=table(test$violator, pred>0.5)
t
t=table(test$violator, pred>0.1)
t

(t[1,1]+t[2,2])/nrow(test)

summary(model1)

# Using RORC
install.packages("ROCR")
library(ROCR)

ROCRpredTest = prediction(pred, test$violator)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

table(train$violator, train$year)

# Last homework

loans=read.csv("loans.csv")
summary(loans)
str(loans)

na=subset(loans, is.na(loans$log.annual.inc) | is.na(loans$days.with.cr.line) | 
          is.na(loans$revol.util) | is.na(loans$inq.last.6mths) | is.na(loans$delinq.2yrs) |
          is.na(loans$pub.rec))
summary(na)
head(na)
summary(na)
summary(loans)

loans=read.csv("loans_imputed.csv")
summary(loans)

set.seed(144)

split=sample.split(loans$not.fully.paid,0.7)

train=loans[split==TRUE,]
test=loans[split==FALSE,]
nrow(train)
nrow(test)

model1=glm(not.fully.paid ~ . , data=train, family=binomial)
summary(model1)
coef(model1)["fico"]*(700-710)
oddsAoverB=exp(coef(model1)["fico"]*(700-710))
oddsAoverB

pred1=predict(model1, newdata=test, type="response")
predicted.risk=pred1
t=table(test$not.fully.paid, pred1 > 0.5  )
t
accuracy=(t[1,1]+t[2,2])/nrow(test)
accuracy
sensitivity=t[2,2]/(t[2,1]+t[2,2])
sensitivity
specificity=t[1,1]/(t[1,1]+t[1,2])
specificity

# Baseline model assume will be not.fully.paid==0 which is the most frequent
accuracytest=nrow(test[test$not.fully.paid==0,])/nrow(test)
accuracytest


library("ROCR")

rocpredtest= prediction(pred1, test$not.fully.paid)
auc = as.numeric(performance(rocpredtest, "auc")@y.values)
auc

# add predicted risk to the test
test$predicted.risk = predicted.risk
summary(test)


model2=glm(not.fully.paid ~ int.rate , data=train, family=binomial)
summary(model2)
pred2=predict(model2, newdata=test, type="response")
max(pred2)
head(pred2)
table(test$not.fully.paid, pred2>0.5)

rocpred2test= prediction(pred2, test$not.fully.paid)
auc = as.numeric(performance(rocpred2test, "auc")@y.values)
auc

# To compute interest revenue, consider a $c investment in a loan that has an annual 
# interest rate r over a period of t years. Using continuous compounding of interest, 
# this investment pays back c * exp(rt) dollars by the end of the t years, 
# where exp(rt) is e raised to the r*t power.
# c*exp(rt)
# $10 with interest of 6% over 3 years
10*exp(0.06*3)
# Profit after t years is c*exp(rt)-c

summary(test)

test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1

max(test$profit*10)

highinterest=test[test$int.rate>=0.15,]
mean(highinterest$profit)
prop.table(table(highinterest$not.fully.paid))

cutoff = sort(highinterest$predicted.risk, decreasing=FALSE)[100]
cutoff

selectedLoans=highinterest[highinterest$predicted.risk <= cutoff,]
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
