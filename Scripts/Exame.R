# Exame 

fedFunds=read.csv("federalFundsRate.csv", stringsAsFactors = FALSE)

str(fedFunds)
prop.table(table(fedFunds$RaisedFedFunds))


t=table(fedFunds$Chairman)

t[3]

fedFunds$Chairman = as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres = as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds = as.factor(fedFunds$RaisedFedFunds)
str(fedFunds)

set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)

training=subset(fedFunds, spl==TRUE)
testing = subset(fedFunds, spl==FALSE)

logi=glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection,
            family=binomial, data = training)
summary(logi)

logi$coefficients

thismonth=c(1, 1.7, -3, 5.1, 65.3, 0, 18)
logi$coefficients * thismonth
logit=sum(logi$coefficients * thismonth)
logit
odds=exp(logit)
odds
1/odds
1/(1+1/odds)

mydata=head(training,1)

mydata[1,]$PreviousRate = 1.7
mydata[1,]$Streak = -3
mydata[1,]$Unemployment = 5.1
mydata[1,]$HomeownershipRate = 65.3
mydata[1,]$DemocraticPres = 0
mydata[1,]$MonthsUntilElection = 18 

mydata
predictMydata=predict(logi, type="response", newdata = mydata)
predictMydata
head(training)

predictTest=predict(logi, type="response", newdata = testing)

t=table(testing$RaisedFedFunds, predictTest > 0.5)
t

accuracy=(t[1,1]+t[2,2])/length(predictTest)
accuracy

table(testing$RaisedFedFunds)
