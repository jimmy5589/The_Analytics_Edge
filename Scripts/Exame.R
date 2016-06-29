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

thismonth=c(1, 0.017, -3, 0.051, 0.653, 0, 18)
logi$coefficients * thismonth
logit=sum(logi$coefficients * thismonth)
logit
odds=exp(logit)
odds
1/odds
1/(1+1/odds)

mydata=head(training,1)
mydata$PreviousRate
mydata[1,]$PreviousRate = 0.017
mydata[1,]$Streak = -3
mydata[1,]$Unemployment = 0.051
mydata[1,]$HomeownershipRate = 0.653
mydata[1,]$DemocraticPres = 0  
mydata[1,]$MonthsUntilElection = 18 

mydata
predictMydata=predict(logi, type="response", newdata = mydata)
predictMydata

