FluTrain=read.csv('FluTrain.csv')

FluTrain[FluTrain$ILI == max(FluTrain$ILI),]
FluTrain[FluTrain$Queries == max(FluTrain$Queries),]

hist(FluTrain$ILI)

# When handling a skewed dependent variable, it is often useful to predict the logarithm
# of the dependent variable instead of the dependent variable itself -- this prevents the 
# small number of unusually large or small observations from having an undue influence 
# on the sum of squared errors of predictive models. In this problem, we will predict 
# the natural log of the ILI variable, which can be computed in R using the log() function.

plot(FluTrain$Queries, log(FluTrain$ILI))
plot(log(FluTrain$ILI), FluTrain$Queries)

#plot(FluTrain$ILI, FluTrain$Queries)

cor(FluTrain$Queries, FluTrain$ILI)

# Problem 2.2

# ****** IMPORTANT *** WARNING ***
# if you built your model using the format
# FluTrend1 = lm(log(dataframe$variable) ~ dataframe$other_variable) 
# you will get this error. Try instead
# FluTrend1 = lm(log(variable) ~ other_variable, data = dataframe)

# WRONG !!!!
FluTrend1=lm(log(FluTrain$ILI) ~ FluTrain$Queries, data=FluTrain)
#
FluTrend1=lm(log(ILI) ~ Queries, data=FluTrain)

summary(FluTrend1)

cor1=cor(log(FluTrain$ILI), FluTrain$Queries)
cor2=cor(FluTrain$ILI, FluTrain$Queries)

# Problem 2.3 
# Relation between R2 and the correlation
c(exp(-0.5*cor1),log(1/cor1),cor1^2)

# ******* IMPORTANT
# Relation between the R2 and the correlation is correlation^2
# For a single variable linear regression model, there is a direct relationship 
# between the R-squared and the correlation between the independent and the 
# dependent variables. What is the relationship we infer from our problem.

FluTest=read.csv("FluTest.csv")

# Predictions
# Be careful because of log
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))


which(FluTest$Week == '2012-03-11 - 2012-03-17')
PredTest1[11]

# Problem 3.3

SSE1=sum((PredTest1-FluTest$ILI)^2)
RMSE1=sqrt(SSE1/nrow(FluTest))


# Time Series Package loading

install.packages("zoo")

library(zoo)

# Changed ~/.Renvironment to add proxy

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

plot(log(FluTrain$ILI),log(FluTrain$ILILag2))

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

# Problem 4.3

FluTrend2=lm(log(ILI) ~ Queries + log(ILILag2) , data=FluTrain)
summary(FluTrend2)
summary(FluTrend1)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2=coredata(ILILag2)
summary(FluTest)

FluTest$ILILag2[2]=FluTrain$ILI[nrow(FluTrain)]
FluTest$ILILag2[1]=FluTrain$ILI[nrow(FluTrain)-1]
FluTest$ILILag2[1]
FluTest$ILILag2[2]

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))

PredTest2

SSE2=sum((PredTest2-FluTest$ILI)^2)
RMSE2=sqrt(SSE2/nrow(FluTest))

# LAst comments:
# How to avoid over-fitted models
# Use Predicted R2
# " This statistic is a form of cross-validation that doesn't require you to collect
# a separate sample. Instead, Minitab calculates predicted R-squared by systematically
# removing each observation from the data set, estimating the regression equation,
# and determining how well the model predicts the removed observation. "

SST=sum((mean(FluTrain$ILI)-FluTest$ILI)^2)
R2_1=1-SSE1/SST
R2_2=1-SSE2/SST
c(R2_1, R2_2)

# [1] 0.3224937 0.8954874

