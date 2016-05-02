# Logistic Regression
quality=read.csv("quality.csv")

str(quality)


install.packages("caTools")
library(caTools)

# Lets split the quality data into training and testing
# need to set the seed to make sure we both have the same data as the teacher
set.seed(88)

split=sample.split(quality$PoorCare, SplitRatio=0.75)

qualityTrain = subset(quality, split==TRUE)
qualityTest = subset(quality, split==FALSE)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
str(qualityTrain)

summary(QualityLog)

# Prefered model the one with minimal AIC
predictTrain=predict(QualityLog, type="response")
# Type="Response" means results in probability

summary(predictTrain)

# values should be between 0 and 1 because we choose "probability"

tapply(predictTrain, qualityTrain$PoorCare, mean)

# Quick question
QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLog)

# Sensitivity vs Specificity

table(qualityTrain$PoorCare, predictTrain > 0.5)

#Sensitivity True Positives rate
10/25
#Specificity True Negatives rate
70/74


#ROC Curves
# Install package

install.packages("ROCR")
library("ROCR")

ROCRpred=prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf=performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# Returning to the originalmodel for the quick question

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)


predictTest = predict(QualityLog, type="response", newdata=qualityTest)

ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

# AUC= Area Under the Curve = measures quality of prediction
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc

# The AUC of a model has the following nice interpretation: 
# given a random patient from the dataset who actually received poor care, 
# and a random patient from the dataset who actually received good care, 
# the AUC is the perecentage of time that our model will classify which is which correctly.









