
stevens=read.csv("stevens.csv")

library(caTools)

set.seed(3000)
# First argument for the split is the outcome variable
spl=sample.split(stevens$Reverse, SplitRatio = 0.7)

Train=subset(stevens, spl==TRUE)
Test=subset(stevens, spl==FALSE)

# RPART and RPART.PLOT packages
install.packages("rpart")
library(rpart)

install.packages("rpart.plot")
library(rpart.plot)

# Create a CART tree. "class" type = Classification.

StevensTree=rpart(Reverse ~ Circuit + Issue + Petitioner + 
                    Respondent + LowerCourt + Unconst, data=Train, 
                  method="class", minbucket=25)
# Plot the tree
prp(StevensTree)

# Lets create a prediction
PredictCART=predict(StevensTree,newdata = Test, type = "class")
# This is equivalent to use a threshold of 0.5

#Confusion matrix to measure accuracy
t=table(Test$Reverse, PredictCART)
t

accuracy=(t[1,1]+t[2,2])/length(PredictCART)
accuracy

# Evaluate our model using a ROC curve
library(ROCR)
# It is a plot of the true positive rate against the false positive rate for the 
# different possible cutpoints.
PredictROC=predict(StevensTree, newdata=Test)

PredictROC
# For each observation in the Test set, it gives 2 numbers which can be thought
# as the probability of outcome 0 and the probability of outcome 1.
# Each test set observation is classified into a subset, or bucket of our CART tree.
# These numbers gives the percentage of Train set data in that subset with outcome 0
# and the percentage of Train set data in that subset with outcome 1.

# Using the prediction function
pred=prediction(PredictROC[,2], Test$Reverse)
perf=performance(pred, "tpr", "fpr")
plot(perf)

# AUC
auc = as.numeric(performance(pred, "auc")@y.values)
auc

# Quick question
# Build a model for minbucket=5

StevensTree2=rpart(Reverse ~ Circuit + Issue + Petitioner + 
                    Respondent + LowerCourt + Unconst, data=Train, 
                  method="class", minbucket=5)
# Plot the tree
prp(StevensTree2)

# Another CART model now with minbuket=100
StevensTree3=rpart(Reverse ~ Circuit + Issue + Petitioner + 
                     Respondent + LowerCourt + Unconst, data=Train, 
                   method="class", minbucket=100)
# Plot the tree
prp(StevensTree3)

##################
## RANDOM FORESTS
##################

install.packages("randomForest")
library("randomForest")

StevensForest=randomForest(Reverse ~ Circuit + Issue + Petitioner + 
                     Respondent + LowerCourt + Unconst, data=Train, 
                   nodesize=25, ntree=200)
# randomForest doesnt have a method so if we want to create a classification model
# we need to make sure our outcome is a factor
# lets convert to factor
Train$Reverse=as.factor(Train$Reverse)
Test$Reverse=as.factor(Test$Reverse)

StevensForest=randomForest(Reverse ~ Circuit + Issue + Petitioner + 
                             Respondent + LowerCourt + Unconst, data=Train, 
                           nodesize=25, ntree=200)

# Lets make predictions
PredictForest=predict(StevensForest, newdata=Test)

# Confusion Matrix
t=table(Test$Reverse, PredictForest)
t

accuracy=(t[1,1]+t[2,2])/length(PredictForest)
accuracy

# Random Forests have a random component that might produce different results
# Quick question

set.seed(200)

StevensForest=randomForest(Reverse ~ Circuit + Issue + Petitioner + 
                             Respondent + LowerCourt + Unconst, data=Train, 
                           nodesize=25, ntree=200)
PredictForest=predict(StevensForest, newdata=Test)
t=table(Test$Reverse, PredictForest)
t
accuracy=(t[1,1]+t[2,2])/length(PredictForest)
accuracy

## CROSS VALIDATION

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

numFolds=trainControl(method = "cv", number = 10)
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))

train(Reverse ~ Circuit + Issue + Petitioner + 
                             Respondent + LowerCourt + Unconst, data=Train, 
                           method = "rpart", trControl=numFolds, tuneGrid=cpGrid)
# result was cp=0.19
# Now lets build a CART tree with this new cp values

StevensTreeCV=rpart(Reverse ~ Circuit + Issue + Petitioner + 
                     Respondent + LowerCourt + Unconst, data=Train, 
                   method="class", cp=0.19)
PredictCV=predict(StevensTreeCV, newdata=Test, type="class")
# Confusion matrix
t=table(Test$Reverse, PredictCV)
t
accuracy=(t[1,1]+t[2,2])/length(PredictCV)
accuracy

# Quick question
prp(StevensTreeCV)
