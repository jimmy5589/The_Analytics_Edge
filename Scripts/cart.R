
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


#####

Claims=read.csv("ClaimsData.csv")

prop.table(table(Claims$bucket2009))
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio=0.6)
ClaimsTrain=subset(Claims, spl==TRUE)
ClaimsTest=subset(Claims, spl=FALSE)

# Quick question
str(ClaimsTrain)
mean(ClaimsTrain$age)

prop.table(table(ClaimsTrain$diabetes))
##

# Accuracy for the baseline model on the test set
# Baseline model predicts the same outcome for 2009 as 2008
#  
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

# add the diagonal and divide by the nrow of test set
0.68

# Build the penalty matrix
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix
# actual on the left, predicted on the righ

as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
# multiplies correspondent numbers in both matrixes

# Penalty error
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)
0.7396207

# Quick test
# Assume the baseline method would predict cost bucket 1 to everyone
out=rep(1, times=nrow(ClaimsTest))

# Outcome vs prediction
pred=ClaimsTest
str(pred)

pred$bucket2008=as.vector(rep(as.integer(1), times=nrow(ClaimsTest)))
mean(pred$bucket2008)
t=table(ClaimsTest$bucket2009, pred$bucket2008)
t

accuracy=t[1]/nrow(ClaimsTest)
accuracy
0.6712678

# Now Penalty Error

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix
t
t*PenaltyMatrix[,1]

sum(t*PenaltyMatrix[,1])/nrow(ClaimsTest)

####

library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)
prp(ClaimsTree)

PredictTest=predict(ClaimsTree, newdata=ClaimsTest, type="class")

# accuracy now

table(ClaimsTest$bucket2009, PredictTest)

