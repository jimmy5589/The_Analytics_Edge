# Study
#########
summary(train)
set.seed(144)
spl=sample.split(train$Democrat, SplitRatio = 0.75)
trn=subset(train, spl==TRUE)
tst=subset(train, spl==FALSE)

# Kmeans ###

# clustering
# remove dependent variable
limitedtrn = trn

limitedtrn$Democrat = NULL

limitedtst = tst

limitedtst$Democrat = NULL

# Normalize the data
library(caret)

preproc = preProcess(limitedtrn)

normTrain = predict(preproc, limitedtrn)

normTest = predict(preproc, limitedtst)

# K-means
set.seed(144)
km = kmeans(normTrain, centers = 3)
str(km)
table(km$cluster)


###########
table(trn$Q109244)

###########
library(rpart)
library(rpart.plot)

modelCART=rpart(Democrat ~ .-USER_ID, data=trn, method = "class")
prp(modelCART)
predictCART = predict(modelCART, newdata = tst, type = "class")

#confusion matrix
t=table(tst$Democrat, predictCART)
t
accuracy=(t[1,1]+t[2,2])/nrow(tst)
accuracy

table(predictCART)

# 0.6063218  better than GLM 
# 0.5739943  Competition 936

# what about the random forest
library(randomForest)

set.seed(144)
trnRF = randomForest( Democrat ~ .-USER_ID, data=trn, nodesize=25, ntree=200 )
predictRF = predict(trnRF, newdata = tst, type = "class")

head(predictRF)
t=table(tst$Democrat, predictRF >= 0.5)
t
# accuracy
accuracy=(t[1,1]+t[2,2])/nrow(tst)
accuracy
# 0.6070402
# nodesize=25, ntree=200   produced accuracy = 0.6149425

################
# Submission 2
################

modelCARTsub=rpart(Democrat ~ .-USER_ID, data=train, method = "class")
prp(modelCARTsub)
predictCARTsub = predict(modelCARTsub, newdata = test, type = "class")
head(predictCARTsub)
# Build the dataframe ##########

submission2=data.frame(test$USER_ID)
submission2$Predictions=ifelse(predictCARTsub==TRUE, "Democrat","Republican")

table(submission2$Predictions)
colnames(submission2)=c("USER_ID","Predictions")
# write.csv file
write.csv(file = "submission2.csv", x = submission2, quote=FALSE, row.names = FALSE)

################################

####
# Submission 3
####

library(randomForest)

set.seed(144)
trainRF = randomForest( Democrat ~ .-USER_ID, data=train, nodesize=25, ntree=200 )
predictRFsub = predict(trainRF, newdata = test)

head(predictRFsub)

# Build the dataframe ##########

submission3=data.frame(test$USER_ID)
submission3$Predictions=ifelse(predictRFsub >= 0.5, "Democrat","Republican")

table(submission3$Predictions)
colnames(submission3)=c("USER_ID","Predictions")
# write.csv file
write.csv(file = "submission3.csv", x = submission3, quote=FALSE, row.names = FALSE)


# Submission 7

####
# Submission 3
####
model3=glm(Democrat ~ 
             Q115611 + Q109244 +
             Q98197 
           , data=trainIN, family=binomial)
summary(model3)
predictTst3 = predict(model3, newdata=testIN, type="response")

table(predictTst3 >= 0.5)

head(predictTst3)


# Build the dataframe ##########

submission3=data.frame(test$USER_ID)
submission3$Predictions=ifelse(predictTst3 >= 0.5, "Democrat","Republican")

table(submission3$Predictions)
colnames(submission3)=c("USER_ID","Predictions")
# write.csv file
write.csv(file = "submission7.csv", x = submission3, quote=FALSE, row.names = FALSE)



