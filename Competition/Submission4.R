# Submission 4
# Submission4
model.d1=glm(Democrat ~ .-USER_ID, data=train, family=binomial)

summary(model.d1)
predictTst_d1 = predict(model.d1, newdata=test, type="response")


table(predictTst_d1 >= 0.5)

head(predictTst_d1)

# Build the dataframe ##########

submission1=data.frame(test$USER_ID)
submission1$Predictions=ifelse(predictTst_d1 >= 0.5, "Democrat","Republican")

table(submission1$Predictions)
colnames(submission1)=c("USER_ID","Predictions")
# wroite .csv file
write.csv(file = "submission1.csv", x = submission1, quote=FALSE, row.names = FALSE)

################################


summary(submission1)
head(submission1)
head(submission)
str(submission1)
str(submission)



####
summary(trn)
summary(train)
summary(tst)
table(trn$Q109244)
table(trn$Q115611)
#
# No   None  Yes 
# 1864 1624  688 

1864/(1864+688)
# 0.7304075  0.2695925
# split of 1186 No + 438 Yes

trn2=subset(trn, trn$Q109244 == "No" | trn$Q109244 == "Yes")
tst2=subset(tst, tst$Q109244 == "No" | tst$Q109244 == "Yes")

table(trn2$Q109244)
set.seed(144)
model3=glm(Democrat ~ 
            Q115611 + Q109244 +
             Q98197 
           , data=trn2, family=binomial)
# Q101596, Q113181, Q116953, Q118232, HouseholdStatus
summary(model3)
predictTst3 = predict(model3, newdata=tst2, type="response")

t=table(tst2$Democrat, predictTst3 > 0.50 )
t
accuracy=(t[1,1]+t[2,2])/nrow(tst2)
accuracy

# 0.5553161
# 0.5596264  removed Q101596, Q113181
# 0.5833333  removed above + Q116953
#              Q118232
# 0.6185345

### CART

library(rpart)
library(rpart.plot)

modelCART3=rpart(Democrat ~ 
                   Q115611 + Q109244 +
                   Q98197 + Q118232
                 , data=trn, method = "class", cp=0.04)
prp(modelCART3)
predictCART3 = predict(modelCART3, newdata = tst, type = "class")

#confusion matrix
t=table(tst$Democrat, predictCART3)
t
accuracy=(t[1,1]+t[2,2])/nrow(tst)
accuracy

table(predictCART3)

## RF

set.seed(144)
trnRF = randomForest( Democrat ~ 
                        Q115611 + Q109244 +
                        Q98197 + Q118232
                      , data=trn, nodesize=25, ntree=200 )
predictRF = predict(trnRF, newdata = tst, type = "class")

head(predictRF)
t=table(tst$Democrat, predictRF)
t
# accuracy
accuracy=(t[1,1]+t[2,2])/nrow(tst)
accuracy
# 0.6235632

####
# Submission 3
####

library(randomForest)

set.seed(144)
trainRF = randomForest( Democrat ~ HouseholdStatus +
                          Q118232 + Q116953 +
                          Q115611 + Q109244 +
                          Q98197, data=train, nodesize=25, ntree=200 )
predictRFsub = predict(trainRF, newdata = test)

head(predictRFsub)

# Build the dataframe ##########

submission=data.frame(test$USER_ID)
submission$Predictions=ifelse(predictRFsub >= 0.5, "Democrat","Republican")

table(submission$Predictions)
colnames(submission)=c("USER_ID","Predictions")
# write.csv file
write.csv(file = "submission4.csv", x = submission, quote=FALSE, row.names = FALSE)

### Submission 6

model.d1=glm(Democrat ~ 
               Q115611 + Q109244 +
               Q98197 + Q118232
             , data=train, family=binomial)

summary(model.d1)
predictTst_d1 = predict(model.d1, newdata=test, type="response")


table(predictTst_d1 >= 0.5)

head(predictTst_d1)

# Build the dataframe ##########

submission1=data.frame(test$USER_ID)
submission1$Predictions=ifelse(predictTst_d1 >= 0.5, "Democrat","Republican")

table(submission1$Predictions)
colnames(submission1)=c("USER_ID","Predictions")
# wroite .csv file
write.csv(file = "submission6.csv", x = submission1, quote=FALSE, row.names = FALSE)

###############

train$Democrat = as.factor(train$Democrat)
summary(train)


## CROSS VALIDATION


library(caret)
library(e1071)

numFolds=trainControl(method = "cv", number = 10)
cpGrid=expand.grid(.cp=seq(0.01,0.5,0.01))

train(Democrat ~ 
        Q115611 + Q109244 +
        Q98197 + Q118232
      , data=trn, 
      method = "rpart", trControl=numFolds, tuneGrid=cpGrid)
# result was cp=0.19
#


