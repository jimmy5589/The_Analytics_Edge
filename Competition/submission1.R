# Submission1
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
