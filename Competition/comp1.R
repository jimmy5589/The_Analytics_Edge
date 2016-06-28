## Kaggle competition
library(caTools)

submission=read.csv("sampleSubmission2016.csv")
summary(submission)
train=read.csv("train2016.csv")
my=read.csv("train2016.csv", stringsAsFactors = FALSE)
str(my)
summary(train)
str(train)

cor(train$Gender, train$EducationLevel)
cor(a$Q124742, a$Q124122)

a=as.numeric(train$Q124742)
## Convert to numeric
cor(train)
a=train
to.replace <- names(which(sapply(a, is.factor)))
to.replace
for (var in to.replace) a[,eval(var)] = as.numeric(a[,eval(var)])
str(a)
#####
cor(a)
# Gets the levels
head(as.numeric(train$Gender))
head(train$Gender)

test=read.csv("test2016.csv")
summary(test)

str(train)
str(test)

spl=sample.split(train$Party, SplitRatio = 0.7)
trn=subset(train, spl==TRUE)
tst=subset(train, spl==FALSE)

model1=glm(Party ~ .-USER_ID -YOB , data=trn, family=binomial)

summary(model1)
predictTst = predict(model1, newdata=tst, type="response")

table(tst$Party)
t=table(tst$Party, predictTst > 0.5 )
t
accuracy=(t[1,2]+t[2,2])/nrow(tst)
accuracy

model2=glm(Party ~ HouseholdStatus + Q124742 + Q121699 + Q121700 + Q120379 + Q120650 +
             Q120472 + Q120012 + Q118892 + Q118232 + Q116797 + Q116197 + Q115611 +
             Q114517 + Q114152 + Q112478 + Q112512 + Q112270 + Q110740 + Q108950 +
             Q109244 + Q102089 + Q101163 + Q101596 + Q101596 + Q100689 + Q99716 +
             Q99581 + Q98869 + Q98578 + Q98197
           , data=trn, family=binomial)

summary(model2)
predictTst2 = predict(model2, newdata=tst, type="response")

t2=table(tst$Party, predictTst2 > 0.5 )
t2
accuracy=(t2[1,2]+t2[2,2])/nrow(tst)
accuracy

model3=glm(Party ~ HouseholdStatus + Q118892 + Q118232 + Q116197 + Q109244 + 
             Q101596 + Q98197
           , data=trn, family=binomial)

summary(model3)
predictTst3 = predict(model3, newdata=tst, type="response")

t3=table(tst$Party, predictTst3 > 0.5 )
t3
accuracy=(t3[1,2]+t3[2,2])/nrow(tst)
accuracy


predictTest = predict(model1, newdata=test, type="response")

table(tst$Party)
t=table(tst$Party, predictTst > 0.5 )
t

