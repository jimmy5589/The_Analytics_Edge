library(caTools)

submission=read.csv("sampleSubmission2016.csv")
summary(submission)

train=read.csv("train2016.csv")
summary(train)

test=read.csv("test2016.csv")
summary(test)

## treat the data
## NAs for Train
train[train$Gender=="",]$Gender=NA
train[train$Income=="",]$Income=NA
train[train$HouseholdStatus=="",]$HouseholdStatus=NA
train[train$EducationLevel=="",]$EducationLevel=NA
train$Democrat=train$Party=="Democrat"
train$Party=NULL

summary(train)
str(train)
head(train,10)

## NAs for Test
test[test$Gender=="",]$Gender=NA
test[test$Income=="",]$Income=NA
test[test$HouseholdStatus=="",]$HouseholdStatus=NA
test[test$EducationLevel=="",]$EducationLevel=NA

summary(test)
str(test)
head(test,10)
##

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(train), "Democrat")

imputed = complete(mice(train[vars.for.imputation],MaxNWts = 3000))

train[vars.for.imputation] = imputed

summary(train)

set.seed(144)

vars.for.imputation = names(test)

imputed = complete(mice(test[vars.for.imputation],MaxNWts = 3000))

test[vars.for.imputation] = imputed

summary(test)


?complete
?mice
methods(mice)

#########
spl=sample.split(train$Democrat, SplitRatio = 0.75)
trn=subset(train, spl==TRUE)
tst=subset(train, spl==FALSE)

model1=glm(Democrat ~ .-USER_ID, data=trn, family=binomial)

summary(model1)
predictTst = predict(model1, newdata=tst, type="response")

table(tst$Democrat)
t=table(tst$Democrat, predictTst > 0.5 )
t
accuracy=(t[1,2]+t[2,2])/nrow(tst)
accuracy
# 0.5739943
head(predictTst)


#

df=train

library("plyr")
# df$Q96024 = car::recode(df$Q96024, "''='None'")

########################################
# Convert the empty levels to NONE
########################################
to.replace <- names(which(sapply(df, is.factor)))
to.replace

for (mylabel in to.replace) cat(paste("df","$",mylabel,sep=""),"= car::recode(",paste("df$",mylabel, sep="") ,",\"\'\'=\'None\'\")\n")


#######################################

summary(trn)
summary(model1)
set.seed(144)
model2=glm(Democrat ~ YOB + Income + HouseholdStatus + EducationLevel + Gender +
      Q124742 + Q122771 + Q121699 + Q120379 + Q120472 + 
      Q120194 + Q118232 + Q118233 + Q116881 + Q116953 +
      Q115611 + Q115899 + Q115390 + Q113181 + Q110740 + Q109244 +
      Q107869 + Q106997 + Q106389 + Q106042 + Q104996 + Q102687 + Q101163 +
        Q101596 + Q100689 + Q99480 + Q98869 + Q98578 + Q98197 
      , data=trn, family=binomial)
# Q118892 + Q112478 
summary(model2)
predictTst2 = predict(model2, newdata=tst, type="response")

t=table(tst$Democrat, predictTst2 > 0.5 )
t
accuracy=(t[1,1]+t[2,2])/nrow(tst)
accuracy

###
model2=glm(Democrat ~ .-USER_ID 
           , data=trn, family=binomial)
# Q118892 + Q112478 
summary(model2)
predictTst2 = predict(model2, newdata=tst, type="response")

t=table(tst$Democrat, predictTst2 > 0.5 )
t
accuracy=(t[1,1]+t[2,2])/nrow(tst)
accuracy


############################################################

##
trainIN=train
testIN=test

library(plyr)
table(trainIN$Q109244)


trainIN$Q109244=revalue(trainIN$Q109244, c("No"="No","None"=NA,"Yes"="Yes"))


levels(trainIN$Q109244)

testIN$Q109244=revalue(testIN$Q109244, c("No"="No","None"=NA,"Yes"="Yes"))

table(testIN$Q109244)


#####################

library(mice)

set.seed(144)

vars.for.imputation = setdiff(names(trainIN), "Democrat")

imputed = complete(mice(trainIN[vars.for.imputation],MaxNWts = 3000))

trainIN[vars.for.imputation] = imputed

summary(trainIN)

set.seed(144)

vars.for.imputation = names(testIN)

imputed = complete(mice(testIN[vars.for.imputation],MaxNWts = 3000))

testIN[vars.for.imputation] = imputed

summary(testIN)

#####################

