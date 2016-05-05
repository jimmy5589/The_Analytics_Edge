polling=read.csv("PollingData.csv")
summary(polling)

table(polling$Year)

install.packages("mice")
library(mice)

simple=polling[c("Rasmussen","SurveyUSA","PropR","DiffCount")]
set.seed(144)
inputed=complete(mice(simple))
summary(inputed)

polling$Rasmussen=inputed$Rasmussen
polling$SurveyUSA=inputed$SurveyUSA

Train=subset(polling, Year=="2004" | Year=="2008")
Test=subset(polling, Year=="2012")

# Check the dependent variable
table(Train$Republican)

#Building a baseline

#Lets create a smart baseline
#Lets use the Rasmussen
# and use the function sign

table(sign(Train$Rasmussen))

# Lets compare now our baseline with the actual Poll results in the Train
table(Train$Republican, sign(Train$Rasmussen))

# Better baseline
# predicted more close to the actual data

# Lets check Train for colinearity
cor(Train)
# returns an error, due to the fact that State is not numeric
str(Train)

#So, we take only the numeric independent var + the dependent one
cor(Train[c("Rasmussen","SurveyUSA","PropR","DiffCount","Republican")])

# All seem to be high correlated
# so we just take one that is highly correlated with the dependent one
# in this case PropR

mod1=glm(Republican ~ PropR, data=Train, family=binomial)
summary(mod1)

# Lets do a prediction within the Train dataset
# and because of that we not going to use new data
pred1= predict(mod1, type="response")
table(Train$Republican, pred1 > 0.5)

# only 2+2=4 mistakes
#same as our baseline
# lets try to improve the model
# adding another variable
# lets choose 2 vraiables that have a low correlation between them
# they might improve the model
# so we gonna try the pair of variables (SurveyUSA and DiffCount)
mod2=glm(Republican ~ SurveyUSA+DiffCount, data=Train, family=binomial)
pred2= predict(mod2, type="response")
table(Train$Republican, pred2 > 0.5)


#less mistakes only 3
#But variables are less significant (drawback)
#but AIC is smaller making it a better model
#nevertheless we will use this model for our predictions

# time to evaluate our model in the testing set

#lets comapare our testing set dependent variable with our baseline in the Test also

table(Test$Republican, sign(Test$Rasmussen))

# 4 mistakes and 2 inconclusive results on the testing set.

TestPrediction= predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction>=0.5)

# Lets remove the only mistake out
subset(Test, TestPrediction >= 0.5 & Republican==0)

# Only 1 mistake, is outperforming the baseline so is a nice model to use


# Assignment 3

songs=read.csv("songs.csv")
summary(songs)
str(songs)
nrow(songs[songs$year==2010,])

MJ=songs[songs$artistname=="Michael Jackson",]

nrow(MJ)

MJ[MJ$Top10==TRUE,]$songtitle

table(songs$timesignature)

songs[songs$tempo==max(songs$tempo),]

SongsTrain=subset(songs, songs$year<=2009)
SongsTest=subset(songs, songs$year==2010)

# trick to exclude variables from our model
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

head(SongsTrain$timesignature_confidence)

cor(SongsTrain$loudness, SongsTrain$energy)

# For numeric variables we can remove them this way
SongsLog2 = glm(Top10 ~ .-loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ .-energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

TestPrediction= predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, TestPrediction>0.45)
# Accuracy TN+TP/N
(309+19)/nrow(SongsTest)

# Accuracy of the baseline
# Baseline will be the non-Top10 because is more easy to predict
# checking the accuracy of the baseline on the test set:
1-mean(SongsTest$Top10)

# How many songs our model correctly predicts in our test set
TestPrediction


sensitivity=t[2,2]/(t[2,1]+t[2,2])
sensitivity
specificity=t[1,1]/(t[1,1]+t[1,2])
specificity

t[2,2]

t=table(SongsTest$Top10, TestPrediction >= 0.45)
t
(19/(19+40))
(309/(309+5))
sensitivity=t[2,2]/(t[2,1]+t[2,2])
sensitivity
specificity=t[1,1]/(t[1,1]+t[1,2])
specificity

