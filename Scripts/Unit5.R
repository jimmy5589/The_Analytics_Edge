# Unit 5

install.packages("tm")
# Package to help to use "tm"
install.packages("SnowballC")
library(tm)
library(SnowballC)

length(stopwords("english"))

Sys.setlocale("LC_ALL", "C")

tweets=read.csv("tweets.csv", stringsAsFactors = FALSE)
tweets$Negative=as.factor(tweets$Avg <= -1)
str(tweets)

table(tweets$Negative)

# Corpus: is a collection of documents
corpus=Corpus(VectorSource(tweets$Tweet))

corpus[[1]]

corpus=tm_map(corpus, tolower)
corpus[[1]]

corpus = tm_map(corpus, PlainTextDocument)

corpus=tm_map(corpus, removePunctuation)
corpus[[1]]

stopwords("english")[1:10]

corpus=tm_map(corpus, removeWords, c("apple", stopwords("english")))
corpus[[1]]$content

corpus=tm_map(corpus, stemDocument)
corpus[[1]]$content

#

frequencies=DocumentTermMatrix(corpus)
frequencies
# look at documents from 1000 to 1005 and words 505 to 515
inspect(frequencies[1000:1005,505:515])
# the result is "sparse"

findFreqTerms(frequencies, lowfreq = 20)

# Sparsety of 0.995, only keep terms that show in more than 0.5% of the tweets
sparse= removeSparseTerms(frequencies, 0.995)
sparse

tweetsSparse= as.data.frame(as.matrix(sparse))
tweetsSparse

# take care of the names
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
# Add dependent variable
tweetsSparse$Negative = tweets$Negative

# split the data
library(caTools)

set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio =  0.7 )
rm("train","test")
trainSparse=subset(tweetsSparse, split==TRUE)
testSparse=subset(tweetsSparse, split==FALSE)

# Quick question
findFreqTerms(frequencies, lowfreq = 100)
#

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method = "class")
prp(tweetCART)

predictCART = predict(tweetCART, newdata = testSparse, type = "class")

#confusion matrix
table(testSparse$Negative, predictCART)
accuracy=(294+18)/nrow(testSparse)
accuracy
# 0.8788732
# Baseline always predict Negative
table(testSparse$Negative)
accuracy=300/355
accuracy
# 0.845

# what about the random forest
library(randomForest)

set.seed(123)
tweetRF = randomForest(Negative ~ ., data=trainSparse)
predictRF = predict(tweetRF, newdata = testSparse)

table(testSparse$Negative, predictRF)
# accuracy
accuracy = (293+21)/nrow(testSparse)
accuracy
# 0.884507

# Quick question
tweetLOG = glm(Negative ~ ., data=trainSparse, family="binomial")
predictLOG = predict(tweetLOG, newdata = testSparse, type="response")

table(testSparse$Negative, predictLOG > 0.5)
(253+32)/nrow(testSparse)

# The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline. 
# If you were to compute the accuracy on the training set instead, you would see that 
# the model does really well on the training set - this is an example of over-fitting. 
# The model fits the training set really well, but does not perform well on the test set. 
# A logistic regression model with a large number of variables is particularly at risk 
# for overfitting.


