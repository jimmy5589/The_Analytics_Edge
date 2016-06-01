# Unit 5 assignment

wiki=read.csv("wiki.csv", stringsAsFactors=FALSE)

wiki$Vandal=as.factor(wiki$Vandal)

table(wiki$Vandal)

# Corpus: is a collection of documents
corpusAdded=Corpus(VectorSource(wiki$Added))

# Remove English stop words
corpusAdded=tm_map(corpusAdded, removeWords, stopwords("english"))

# Stem the words
corpusAdded=tm_map(corpusAdded, stemDocument)

# Build the document term matrix
dtmAdded=DocumentTermMatrix(corpusAdded)
dtmAdded

# Remove sparse terms, keeping the ones that appear in more than 0.3% of the revisions
length(stopwords("english"))
sparseAdded=removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Convert to a data frame
wordsAdded=as.data.frame(as.matrix(sparseAdded))

str(wordsAdded)

# Prepend all the words with the letter "A"
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)

####
# Same for Words Removed
####
# Corpus: is a collection of documents
corpusRemoved=Corpus(VectorSource(wiki$Removed))

# Remove English stop words
corpusRemoved=tm_map(corpusRemoved, removeWords, stopwords("english"))

# Stem the words
corpusRemoved=tm_map(corpusRemoved, stemDocument)

# Build the document term matrix
dtmRemoved=DocumentTermMatrix(corpusRemoved)

# Remove sparse terms, keeping the ones that appear in more than 0.3% of the revisions
sparseRemoved=removeSparseTerms(dtmRemoved, 0.997)

# Convert to a data frame
wordsRemoved=as.data.frame(as.matrix(sparseRemoved))

str(wordsRemoved)

# Prepend all the words with the letter "A"
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

### Combine both

wikiWords=cbind(wordsAdded, wordsRemoved)
# Add dependent variable
wikiWords$Vandal=wiki$Vandal

set.seed(123)

#Split in Train and Test 0.7
spl=sample.split(wikiWords$Vandal, SplitRatio=0.7)
train=subset(wikiWords, spl==TRUE)
test=subset(wikiWords, spl==FALSE)

# Baseline accuracy for non-vandalism on the test set
t=table(test$Vandal)
t
accuracy=t[1]/nrow(test)
accuracy
# 0.5343643
# 0.5313844  correct

# Build CART model

wikiCART=rpart(Vandal ~ ., data=train, method = "class")
prp(wikiCART)

pred=predict(wikiCART, newdata = test, type="class")

# Confusion Matrix
t=table(test$Vandal, pred)
t
accuracy=(t[1,1]+t[2,2])/length(pred)
accuracy
# 0.5463918
# 0.5417025 correct

# We weren't able to improve on the baseline using the raw textual information. 
# More specifically, the words themselves were not useful. There are other options 
# though, and in this section we will try two techniques - identifying a key class 
# of words, and counting words.

# We hypothesize that given that a lot of vandalism seems to be adding links to 
# promotional or irrelevant websites, the presence of a web address is a sign of vandalism.

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

# use the old split
train2 = subset(wikiWords2, spl==TRUE)

test2 = subset(wikiWords2, spl==FALSE)

wikiCART2=rpart(Vandal ~ ., data=train2, method = "class")
prp(wikiCART2)

pred2=predict(wikiCART2, newdata = test2, type="class")

# Confusion Matrix
t2=table(test2$Vandal, pred2)
t2
accuracy=(t2[1,1]+t2[2,2])/length(pred2)
accuracy
# 0.5867698
# 0.5726569 correct
# error made when the split DID NOT SPECIFY THE VARIABLE  !!!!

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

# use the old split
train3 = subset(wikiWords2, spl==TRUE)

test3 = subset(wikiWords2, spl==FALSE)

wikiCART3=rpart(Vandal ~ ., data=train3, method = "class")
prp(wikiCART3)

pred3=predict(wikiCART3, newdata = test3, type="class")

# Confusion Matrix
t3=table(test3$Vandal, pred3)
t3
accuracy=(t3[1,1]+t3[2,2])/length(pred3)
accuracy
# 0.6552021

####
wikiWords3 = wikiWords2

wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

train3 = subset(wikiWords3, spl==TRUE)

test3 = subset(wikiWords3, spl==FALSE)

wikiCART3=rpart(Vandal ~ ., data=train3, method = "class")
prp(wikiCART3)

pred3=predict(wikiCART3, newdata = test3, type="class")

# Confusion Matrix
t3=table(test3$Vandal, pred3)
t3
accuracy=(t3[1,1]+t3[2,2])/length(pred3)
accuracy

# 0.7188306
#############################################
# Problem 2
############

trials = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
summary(trials)

max(nchar(trials$abstract))
#3708
sum(nchar(trials$abstract)==0)
#112
min(nchar(trials$title))
#28
trials[nchar(trials$title)==28,]$title

corpusTitle=Corpus(VectorSource(trials$title))
corpusAbstract=Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))
str(dtmTitle)
str(dtmAbstract)

#
ind=which(colSums(dtmAbstract) == max(colSums(dtmAbstract)))

colSums(dtmAbstract)[ind]
#

colnames(dtmTitle) = paste0("T", colnames(dtmTitle))

colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))
str(dtmAbstract)

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

set.seed(144)

spl = sample.split(dtm$trial, SplitRatio = 0.7)  # !!!! forgot AGAIN THE SPLIT TAKES THE DEPENDENT VARIABLE


train = subset(dtm, spl == TRUE)
test = subset(dtm, spl == FALSE)

table(train$trial)
accuracy=726/nrow(train)
accuracy
#

trialCART = rpart(trial ~ ., data=train, method="class")
prp(trialCART)

pred = predict(trialCART, data=train)
head(pred)
max(pred[,2])
pred.prob=pred[,2]

# Because the CART tree assigns the same predicted probability to each leaf node and 
# there are a small number of leaf nodes compared to data points, we expect exactly the 
# same maximum predicted probability.

t=table(train$trial, pred.prob > 0.5)
t
accuracy=(t[1,1]+t[2,2])/length(pred.prob)
accuracy
sensitivity=t[2,2]/(t[2,1]+t[2,2])  
sensitivity
specificity=t[1,1]/(t[1,1]+t[1,2])
specificity

# wrong 0.8233487

predTest = predict(trialCART, newdata=test, type = "class")
t=table(test$trial, predTest)
t
accuracy=(t[1,1]+t[2,2])/length(predTest)
accuracy

# AUC

predROC=predict(trialCART, newdata=test)
predROCR = prediction(predROC[,2], test$trial)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values


#############
# PROBLEM 3
######

emails = read.csv("emails.csv", stringsAsFactors = FALSE)

table(emails$spam)
emails[1,]
summary(emails)

max(nchar(emails$text))
which(nchar(emails$text) == min(nchar(emails$text)))
    
emails[1992,]

# CORPUS

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm
emailsSparse = as.data.frame(as.matrix(spdtm))
summary(emailsSparse)
which.max(colSums(emailsSparse))

emailsSparse$spam = emails$spam

ham=emailsSparse[emailsSparse$spam == 0,]
ncol(ham[,colSums(ham) > 5000])
head(ham[,colSums(ham) > 5000])

spam=emailsSparse[emailsSparse$spam == 1,]
ncol(spam[,colSums(spam) > 1000])
head(spam[,colSums(spam) > 1000])

emailsSparse$spam = as.factor(emailsSparse$spam)
set.seed(123)

spl = sample.split(emailsSparse$spam, SplitRatio = 0.7)

train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data=train, family = "binomial")

spamCART = rpart(spam ~ ., data=train, method = "class")
prp(spamCART)


summary(spamLog)

predLog=predict(spamLog, data=train, type="response")
table(predLog < 0.00001)
table(predLog > 0.99999)
table(predLog >= 0.00001 & predLog <= 0.99999)

table(train$spam, predLog>0.5)
(3052+954)/nrow(train)

# AUC

predROC=predict(spamLog, data=train)
head(predROC)
predROCR = prediction(predROC, train$spam)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)

# Compute AUC

performance(predROCR, "auc")@y.values

# spamCART
predCART=predict(spamCART, data=train, type="class")
table(train$spam, predCART)
(2885+894)/nrow(train)

# AUC
predROC=predict(spamCART, data=train)
head(predROC)
predROCR = prediction(predROC[,2], train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
#

# randomForest
# Convert column names
colnames(train) = make.names(colnames(train))
set.seed(123)
spamRF = randomForest(spam ~ . , data=train )
predictRF = predict(spamRF, data=train)
table(train$spam, predictRF)
(3013+914)/nrow(train)

predictRF = predict(spamRF, data=train, type="prob")

# AUC
predROC=predict(spamRF, data=train, type = "prob")
head(predROC)
predROCR = prediction(predROC[,2], train$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
#

# For the Test set
predLog=predict(spamLog, newdata=test, type="response")
t=table(test$spam, predLog>0.5)
t
(t[1,1]+t[2,2])/nrow(test)
# 0.9505239

# AUC

predROC=predict(spamLog, newdata=test)
head(predROC)
predROCR = prediction(predROC, test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
#

# spamCART
predCART=predict(spamCART, newdata=test, type="class")
t=table(test$spam, predCART)
t
(t[1,1]+t[2,2])/nrow(test)

# AUC
predROC=predict(spamCART, newdata=test)
head(predROC)
predROCR = prediction(predROC[,2], test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
#

# randomForest
# Convert column names
colnames(test) = make.names(colnames(test))
set.seed(123)
spamRF = randomForest(spam ~ . , data=train )
predictRF = predict(spamRF, newdata=test)
t=table(test$spam, predictRF)
t
(t[1,1]+t[2,2])/nrow(test)

# AUC
predROC=predict(spamRF, newdata=test, type = "prob")
head(predROC)
predROCR = prediction(predROC[,2], test$spam)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
# Compute AUC
performance(predROCR, "auc")@y.values
#
#############
# OPTIONAL: PROBLEM 4 continuation of PROBLEM 3
####

wordCount = rowSums(as.matrix(dtm))
hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount=log(wordCount)

boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)

