# Unit6_assignment

dailykos = read.csv("dailykos.csv")
summary(dailykos)
str(dailykos)
distances = dist(dailykos[1:1545], method = "euclidean")
cluster = hclust(distances, method = "ward.D")
plot(cluster)

clusterGroups = cutree(cluster, k=7)
clusterh1 = subset(dailykos, clusterGroups == 1)
clusterh2 = subset(dailykos, clusterGroups == 2)
clusterh3 = subset(dailykos, clusterGroups == 3)
clusterh4 = subset(dailykos, clusterGroups == 4)
clusterh5 = subset(dailykos, clusterGroups == 5)
clusterh6 = subset(dailykos, clusterGroups == 6)
clusterh7 = subset(dailykos, clusterGroups == 7)

# or
HierCluster = split(dailykos, clusterGroups)
hierCluster[[1]]

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# K-means
k=7
set.seed(1000)
KMC = kmeans(dailykos, centers = k)
str(KMC)
#Extract clusters
dailykos.cluster = KMC$cluster

cluster1 = subset(dailykos, dailykos.cluster == 1)
cluster2 = subset(dailykos, dailykos.cluster == 2)
cluster3 = subset(dailykos, dailykos.cluster == 3)
cluster4 = subset(dailykos, dailykos.cluster == 4)
cluster5 = subset(dailykos, dailykos.cluster == 5)
cluster6 = subset(dailykos, dailykos.cluster == 6)
cluster7 = subset(dailykos, dailykos.cluster == 7)

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

table(dailykos.cluster, clusterGroups)

##############
# Section 2
##############
airlines = read.csv("AirlinesCluster.csv")

summary(airlines)

library(caret)
# Normalize data
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)
summary(preproc)

# Hier
distances = dist(airlinesNorm, "euclidean")
hcluster = hclust(distances, "ward.D")
plot(hcluster)
clusterGroups = cutree(hcluster, k=5)
str(clusterGroups)
table(clusterGroups)
# compare means
spl=split(airlines, clusterGroups)
means=lapply(spl, colMeans)

sapply(c(1,2,3,4), function(x) means[[5]]>means[[x]])
means[[5]]

# K-mean clustering 
k=5
set.seed(88)
kmeansClust = kmeans(airlinesNorm, centers = k, iter.max = 1000)
str(kmeansClust)

table(kmeansClust$cluster)

kmeansClust$centers

means
splk=split(airlines, kmeansClust$cluster)
meansk=lapply(splk, colMeans)
means
meansk
means[[1]]
meansk[[1]]

sapply(1:5, function(x) means[[x]]-meansk[[x]])

plot(meansk[[1]])
points(means[[2]], col="red")
str(kmeansClust)
plot(kmeansClust$centers[1,])

table(kmeansClust$cluster, clusterGroups)

#############
# Section 3
#############

stocks = read.csv("StocksCluster.csv")

summary(stocks)

prop.table(table(stocks$PositiveDec > 0))
corr=cor(stocks)

for (i in 1:12){
  corr[i,i] = 0
}
corr
max(corr)

summary(stocks)
str(stocks)
colMeans(stocks[1:11])
max(colMeans(stocks[1:11]))
min(colMeans(stocks[1:11]))

library(caTools)
set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family = "binomial")
summary(StocksModel)

pred = predict(StocksModel, type = "response")

table(stocksTrain$PositiveDec, pred > 0.5)
(990+3640)/nrow(stocksTrain)

predtp = predict(StocksModel, newdata = stocksTest, type = "response")

table(stocksTest$PositiveDec, predtp >=0.5)

head(predtp, 30)
(417+1553)/nrow(stocksTest)

table(stocksTest$PositiveDec)
1897/nrow(stocksTest)

# clustering
# remove dependent variable
limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

# Normalize the data
library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)


mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)
# they are different, because the normalization was based on the mean of the training set.
# test set was normalized subtracting the mean from the training set

# K-means
set.seed(144)
km = kmeans(normTrain, centers = 3)
str(km)
table(km$cluster)

# k-centroids cluster analysis
library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

str(clusterTrain)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

# Which cluster have the highest average odf the dependent variable?
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
# cluster 1 has highest average

# Lets build logistic regression for each cluster
StocksModel1 = glm(PositiveDec ~ ., data=stocksTrain1, family = "binomial")
StocksModel2 = glm(PositiveDec ~ ., data=stocksTrain2, family = "binomial")
StocksModel3 = glm(PositiveDec ~ ., data=stocksTrain3, family = "binomial")
# Which variables have at least 1 postive sign and at least 1 negative sign accross models
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)
str(StocksModel1)
StocksModel1$coefficients
StocksModel2$coefficients
StocksModel3$coefficients

#Predict test sets
PredictTest1 = predict(StocksModel1, newdata=stocksTest1, type="response")
PredictTest2 = predict(StocksModel2, newdata=stocksTest2, type="response")
PredictTest3 = predict(StocksModel3, newdata=stocksTest3, type="response")

t1=table(stocksTest1$PositiveDec, PredictTest1>0.5)
t1
(t1[1,1]+t1[2,2])/nrow(stocksTest1)

t2=table(stocksTest2$PositiveDec, PredictTest2>0.5)
t2
(t2[1,1]+t2[2,2])/nrow(stocksTest2)

t3=table(stocksTest3$PositiveDec, PredictTest3>0.5)
t3
(t3[1,1]+t3[2,2])/nrow(stocksTest3)

# To compute the overall test-set accuracy of the cluster-then-predict approach, we can 
# combine all the test-set predictions into a single vector and all the true outcomes into 
# a single vector:
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)

AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
tc=table(AllOutcomes, AllPredictions>0.5)
tc
(tc[1,1]+tc[2,2])/length(AllOutcomes)

# We see a modest improvement over the original logistic regression model. Since 
# predicting stock returns is a notoriously hard problem, this is a good increase in 
# accuracy. By investing in stocks for which we are more confident that they will 
# have positive returns (by selecting the ones with higher predicted probabilities), 
# this cluster-then-predict model can give us an edge over the original logistic 
# regression model.
