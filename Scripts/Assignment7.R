
library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
polling = read.csv("PollingImputed.csv")
Train = polling[polling$Year == 2004 | polling$Year == 2008,]
Test = polling[polling$Year == 2012,]

mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")

TestPrediction = predict(mod2, newdata=Test, type="response")

TestPredictionBinary = as.numeric(TestPrediction > 0.5)

predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)

table(TestPredictionBinary)

mean(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]

str(predictionMap)
str(statesMap)

length(table(statesMap$region))

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", name = "Prediction 2012")


Test
TestPrediction[which(Test$State == "Florida")]

############
# Assign 2
############

edges = read.csv("edges.csv")
users = read.csv("users.csv")

str(users)
str(edges)
head(edges)
table(edges$V1, edges$V2)
rsum = rowSums(table(edges$V1, edges$V2))
rsum
csum = colSums(table(edges$V1, edges$V2))
csum
mean(rsum)
mean(csum)
edges[edges$V1==3981,]
edges[edges$V1==4009,]
edges[edges$V2==4009,]
edges[edges$V2==3981,]

rsum
csum
all=c(rsum,csum)
allsum=tapply(unlist(all), names(unlist(all)), sum)
allsum
mean(allsum)
length(allsum)
sum(allsum)/59

# From str(edges) or nrow(edges), we see that there are 146 pairs of users in our dataset 
# who are Facebook friends. However, each pair (A, B) must be counted twice, 
# because B is a friend of A and A is a friend of B. To think of this in simpler terms, 
# consider a network with just new people, A and B, and a single edge (A, B). 
# Even though there are two vertices and one edge, each user has on average one friend.

# For our network, the average number of friends per user is 292/59=4.95.


head(users)
table(users$school, users$locale)
table(users$gender, users$school)
users
