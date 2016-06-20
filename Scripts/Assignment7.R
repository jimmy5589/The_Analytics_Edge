
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

####

install.packages("igraph")
library(igraph)

g=graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

d=degree(g)

table(d>=10)

V(g)$size = degree(g)/2+2

plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

V(g)$color = "black"

V(g)$color[V(g)$gender == "A"] = "red"

V(g)$color[V(g)$gender == "B"] = "gray"

plot(g, vertex.label=NA)

V(g)$color = "black"

V(g)$color[V(g)$school == "A"] = "blue"

V(g)$color[V(g)$school == "AB"] = "green"

plot(g, vertex.label=NA)


V(g)$color = "black"

V(g)$color[V(g)$locale == "A"] = "blue"

V(g)$color[V(g)$locale == "B"] = "green"

plot(g, vertex.label=NA, edge.width=1)


?igraph.plotting

# The three functions to plot the igraph are 
# plot.igraph (the function we used through the command "plot"), tkplot, and rglplot. 
# rglplot makes 3-D plots -- you can try one with rglplot(g, vertex.label=NA). 
# Once you've made the plot, you can click and drag to rotate the graph. 
# To use this function, you will need to install and load the "rgl" package.

# To change the edge width, you need to change the edge parameter called "width". 
# From ?igraph.plotting, we read that we need to append the prefix "edge." to the beginning 
# for our call to plot, so the full parameter is called "edge.width". For instance, 
# we could plot with edge width 2 with the command plot(g, edge.width=2, vertex.label=NA).

install.packages("rgl")
library(rgl)

rglplot(g, vertex.label=NA)

# Last Exercise
tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

library(tm)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

install.packages("wordcloud")
library(wordcloud)

colnames(allTweets)
sums=colSums(allTweets)
max(colSums(allTweets))

which(sums == 1297)
# it's apple
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))

# removing the word "apple"
corpus = tm_map(corpus, removeWords, c("apple",stopwords("english")))
frequencies = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))

wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))
?wordcloud

negativeTweets = subset(allTweets, tweets$Avg <= -1)

wordcloud(colnames(negativeTweets), colSums(negativeTweets), colors="blue")

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), random.order = FALSE)
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), rot.per = 0.1)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), random.color=TRUE)

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), random.color=FALSE)

##
install.packages("RColorBrewer")
library(RColorBrewer)

# The function brewer.pal() returns color palettes from the ColorBrewer project 
# when provided with appropriate parameters, 
# and the function display.brewer.all() displays the palettes we can choose from.
display.brewer.pal(8,"Accent")
display.brewer.pal(8,"Set2")
display.brewer.pal(8,"PuBuGn")
display.brewer.pal(8,"Greys")

display.brewer.all()

wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), colors=brewer.pal(9,"Blues"))

brewer.pal(9,"Blues")
brewer.pal(9,"Blues")[c(5,6,7,8,9)]

# removing first 4 elements from the palette
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25), colors=brewer.pal(9,"Blues")[c(-1,-2,-3,-4)])

# Same same, remoce the first 4
brewer.pal(9,"Blues")[c(-1,-2,-3,-4)]
brewer.pal(9,"Blues")[c(5,6,7,8,9)]

c(1,2,3,4,5,6,7,8)[c(-1,-2)]
