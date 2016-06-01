
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


