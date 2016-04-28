# Reading Test Scores

pisaTrain=read.csv("pisa2009train.csv")
pisaTest=read.csv("pisa2009test.csv")

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# To remove missing values, delete all observations with missing values
pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

# Unordered factors in linear regression, how to:
# As an example, consider the unordered factor variable "color", 
# with levels "red", "green", and "blue". 
# If "green" were the reference level, then we would add binary variables "colorred" 
# and "colorblue" to a linear regression problem. 
# All red examples would have colorred=1 and colorblue=0. 
# All blue examples would have colorred=0 and colorblue=1. 
# All green examples would have colorred=0 and colorblue=0.

#relevel to the most common

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

# Create a model based in all independent variables

lmScore= lm(readingScore ~ ., data=pisaTrain)
summary(lmSquare)

# Note that this R-squared is lower than the ones for the models we saw in the lectures
# and recitation. This does not necessarily imply that the model is of poor quality. 
# More often than not, it simply means that the prediction problem at hand 
# (predicting a student's test score based on demographic and school-related variables) 
# is more difficult than other prediction problems (like predicting a team's number of wins 
# from their runs scored and allowed, or predicting the quality of wine from weather 
# conditions).

RMSE=sqrt(mean(lmScore$residuals^2))


predTest=predict(lmScore, newdata=pisaTest)

# Diff between Max and Min scores
max(predTest)-min(predTest)

#Problem 4.3
SSE=sum((predTest-pisaTest$readingScore)^2)
RMSE=sqrt(SSE/nrow(pisaTest))

#Problem 4.3
# Predicted Test Score used in baseline model.
PredictedTestScore=mean(pisaTrain$readingScore)

SST=sum((PredictedTestScore-pisaTest$readingScore)^2)

