# Reading Test Scores

pisaTrain=read.csv("pisa2009train.csv")
pisaTest=read.csv("pisa2009test.csv")

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

# To remove missing values, delete all observations with missing values
pisaTrain = na.omit(pisaTrain)

pisaTest = na.omit(pisaTest)

# Unordered factors ina linear regression, how to:
# As an example, consider the unordered factor variable "color", 
# with levels "red", "green", and "blue". 
# If "green" were the reference level, then we would add binary variables "colorred" 
# and "colorblue" to a linear regression problem. 
# All red examples would have colorred=1 and colorblue=0. 
# All blue examples would have colorred=0 and colorblue=1. 
# All green examples would have colorred=0 and colorblue=0.

