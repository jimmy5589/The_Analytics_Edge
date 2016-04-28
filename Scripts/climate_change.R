climate=read.csv("climate_change.csv")

training=subset(climate, climate$Year <= 2006)

testing=subset(climate, climate$Year > 2006)

model1=lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=training)

model2=lm(Temp ~ MEI + TSI + Aerosols +N2O, data=training)

modelS=step(model1)
# step function does not address the collinearity of the variables
# adding highly correlated variables will not improve the R2 significantly. 
# The consequence of this is that the step function will not necessarily produce 
# a very interpretable model - just a model that has balanced quality and simplicity 
# for a particular weighting of quality and simplicity (AIC).

predictionForS=predict(modelS, newdata=testing)
SSE=sum((predictionForS-testing$Temp)^2)
SST=sum((mean(training$Temp)-testing$Temp)^2)
R2=1-(SSE/SST)
# predictionForS is prediction Points for Model S (created by step)


