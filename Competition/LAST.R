####
summary(trn)
summary(train)
summary(tst)
table(trn$Q109244)
table(trn$Q115611)
#
# No   None  Yes 
# 1864 1624  688 

1864/(1864+688)
# 0.7304075  0.2695925
# split of 1186 No + 438 Yes

trn2=subset(trn, trn$Q109244 == "No" | trn$Q109244 == "Yes")
tst2=subset(tst, tst$Q109244 == "No" | tst$Q109244 == "Yes")

table(trn2$Q109244)

############
set.seed(144)
spl=sample.split(trainIN$Democrat, SplitRatio = 0.75)
trnIN=subset(trainIN, spl==TRUE)
tstIN=subset(trainIN, spl==FALSE)

set.seed(144)

model3=glm(Democrat ~ 
             Q115611 + Q109244 +
             Q98197 
           , data=trnIN, family=binomial)
# Q101596, Q113181, Q116953, Q118232, HouseholdStatus
summary(model3)
predictTst3 = predict(model3, newdata=tstIN, type="response")

t=table(tstIN$Democrat, predictTst3 > 0.50 )
t
accuracy=(t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
accuracy

# 0.5553161
# 0.5596264  removed Q101596, Q113181
# 0.5833333  removed above + Q116953
#              Q118232
# 0.6185345

