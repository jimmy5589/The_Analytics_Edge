IBM$Date = as.Date(IBM$Date, "%m/%d/%y")

GE$Date = as.Date(GE$Date, "%m/%d/%y")

CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")

ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-12-01")), lwd=2)
# Problem 3
# See colors options with colors()
plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="purple")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="green")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="black")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-30")), lwd=2)
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-30")), lwd=2)

# Problem 4

tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean) > mean(IBM$StockPrice)
tapply(GE$StockPrice, months(GE$Date), mean) > mean(GE$StockPrice)
tapply(Boeing$StockPrice, months(Boeing$Date), mean) > mean(Boeing$StockPrice)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean) > mean(CocaCola$StockPrice)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean) > mean(ProcterGamble$StockPrice)

tapply(IBM$StockPrice, months(IBM$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean) 
tapply(Boeing$StockPrice, months(Boeing$Date), mean) 
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)

which.max(tapply(GE$StockPrice, months(GE$Date), mean))
which.max(tapply(GE$StockPrice, months(GE$Date), mean))

# Exercise 3
# Problem 1
which.max(table(CPS$Industry))

sort(table(CPS$State))

table(Citizenship)/nrow(CPS)
1-0.05780567
#Problem 1.5
table(Race, Hispanic)>250

# Problem 2
# About Missing values
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))
#2.3
t=table(State, is.na(MetroAreaCode))
table(State, is.na(MetroAreaCode))==0
#2.4
prop.table(table(Region, is.na(MetroAreaCode)),1)
#2.5
prop.table(table(State, is.na(MetroAreaCode)),1)
sort(prop.table(table(State, is.na(MetroAreaCode)),1))
# Correct way for 2.5
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

# Problem 3
MetroAreaMap=read.csv("MetroAreaCodes.csv")
CountryMap=read.csv("CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)

# Problem 3.4 (INTERESTING case)
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))

#Problem 3.5
sort(tapply(CPS$Race=="Asian", CPS$MetroArea, mean))>0.2

#Problem 3.6
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE), decreasing=TRUE)

#Problem 4

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

#Problem 4.3
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")

#Problem 4.4
sort(tapply(CPS$Country=="Somalia", CPS$MetroArea, sum, na.rm=TRUE))

