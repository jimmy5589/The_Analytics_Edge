# Exame 2

Hh=read.csv("Households.csv")
summary(Hh)
str(Hh)

table(Hh$MorningPct == 100)

table(Hh$AfternoonPct == 100 )


head(Hh)

Hh[Hh$AvgSalesValue > 150,]
min(Hh[Hh$AvgSalesValue > 150,]$AvgDiscount)

Hh[Hh$AvgDiscount > 25,]
min(Hh[Hh$AvgDiscount > 25,]$AvgSalesValue)

nrow(Hh[Hh$NumVisits >= 300,])/nrow(Hh)


