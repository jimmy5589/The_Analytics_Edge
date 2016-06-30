# Exame 2

Hh=read.csv("Households.csv")
summary(Hh)
str(Hh)

table(Hh$MorningPct > 0 & Hh$AfternoonPct == 0 )

table(Hh$MorningPct == 0 & Hh$AfternoonPct > 0 )

nrow(Hh[Hh$MorningPct > 0 & Hh$AfternoonPct == 0,] )
nrow(Hh[Hh$MorningPct == 0 & Hh$AfternoonPct > 0,] )

head(Hh)


nrow(Hh[Hh$AfternoonPct == 0,])
nrow(Hh[Hh$MorningPct == 0,])
