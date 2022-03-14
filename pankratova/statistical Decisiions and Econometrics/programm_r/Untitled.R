efr<-BankiRU.for.Mac$EffRate
efr
class(efr)
table(efr)
#interval range
table(cut(efr,breaks = 4))
table(cut(efr,breaks = 7))
k<-ceiling(1+log2(length(efr)))
table(cut(efr,breaks = 6))
#Basic characteristics
#mean
mean(efr)
#m = 4.14
#Variance
var(efr)
#sample stardard diveation 
sd(efr)
#mode
median(efr)
#Q_0 = X_min   Q_1 = 0.25分位数 Q_2 = 中位数 Q_3 = 0.75分位数 Q_4 = X_max
quantile(efr)
summary(efr)
quantile(efr,0.25)

plot(ecdf(efr))

#koef of asymmetry
install.packages("e1071")
library(e1017)
skewness(efr)
#excess
kurtosis((efr))
hist(efr)
boxplot(efr,range = 0)
boxplot(efr,range = 1)

