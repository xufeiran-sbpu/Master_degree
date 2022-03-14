efr<-BankiRU.for.Mac$EffRate
efr
class(efr) 
table(efr)
#interval range
table(cut(efr,breaks = 4))
table(cut(efr,breaks = 7))
k<-ceiling(1+log2(length(efr)))
k
table(cut(efr,breaks = 6))
#Basic characteristics
#mean
mean(efr)
#m=4.14
#Variance
var(efr)
#0.36
#Sample stardard diveation
sd(efr)
#0.6
#median
median(efr)
quantile(efr)



summary(efr)
quantile(efr, 0.25)



plot(ecdf(efr))



#koef of asymmetry
install.packages('e1071')
library(e1071)
skewness(efr)
#A=0.22
#excess
kurtosis(efr)



hist(efr)



boxplot(efr,range = 0)
boxplot(efr,range = 1.5)



# 23/09/2021
#Confidance interval

#Confidance interval for average 
#value a with sigma is unknown and confidance level =0.95
t.test(efr, conf.level = 0.95, alternative = "two.sided")
#(3.896;7.39)
#Confidance interval for average 
#value a with sigma is unknown and confidance level =0.9
t.test(efr, conf.level = 0.9, alternative = "two.sided")
#(3.938;4.346)



#Confidance interval for average 
#value a with sigma is known (sigma=0.5) and confidance level =0.95



install.packages('BSDA')
library(BSDA)
z.test(efr, sigma.x = 0.5,conf.level = 0.95,alternative = "t")
#(3.946;4.338)



#Confidance interval for variance sigma^2



install.packages('EnvStats')
library(EnvStats)
varTest(efr, conf.level = 0.95, alternative = 't')
#(0.217;0.69)

#Ural and Central price > 55000 in Jume
JetFuelPrices

t_UC<-subset(JetFuelPrices,JetFuelPrices$FedDistrict=='Central'| JetFuelPrices$FedDistrict=='Ural')
t_UC
t_UC_J<-t_UC$June
t_UC_J
length(t_UC_J)

t_55<-subset(t_UC_J,t_UC_J>=55000)
t_55
#小量样本
binom.test(length(t_55),length(t_UC_J),conf.level = 0.95,alternative = "t")$conf.int
#大量样本
prop.test(length(t_55),length(t_UC_J))$conf.int


efr
#Pearson test
install.packages('nortest')
library(nortest)
# H0: normal distribution efr, Ha not normal distribution
pearson.test(efr)
#p-calue = 0.6264 >0.05 there are no reasons to reject H0, efr has noormal distribution 

pearson.test(efr,n.classes = 5)
#p-calue = 0.6264 >0.05 there are no reasons to reject H0, efr has noormal distribution 

#Kolmoogorov test
ks.test(efr,"pnorm",alternative = 't')
# p-value < 2.2e-16 < 0.05    : we have to reject H0

#Shapiro-wilk 
shapiro.test(efr)
#p-value = 0.7146 > 0.05   : there are no reasons to reject H0, efr has noormal distribution 



