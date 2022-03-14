y<-ts(trend$y,start = 2000, frequency = 5)
y
## Simple smothing 
install.packages("forecast")
library(forecast)
ma(y,order = 3)

plot(ma(y,order = 3))
autoplot(y)+autolayer(ma(y,order = 3))+autolayer(ma(y,order = 5))

##exponantial smoothing 
ses(y,alpha = 0.5)
summary(ses(y,alpha = 0.5))
summary(ses(y))
plot(ses(y))
ses(y)$fitted
autoplot(y)+autolayer(ses(y)$fitted,series = "y^")+autolayer(ses(y)$fitted, series = 'y^')


#######################ÏÂÎç

plot(Unemp$`The number of unemployed`)
un<-ts(Unemp$`The number of unemployed`, start = 2000, frequency = 1)
un
plot(un)

ses(un)
summary(ses(un))
plot(ses(un))


#Braun models 
ses(un,alpha = 0.4,lambda = 0.6)
summary(ses(un,alpha = 0.4,lambda = 0.6))



#Holt model 
holt(un,level = 0.95, initial = "optimal")
summary(holt(un,level = 0.95, initial = "optimal"))

autoplot(un)+autolayer(ses(un,alpha = 0.4,lambda = 0.6)$fitted,series = "B") + autolayer(holt(un,level = 0.95, initial = "optimal")$fitted,series = "H")








