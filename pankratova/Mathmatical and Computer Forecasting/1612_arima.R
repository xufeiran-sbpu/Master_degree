## ARIMA 
plot(Aeroflot21$RPK)
RPK<-ts(Aeroflot21$RPK, start = 2015, frequency = 12)

RPK
autoplot(RPK)

ar1<-arima(RPK,order = c(1,0,0), include.mean = TRUE)
ar1
# y[t] = 9456.511+08952*y[t-1]

forecast(ar1)
library(ggplot2)
library(forecast)
ar1_fit<-RPK-ar1$residuals
autoplot(RPK)
autoplot(RPK)+autolayer(ar1_fit, series = "AR(1)")+autolayer(forecast(ar1))


checkresiduals(ar1$residuals)

#Ar1 is not good, bad accurance(Mape), bad fitted
#unreal prediction 

# §à§ã§ä§Ñ§ä§Ü§Ú


#AR(2)
ar2<-arima(RPK,order = c(2,0,0), include.mean = TRUE)

summary(ar2)

checkresiduals(ar2$residuals)

#y^
ar2_fit<-RPK-ar2$residuals
autoplot(RPK)+autolayer(ar2_fit, series = "AR(2)")+autolayer(forecast(ar2))

#MR(1)
ma1<-arima(RPK,order = c(0,0,1),include.mean = TRUE)
summary(ma1)
##y[yt] = 9647.109+0.8831*e[t-1]
ma1_fit<-ma1$residuals
autoplot(RPK)+autolayer(ma1_fit, series = "ma(1)")+autolayer(forecast(ma1))


install.packages("tseries")
library(tseries)
acf(RPK)
acf(AirPassengers)
# here we can see the we have trend but it is not obvious that er have seasonal component

# ndiffs (trend integration(d ?))
# nsdiffs (sesonanal integration (D?))



install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

nsdiffs(RPK)
#Caculate the seasonal difference
RPK_seas<- diff(RPK, d=1, lag = 12)
autoplot(RPK_seas)

# Trend integration 
ndiffs(RPK_seas)
#D=1
RPK_seas_tr<-diff(RPK_seas,d=1,lag=1)
RPK_seas_tr
autoplot(RPK_seas_tr)
adf.test(RPK_seas_tr)
#p-value = 0.03925 we have to reject H0, our data set is stationary
acf(RPK_seas_tr)
pacf(RPK_seas_tr)

chart.ACFplus(RPK)
a1<-arima(RPK,order = c(1,1,1), seasonal = list(order = c(1,1,1),period = 12),include.mean = TRUE)
a1
summary(a1)
#residuals
a1_res<-a1$residuals
a1_fit<-RPK-a1_res

autoplot(RPK)+autolayer(a1_fit,series = "ARIMA(1,1,1)(0,1,1)12")+autolayer(forecast((ar1_fit)))
checkresiduals(a1)
shapiro.test(a1$residuals)
# p-value = 8.944e-13 < 0.05 H0 is rejected 




a2<-auto.arima(RPK)
a2
summary(a2)
a2_res<-a2$residuals
a2_fit<-RPK-a2_res
autoplot(RPK)+autolayer(a2_fit,series = "auto_ARIMA(1,1,1)(0,1,1)12")+autolayer(forecast((ar1_fit)))
checkresiduals(a2)
autoplot(RPK)+autolayer(a2_fit,series = "auto_ARIMA")+autolayer(a1_fit,series = "ARIMA")



