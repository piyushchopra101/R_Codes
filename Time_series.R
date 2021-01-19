temp_series<-X1880_2014

x11()
plot(temp_series)

temp_ts<- ts(temp_series)

x11()
plot.ts(temp_ts)

x11()
acf(temp_series$Value)

x11()
pacf(temp_series$Value)

x11()
acf(diff(temp_series$Value,1))

 ## MA = seasonal,4 -- 
x11()
pacf(diff(temp_series$Value,1))



fit <- arima(temp_series$Value, order=c(0,1,1), seasonal=c(0,1,1))
fit

fit_312100 <- arima(temp_series$Value[1:130], order=c(3,1,2), seasonal=list(order=c(1,0,0),period=18))
fit_312100

################################################################################################
> fit_312100

Call:
  arima(x = temp_series$Value[1:130], order = c(3, 1, 2), seasonal = list(order = c(1, 
                                                                                    0, 0), period = 18))

Coefficients:
  ar1      ar2      ar3     ma1      ma2    sar1
-0.6901  -0.0940  -0.2855  0.4025  -0.3680  0.2408
s.e.   0.2386   0.3012   0.1130  0.2535   0.2868  0.0973

sigma^2 estimated as 0.008233:  log likelihood = 125.71,  aic = -237.43
> 

###############################################################################################

x11()
plot(fit_312001$residuals)


forecast(fit_312100, h=1)

temp_series$Value[131]<-0.5996126
fit_312100 <- arima(temp_series$Value[1:131], order=c(3,1,2), seasonal=list(order=c(1,0,0),period=18))
fit_312100
forecast(fit_312100, h=1)

temp_series$Value[132]<-0.602067
fit_312100 <- arima(temp_series$Value[1:132], order=c(3,1,2), seasonal=list(order=c(1,0,0),period=18))
fit_312100
forecast(fit_312100, h=1)

temp_series$Value[133]<-0.5897178
fit_312100 <- arima(temp_series$Value[1:133], order=c(3,1,2), seasonal=list(order=c(1,0,0),period=18))
fit_312100
forecast(fit_312100, h=1)

temp_series$Value[134]<-0.6363063
fit_312100 <- arima(temp_series$Value[1:134], order=c(3,1,2), seasonal=list(order=c(1,0,0),period=18))
fit_312100
forecast(fit_312100, h=1)


temp_series$Value[135]<-0.5943116


x11()
par(mfrow=c(2,1))
#Ylim<-seq(5.55, 7.55, by=0.05)
plot(temp_series$Value[130:135],type="o",ylim=c(0.55, 0.75))
lines(X1880_2014$Value[130:135],type="o",cex=2,col="red",ylim=c(0.55, 0.75))


result<-data.frame(Actual=X1880_2014$Value[131:135],Predicted=temp_series$Value[131:135])
result$Mape<-abs(result$Actual- result$Predicted)/result$Actual
mean(result$Mape)
[1] 0.08118489

