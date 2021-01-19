attach(data_1)
cong_chic<- ts(data_1$Total.Chic, frequency = 12,start=c(2002,1))
cong_nyc <- ts(data_1$Total.NYC, frequency = 12,start=c(2002,1))
cong_miami<- ts(data_1$Total.Miami, frequency = 12,start=c(2002,1))
cong_las<- ts(data_1$Total.Las, frequency = 12,start=c(2002,1))

plot(cong_chic,type="o", xlab="years",ylab="passengers",main="chicago Airport")
plot(cong_nyc,type="o", xlab="years",ylab="passengers",main="NYC Airport")
plot(cong_miami,type="o", xlab="years",ylab="passengers",main="Miami Airport")
plot(cong_las,type="o", xlab="years",ylab="passengers",main="Las Vegas Airport")

c_las<- decompose(cong_las)
c_miami<- decompose(cong_miami)
c_nyc<-decompose(cong_nyc)
c_chic<-decompose(cong_chic)

plot(c_las)
plot(c_miami)
plot(c_nyc)
plot(c_chic)

s_adj_l <- cong_las - c_las$seasonal
s_adj_c <- cong_chic - c_chic$seasonal
s_adj_n <- cong_nyc - c_nyc$seasonal
s_adj_m <- cong_miami - c_miami$seasonal

plot(s_adj_l)
plot(s_adj_c)
plot(s_adj_n)
plot(s_adj_m)


acf(cong_chic, 150)
pacf(cong_chic, 50)
acf(cong_nyc, 150)
pacf(cong_nyc, 50)
acf(cong_miami, 150)
pacf(cong_miami, 50)
acf(cong_las, 150)
pacf(cong_las, 50)

require(tseries) 
adf.test(cong_chic,k=0)
adf.test(cong_nyc,k=0)
adf.test(cong_miami,k=0)
adf.test(cong_las,k=0)

Box.test (cong_chic, lag = 1, type = "Ljung")
Box.test (cong_nyc, lag = 1, type = "Ljung")
Box.test (cong_miami, lag = 1, type = "Ljung")
Box.test (cong_las, lag = 1, type = "Ljung")

require(forecast)

S_1<-auto.arima(cong_chic,test = "kpss",ic="bic")
S_1
S_nyc<-auto.arima(cong_nyc,test = "kpss",ic="bic")
S_miami<-auto.arima(cong_miami,test = "kpss",ic="bic")
S_las<-auto.arima(cong_las,test = "kpss",ic="bic")
S_nyc
S_miami
S_las 


tsdiag(S_1,1)
tsdiag(S_nyc,1)
tsdiag(S_miami,1)
tsdiag(S_las,1)
require(astsa)
s_c<-sarima(cong_chic,2,1,0,1,1,2,12)
S_n<-sarima(cong_nyc,0,1,1,0,1,1,12)
s_m<-sarima(cong_miami,1,1,1,0,0,2,12)
s_l<-sarima(cong_las,1,1,0,2,1,1,12)
p1<-forecast(S_1,36)
p2<-forecast(S_nyc,36)
p3<-forecast(S_miami,36)
p4<-forecast(S_las,36)

p1
p2
p3
p4

plot.forecast(p1)
plot.forecast(p2)
plot.forecast(p3)
plot.forecast(p4)

test_nyc <- window(cong_nyc, start=c(2013, 3))
cong_chic <- window(cong_nyc, end=c(2013, 2))

models <- list(mod_arima = auto.arima(cong_nyc, ic='aicc', stepwise=FALSE),
  mod_exp = ets(cong_nyc, ic='aicc', restrict=FALSE),
  mod_neural = nnetar(cong_nyc, p=12, size=25),
  mod_tbats = tbats(cong_nyc, ic='aicc', seasonal.periods=12),
  mod_bats = bats(cong_nyc, ic='aicc', seasonal.periods=12),
  mod_stl = stlm(cong_nyc, s.window=12, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(cong_nyc))
fore_nyc <- lapply(models, forecast, 12)
fore_nyc$naive <- naive(cong_nyc, 12)
op <- par(oma=c(1,1,1,1))
par(op)

par(mfrow=c(4, 2))

for(f in fore_nyc){
  plot(f)
  lines(test_nyc, col='red')
  }

acc <- lapply(fore_nyc, function(f){
  accuracy(f, test_nyc)[2,,drop=FALSE]
})
acc <- Reduce(rbind, acc)
row.names(acc) <- names(fore_nyc)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)



