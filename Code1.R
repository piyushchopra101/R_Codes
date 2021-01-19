#poisson and quasipoisson


data10=read.csv("hour1.csv",header=T)
attach(data10)
#data10$season=as.factor(data10$season)
#data10$yr=as.factor(data10$yr)
data10$mnth=as.factor(data10$mnth)
data10$hr=as.factor(data10$hr)
#data10$holiday=as.factor(data10$holiday)
data10$weekday=as.factor(data10$weekday)
data10$workingday=as.factor(data10$workingday)
#data10$weathersit=as.factor(data10$weathersit)
trainrows=sample(nrow(data10),0.7*nrow(data10))
datatraining=data10[trainrows,]
datatest=data10[-trainrows,]
#datatraining$season=as.factor(datatraining$season)
#datatraining$yr=as.factor(datatraining$yr)
datatraining$mnth=as.factor(datatraining$mnth)
datatraining$hr=as.factor(datatraining$hr)
#datatraining$holiday=as.factor(datatraining$holiday)
datatraining$weekday=as.factor(datatraining$weekday)
datatraining$workingday=as.factor(datatraining$workingday)
model9=glm(cnt~mnth+hr+workingday+weekday+atemp+hum+windspeed,family=quasipoisson,data = datatraining)
summary(model9)
AER::dispersiontest(model9)
#AER::dispersiontest(model8,trafo = 2)
x11()
par(mfrow=c(2,2))
plot(model9)



#Negative binomial distribuition
trainrows=sample(nrow(data10),0.7*nrow(data10))
datatraining=data10[trainrows,]
datatest=data10[-trainrows,]
#datatraining$season=as.factor(datatraining$season)
#datatraining$yr=as.factor(datatraining$yr)
datatraining$mnth=as.factor(datatraining$mnth)
datatraining$hr=as.factor(datatraining$hr)
#datatraining$holiday=as.factor(datatraining$holiday)
datatraining$weekday=as.factor(datatraining$weekday)
datatraining$workingday=as.factor(datatraining$workingday)
#datatraining$weathersit=as.factor(datatraining$weathersit)
library(MASS)
model11=glm.nb(cnt~mnth+hr+workingday+weekday+atemp+windspeed+hum,data=datatraining)
summary(model11)

datatest$mnth=as.factor(datatest$mnth)
datatest$hr=as.factor(datatest$hr)

datatest$weekday=as.factor(datatest$weekday)
datatest$workingday=as.factor(datatest$workingday)

pred<-predict(model11,datatest[-17])

result<-data.frame(predicted=(pred),actual=log(datatest$cnt))
result$bins<- ifelse(abs(result$predicted-result$actual)>2.5,1,0)
result$pred_ex<-(exp(pred))
result$act_ex<-exp(result$actual)

result$diff<- (abs(result$actual-result$predicted))

a<-result[result$actual>0,]
a$mape<-(a$diff/a$actual)
mean(a$mape)
table(result$bins)
#exp(3)
x11()
par(mfrow=c(2,2))
plot(model11)