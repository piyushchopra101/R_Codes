data14=read.csv("hour1.csv",header = T)
attach(data14)
#data14$season=as.factor(data10$season)
#data14$yr=as.factor(data10$yr)
data14$mnth=as.factor(data14$mnth)
data14$hr=as.factor(data14$hr)
#data10$holiday=as.factor(data10$holiday)
data14$weekday=as.factor(data14$weekday)
data14$workingday=as.factor(data14$workingday)
#data10$weathersit=as.factor(data10$weathersit)
trainrows=sample(nrow(data14),0.7*nrow(data14))
datatraining=data14[trainrows,]
datatest=data14[-trainrows,]

# Linear Regression with Log Transformation
model2=lm(log(cnt)~mnth+hr+weekday+workingday+atemp+hum+windspeed,data=datatraining)
summary(model2)

x11()
par(mfrow=c(2,2))
plot(model2)

datatest$mnth=as.factor(datatest$mnth)
datatest$hr=as.factor(datatest$hr)
#datatest$holiday=as.factor(datatest$holiday)
datatest$weekday=as.factor(datatest$weekday)
datatest$workingday=as.factor(datatest$workingday)
#pred=predict(model3,newdata=datatest)


pred<-predict(model2,datatest[-17])

result<-data.frame(predicted=(pred),actual=log(datatest$cnt))
result$bins<- ifelse(abs(result$predicted-result$actual)>2.5,1,0)
result$pred_ex<-(exp(pred))
result$act_ex<-exp(result$actual)

result$diff<- (abs(result$actual-result$predicted))

a<-result[result$actual>0,]
a$mape<-(a$diff/a$actual)
mean(a$mape)

m<-mean(airquality$Temp);
std<-sqrt(var(airquality$Temp))
hist(airquality$Temp,prob=T,main="Temperature")(
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)

x11()
hist(aggregate(hour_final$cnt,by=list(hour_final$weekday),length)$Group.1)

##########################################
# Poisson and quasipoisson
#########################################



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



# Negative binomial distribuition

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


##########################
#Various Data Visualization Plots
##########################

install.packages(c("ggplot2","lubridate","scales","plyr","readr"))

library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

train <- data10

train$season  <- factor(train$season, labels = c("Spring", "Summer", "Fall", "Winter"))
train$weathersit <- factor(train$weathersit, labels = c("Good", "Normal", "Bad", "Very Bad"))
#train$hour    <- factor(hour(ymd_hms(train$datetime)))
train$hour    <- factor(train$hr)
train$mnth    <- factor(train$mnth)
train$date<-format(as.Date(train$dteday,format="%m/%d/%Y"), "%d")
train$times   <- as.POSIXct(strftime(ymd_hms(train$datetime), format="%H:%M:%S"), format="%H:%M:%S")
train$Weekday_1 <- wday(ymd(train$dteday),label=TRUE)

season_summary <- ddply(train,.(season,hour),
                        summarise, count = mean(cnt))

month_summary <- ddply(train,.(mnth),summarise, count = sum(cnt))

hour_summary <- ddply(train,.(mnth,hour),summarise, count = sum(cnt))

date_summary <- ddply(train,.(date),summarise, count = mean(cnt))
holiday_summary <- ddply(train,.(holiday),summarise, count = mean(cnt))
weekend_summary <- ddply(train,.(workingday),summarise, count = mean(cnt))


date_summary$date<-factor(date_summary$date)
x11()
ggplot(date_summary,aes(x=(date),y=(count), group=1,colour=date))+
geom_line()+geom_point()

x11()
plot(date_summary$date,date_summary$count,col="red")

train$holiday<-factor(train$holiday)
x11()
ggplot(holiday_summary,aes(x=(holiday),y=count,fill=factor(holiday))) +  stat_summary(fun.y=mean,geom="bar")+
scale_x_discrete("Holiday 0-NO  1-YES") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Holiday wise Count of bikes \n") + 
  theme(plot.title=element_text(size=18))
x11()

x11()
ggplot(weekend_summary,aes(x=(workingday),y=count,fill=factor(workingday))) +  stat_summary(fun.y=mean,geom="bar")+
  scale_x_discrete("Workingday 0-NO  1-YES") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("Workingday wise Count of bikes \n") + 
  theme(plot.title=element_text(size=18))
x11()
ggplot(data=hour_summary, aes(x=factor(hour), y=count, group=mnth, colour=mnth)) +
  geom_line() +
  geom_point()

x11()
ggplot(train, aes(x = windspeed, y = cnt,col=windspeed)) +
  geom_point(data = train,show.legend = TRUE,) +
  #geom_dotplot(dotsize = 0.1) +
  scale_x_discrete("windspeed") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("windspeed wise Count of bikes \n") + 
  theme(plot.title=element_text(size=18))

weather_summary <- ddply(train,.(weather,hour),
                         summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = weather)) +
  geom_point(data = weather_summary, aes(group = weather)) +
  geom_line(data = weather_summary, aes(group = weather)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes more when the weather is Good.\n") + 
  theme(plot.title=element_text(size=18))


day_summary <- ddply(train,.(Weekday,hour),
                     summarise, count = mean(count))
ggplot(train, aes(x = hour, y = count, colour = Weekday)) +
  geom_point(data = day_summary, aes(group=Weekday)) +
  geom_line(data = day_summary, aes(group=Weekday)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Count") +
  theme_minimal() +
  ggtitle("People rent bikes for morning/evening commutes on weekdays,
          and daytime rides on weekends\n")


weather_prob <- ddply(train,.(season, hour),
                      summarise, Good = mean(weather == "Good"),
                      Normal = mean(weather == "Normal"),
                      Bad = mean(weather == "Bad"),
                      Very_bad = mean(weather == "Very Bad"))


ggplot(train, aes(x = hour, y = Good, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Good") +
  theme_minimal() +
  ggtitle("The probability of Good weather is higher in all. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Normal, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Normal") +
  theme_minimal() +
  ggtitle("The probability of Normal weather is higher in Spring. \n") + 
  theme(plot.title=element_text(size=18))

ggplot(train, aes(x = hour, y = Bad, colour = season)) +
  geom_point(data = weather_prob, aes(group = season)) +
  geom_line(data = weather_prob, aes(group = season)) +
  scale_x_discrete("Hour") +
  scale_y_continuous("Prob of Bad") +
  theme_minimal() +
  ggtitle("The probability of Bad weather is higher in Summer and Winter. \n") + 
  theme(plot.title=element_text(size=18))

