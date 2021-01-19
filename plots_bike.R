m<-mean(airquality$Temp);
std<-sqrt(var(airquality$Temp))
hist(airquality$Temp,prob=T,main="Temperature")(
curve(dnorm(x, mean=m, sd=std), col="darkblue", lwd=2, add=TRUE)

x11()
hist(aggregate(hour_final$cnt,by=list(hour_final$weekday),length)$Group.1)

install.packages(c("ggplot2","lubridate","scales","plyr","readr"))

library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

train <- hour_final

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
