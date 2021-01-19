rm(list = ls())
graphics.off()

install.packages("gRbase")
install.packages("RBGL")
install.packages("Rgraphviz")
install.packages("gRain")
install.packages("bnlearn")
install.packages("gRim")
install.packages("RHugin")
install.packages("ggm")
install.packages("bnlearn")
install.packages("Rcpp")
install.packages("e1071")
source("https://bioconductor.org/biocLite.R")
biocLite("RBGL")
biocLite("Rgraphviz")
install.packages("mlr")


library(mlr)
library(dplyr)
library(plyr)
library(gRain)
library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)
library(ggplot2)
library(e1071)
library(bnclassify)

#################################################################
germanData <- read.csv("T:/class docs/Data Mining/project data/german_credit.csv")
gdat<-germanData
names(germanData)
########################################################################
#Pre-processing the Data.
#################################################################

gdat$Creditability[gdat$Creditability==1]<-"Good"
gdat$Creditability[gdat$Creditability==0]<-"Bad"
gdat$Creditability<-as.factor(gdat$Creditability)
gdat$Account.Balance<-mapvalues(gdat$Account.Balance, c(1:4), c("ZeroBal","Lessthan200","GreaterThan200","NoAccount"))
gdat$Account.Balance<-as.factor(gdat$Account.Balance)
gdat$Duration.of.Credit..month<-cut(gdat$Duration.of.Credit..month,breaks=c(0,6,12,18,24,30,36,42,48,54,100),labels=c(10,9,8,7,6,5,4,3,2,1))
gdat$Payment.Status.of.Previous.Credit <-mapvalues(gdat$Payment.Status.of.Previous.Credit, c(0:4), c("no credits/all paid","all paid","existing paid","delayed previously","critical/other existing credit"))
gdat$Payment.Status.of.Previous.Credit<-as.factor(gdat$Payment.Status.of.Previous.Credit)
gdat$Purpose <-mapvalues(gdat$Purpose, c(0:10), c("newCar","usedCar","furniture/equipment","radio/tv","domesticAppliance","repairs","education","vacation","retraining","business","other"  
))
gdat$Purpose<-as.factor(gdat$Purpose)
gdat$Credit.Amount<-cut(gdat$Credit.Amount,breaks=c(0,500,1000,1500,2500,5000,7500,10000,15000,20000,100000),labels=c(10,9,8,7,6,5,4,3,2,1))
gdat[,3]<-NULL

gdat$Value.Savings.Stocks<-mapvalues(gdat$Value.Savings.Stocks,c(1:5),c("LessThan100","LessThan500","Lessthan1000","GreaterThan1000","NoSavings"))
gdat$Value.Savings.Stocks<-as.factor(gdat$Value.Savings.Stocks)

gdat$Length.of.current.employment<-mapvalues(gdat$Length.of.current.employment,c(1:5),c("Unemployed","LessThan_1yr","Lessthan_5yr","lessthan_7yr","Morethan_7yr"))
gdat$Length.of.current.employment<-as.factor(gdat$Length.of.current.employment)

gdat$Guarantors<-mapvalues(gdat$Guarantors,c(1:3),c("none","coApplicant","guarantor"))
gdat$Guarantors<-as.factor(gdat$Guarantors)

gdat$Duration.in.Current.address<-mapvalues(gdat$Duration.in.Current.address,c(1:4),c("LessThan1yr","LessThan4yr","Lessthan7yr","GreaterThan7yr"))
gdat$Duration.in.Current.address<-as.factor(gdat$Duration.in.Current.address)

gdat$Most.valuable.available.asset<-as.factor(gdat$Most.valuable.available.asset)
gdat$No.of.Credits.at.this.Bank<-as.factor(gdat$No.of.Credits.at.this.Bank)
gdat$Occupation<-as.factor(gdat$Occupation)
gdat$No.of.dependents<-as.factor(gdat$No.of.dependents)
gdat$Telephone<-as.factor(gdat$Telephone)
gdat$Foreign.Worker<-as.factor(gdat$Foreign.Worker)
gdat$Instalment.per.cent <-as.factor(gdat$Instalment.per.cent)              
gdat$Sex...Marital.Status<-as.factor(gdat$Sex...Marital.Status)
gdat$No.of.Credits.at.this.Bank<-as.factor(gdat$No.of.Credits.at.this.Bank)

gdat$Age..years.<-cut(gdat$Age..years.,breaks=c(0,25,39,59,64,100),labels=c(1,2,3,5,4))

gdat$Concurrent.Credits<-mapvalues(gdat$Concurrent.Credits,c(1:3),c("Bank","Stores","None"))
gdat$Concurrent.Credits<-as.factor(gdat$Concurrent.Credits)
gdat$Type.of.apartment<-mapvalues(gdat$Type.of.apartment,c(1:3),c("free","OwnerOccupied","Rented"))
gdat$Type.of.apartment<-as.factor(gdat$Type.of.apartment)

gdat1<-gdat[sample(nrow(gdat),900), ]
###########################################
colnames(gdat)<-c("Creditability","AccBal","PaymentStatusPrev.Credit","Purpose","Credit.Amt","SavingsStocks","Length.cur.empl","Instalment.percent","Marital.Status","Guarantors","Curr.address","Most.valuable.asset","Age","Concurrent.Credits","Apt.Type","No.cedits.at.this.Bank","Occupation","dependents","Telephone","Foreign.Worker","Duration.of.Credit..month")
#######################################
names(gdat)
gdat1<-gdat
gdat1$Foreign.Worker<-NULL
gdat1$Telephone<-NULL
summary(mydata)

german.bn <- hc(gdat,score='bic')

german.naive<-naiveBayes(Creditability ~ ., data = gdat)
german.naive<-bnc('nb', 'Creditability',gdat, smooth = 1)

german.naive1<-bnc('tan_cl', 'Creditability',gdat, smooth = 1, dag_args = list(score = 'bic'))
german.naive2<-bnc('tan_hc','Creditability', gdat,dag_args = list( k=10 ,epsilon=0),smooth = 1)
german.naive3<-bnc('tan_hcsp', 'Creditability', gdat, dag_args = list( k=10 ,epsilon=0),smooth = 1)
german.naive4<-bnc('fssj','Creditability', gdat, dag_args = list( k=5 ,epsilon=0),smooth = 1)
german.naive5<-bnc('bsej','Creditability', gdat, dag_args = list( k=5 ,epsilon=0),smooth = 1)
BIC(german.naive5,gdat)
BIC(german.naive4,gdat)
BIC(german.naive3,gdat)
BIC(german.naive2,gdat)
BIC(german.naive1,gdat)
BIC(german.naive,gdat)
AIC(german.naive,gdat)
AIC(german.naive1,gdat)
AIC(german.naive2,gdat)
AIC(german.naive3,gdat)
AIC(german.naive4,gdat)
AIC(german.naive5,gdat)
logLik()

gdat1<-sample_n(gdat, 2000)



############################
# Trying other methods.
###########################

german.tabu<-aracne(gdat)
german.iamb<-iamb(gdat)
german.fast<-fast.iamb(gdat)
german.inter<-inter.iamb(gdat)
german.gr<-compile(grain(gdat))
#################################################################
# Hand-crafted model.
#################################################################
german.hc<-hc(gdat)

german.hc<-drop.arc(german.hc,"Type.of.apartment","Creditability")
german.hc<-drop.arc(german.hc,"Telephone","Credit.Amount")
german.hc<-drop.arc(german.hc,"Length.of.current.employment","Guarantors")
german.hc<-reverse.arc(german.hc,"Length.of.current.employment","Age..years.")
german.hc<-reverse.arc(german.hc,"Account.Balance","Creditability")
german.hc<-reverse.arc(german.hc,"Payment.Status.of.Previous.Credit","Creditability")
german.hc<-reverse.arc(german.hc,"Value.Savings.Stocks","Creditability")

german.hc<-set.arc(german.hc,"Credit.Amount","Creditability")
german.hc<-set.arc(german.hc,"Purpose","Credit.Amount")
german.hc<-set.arc(german.hc,"Age..years.","Creditability")
german.hc<-set.arc(german.hc,"Credit.Amount","Instalment.per.cent")
german.hc<-set.arc(german.hc,"Length.of.current.employment","Guarantors")
german.hc<-set.arc(german.hc,"Guarantors","Creditability")
german.hc<-set.arc(german.hc,"Foreign.Worker","Creditability")
german.hc<-drop.arc(german.hc,"Type.of.apartment","Sex...Marital.Status")
german.hc<-set.arc(german.hc,"Age..years.","Sex...Marital.Status")

german.fit<-bn.fit(german.hc,gdat)

pred<-predict(german.fit,"Creditability",gdat)

######################################################
#accuracy()
#############################
error0<-as.numeric(predict(german.naive, gdat))-as.numeric(gdat$Creditability)
error1<-as.numeric(predict(german.naive1, sim1))-as.numeric(sim1$Creditability)
error2<-as.numeric(predict(german.naive2, sim1))-as.numeric(sim1$Creditability)
error3<-as.numeric(predict(german.naive3, sim1))-as.numeric(sim1$Creditability)
error4<-as.numeric(predict(german.naive4, sim1))-as.numeric(sim1$Creditability)
error5<-as.numeric(predict(german.naive5, sim1))-as.numeric(sim1$Creditability)
error6<-as.numeric(pred)-as.numeric(gdat$Creditability)

accuracy(predict(german.naive, gdat),gdat$Creditability)
accuracy(predict(german.naive1, gdat),gdat$Creditability)
accuracy(predict(german.naive2, gdat),gdat$Creditability)
accuracy(predict(german.naive3, gdat),gdat$Creditability)
accuracy(predict(german.naive4, gdat),gdat$Creditability)
accuracy(predict(german.naive5, gdat),gdat$Creditability)
accuracy(pred,gdat$Creditability)

table(error0)
table(error2)
table(error3)
table(error4)
table(error5)
table(error6)
#######################################################

x11()
graphviz.plot(german.bn,layout="dot",shape="ellipse")

x11()
plot(german.naive1, main="tan_cl" ,layoutType = 'dot', fontsize = 70)

x11()
plot(german.naive2, main="tan_hc" ,layoutType = 'dot', fontsize = 100)

x11()
plot(german.naive3, main="tan_hcsp",layoutType = 'dot', fontsize = 100)

x11()
plot(german.naive4, main="fssj",layoutType = 'dot', fontsize = 70)
x11()
plot(german.naive5,main="bsej", radius=100,layoutType = 'dot', fontsize = 80)

graphvizCapabilities()$layoutTypes

#######################################################
graphviz.plot(german.naive1,layout="dot",shape="ellipse")
b<-as_grain(german.naive1)
a<-as.bn(b)
c<-bn.fit(a,gdat)
#########################################################
x11()
graphviz.plot(german.naive,layout="dot",shape="ellipse")

graphviz.plot(c,layout="dot",shape="ellipse")
x11()
graphviz.plot(german.hc,highlight =list( nodes="Creditability",fill="green"),layout="dot",shape="ellipse")
x11()
graphviz.plot(g,layout="dot",shape="ellipse")
x11()
graphviz.plot(german.fast,layout="dot",shape="ellipse")
x11()
graphviz.plot(german.inter,layout="dot",shape="ellipse")
x11()
graphviz.plot(german.iamb,layout="dot",shape="ellipse")


######################################################
#Model for comparison
#####################################################
install.packages("randomForest")

library(randomForest)
rf50 <- randomForest(Creditability ~., data = gdat, ntree=200, importance=T, proximity=T)
plot(rf50, main="")
rf50
Test50_rf_pred <- predict(rf50, gdat, type="class")
table(Test50_rf_pred, gdat$Creditability)
importance(rf50)
varImpPlot(rf50,  main="", cex=0.8)
#########################################################


