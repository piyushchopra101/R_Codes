###################
## Creating the datasets
###################
sampa=data.frame(matrix( rnorm(50*5000,mean=0,sd=1), 50,5000) )
sampa$Y<-append(rep(1,25),rep(2,25))

attach(sampa)
Cor_Values<-lapply(sampa[1:5000],function(x) cor(sampa$Y,x))
Cor_Values<-sort(unlist(Cor_Values),decreasing = T)
top100 <- Cor_Values[1:100]

## Wrong Approach
klist<-1
x.train<-sampa[,names(top100)]
y.train<-factor(sampa$Y)
numbers<-1:50

library(chemometrics)

knn_set<-knnEval(x.train,y.train,numbers, kfold = 5, knnvec = seq(1),plotit = F)
knn_set$cvSe
[1] 0.0244949


## Correct Approach
# Prepare the folds

s <- split(sample(n.train),rep(1:nfolds,length=n.train))

fold1<-sampa[-s[[1]],]
fold2<-sampa[-s[[2]],]
fold3<-sampa[-s[[3]],]
fold4<-sampa[-s[[4]],]
fold5<-sampa[-s[[5]],]

# Top 100 Predictors 
Cor_Values_fold1<-lapply(fold1[1:5000],function(x) cor(fold1$Y,x))
Cor_Values_fold2<-lapply(fold2[1:5000],function(x) cor(fold2$Y,x))
Cor_Values_fold3<-lapply(fold3[1:5000],function(x) cor(fold3$Y,x))
Cor_Values_fold4<-lapply(fold4[1:5000],function(x) cor(fold4$Y,x))
Cor_Values_fold5<-lapply(fold5[1:5000],function(x) cor(fold5$Y,x))

Cor_Values_fold1<-sort(unlist(Cor_Values_fold1),decreasing = T)
Cor_Values_fold2<-sort(unlist(Cor_Values_fold2),decreasing = T)
Cor_Values_fold3<-sort(unlist(Cor_Values_fold3),decreasing = T)
Cor_Values_fold4<-sort(unlist(Cor_Values_fold4),decreasing = T)
Cor_Values_fold5<-sort(unlist(Cor_Values_fold5),decreasing = T)

top100_fold1<-Cor_Values_fold1[1:100]
top100_fold2<-Cor_Values_fold2[1:100]
top100_fold3<-Cor_Values_fold3[1:100]
top100_fold4<-Cor_Values_fold4[1:100]
top100_fold5<-Cor_Values_fold5[1:100]

x.train_1<-fold1[,names(top100_fold1)]
x.test_1<-sampa[s[[1]],names(top100_fold1)]
y.train_1<-factor(fold1$Y)


x.train_2<-fold2[,names(top100_fold2)]
x.test_2<-sampa[s[[2]],names(top100_fold2)]
y.train_2<-factor(fold2$Y)

x.train_3<-fold3[,names(top100_fold3)]
x.test_3<-sampa[s[[3]],names(top100_fold3)]
y.train_3<-factor(fold3$Y)

x.train_4<-fold4[,names(top100_fold4)]
x.test_4<-sampa[s[[4]],names(top100_fold4)]
y.train_4<-factor(fold4$Y)

x.train_5<-fold5[,names(top100_fold5)]
x.test_5<-sampa[s[[5]],names(top100_fold5)]
y.train_5<-factor(fold5$Y)

library(class)
knn_test_pred_1 <- knn(train = x.train_1 , test = sampa[s[[1]],names(top100_fold1)],cl = y.train_1, k=1)
knn_test_pred_2 <- knn(train = x.train_2 , test = sampa[s[[2]],names(top100_fold2)],cl = y.train_2, k=1)
knn_test_pred_3 <- knn(train = x.train_3 , test = sampa[s[[3]],names(top100_fold3)],cl = y.train_3, k=1)
knn_test_pred_4 <- knn(train = x.train_4 , test = sampa[s[[4]],names(top100_fold4)],cl = y.train_4, k=1)
knn_test_pred_5 <- knn(train = x.train_5 , test = sampa[s[[5]],names(top100_fold5)],cl = y.train_5, k=1)

mean(knn_test_pred_1== sampa[s[[1]],5001])
mean(knn_test_pred_2== sampa[s[[2]],5001])
mean(knn_test_pred_3== sampa[s[[3]],5001])
mean(knn_test_pred_5== sampa[s[[4]],5001])
mean(knn_test_pred_5== sampa[s[[5]],5001])

## Accuracy
Acc<-mean(mean(knn_test_pred_1== sampa[s[[1]],5001]),mean(knn_test_pred_2== sampa[s[[2]],5001]),mean(knn_test_pred_3== sampa[s[[3]],5001]),mean(knn_test_pred_5== sampa[s[[4]],5001]),mean(knn_test_pred_5== sampa[s[[5]],5001]))

## Error
1-Acc

