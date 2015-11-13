rm(list=ls())
cat("\014")
library(ISLR)
library(MASS)
library(class)
library(ggplot2)

mydata=College
n = dim(mydata)[1]
p = 17
X = mydata[,2:18]
y = mydata[,1]

k_fold = 10
n_test = floor(n/k_fold)
n_train = n - n_test

knn_values=c(1,2,3,4,5,6,7,8,9,10,20,30,40,50,60,70,80,90,100,110,120,140,180,200)
Knn=length(knn_values)
knn.error=matrix(0,Knn)
lda.error=0
qda.error=0
logistic.error=0

all_indices=1:n
for (j in 1:k_fold){
  test_indices=(1+(j-1)*n_test):(j*n_test)
  train_indices=all_indices[-test_indices]  
  y_train=y[train_indices]
  X_train=X[train_indices,]
  y_test=y[test_indices]
  X_test=X[test_indices,]
  i=0
  for (ki in knn_values){
    i=i+1
    y_test_predicted=knn(X_train, X_test, y_train, k=ki)
    knn.error[i]=knn.error[i] + sum(y_test != y_test_predicted)/(n_test*k_fold)
  }
  
  data_test=mydata[test_indices,-1]
  data_train=mydata[train_indices,]
  
  logistic.fit=glm(Private ~., data_train, family = binomial)
  logistic_test_prob = predict(logistic.fit, data_test, "response")
  logistic_test_predicted = rep("No", n_test)
  logistic_test_predicted[logistic_test_prob > 0.5] = "Yes"
  logistic.error=logistic.error + sum(y_test != logistic_test_predicted)/(n_test*k_fold)
  
  lda.fit = lda(Private ~., data_train)
  lda_test_predicted=predict(lda.fit, data_test)
  lda.error=lda.error + sum(y_test != lda_test_predicted$class)/(n_test*k_fold)
  
  qda.fit = qda(Private ~., data_train)
  qda_test_predicted=predict(qda.fit, data_test)
  qda.error=qda.error + sum(y_test != qda_test_predicted$class)/(n_test*k_fold)
}

performance = data.frame(knn.error, knn_values)
library(reshape2)
performance=melt(performance, id="knn_values")
colnames(performance)=c("k","experiment","error")
fig=ggplot(performance, aes(x=k,y=error, colour=experiment, lintype=experiment)) + geom_line()





