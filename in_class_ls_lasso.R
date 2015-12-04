#least square lasso ls.lasso
rm(list=ls())
cat("\014")
graphics.off()

Hitters=na.omit(Hitters)
n=dim(Hitters)[1]
p=dim(Hitters)[2]-1

# split the data
set.seed(104)
train=sample(1:n,n/2)
test=(-train)

#
X=model.matrix(Salary~., Hitters)[,-1]
y=Hitters$Salary

#ridge regression
library(glmnet)
grid=10^seq(10,-2,length=100)
set.seed(1)
cv.ridge=cv.glmnet(X[train,],y[train],alpha=0)
par(mfrow=c(1,1))
plot(cv.ridge)
bestlam=cv.ridge$lambda.min
ridge.mod=glmnet(X[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
y.pred.ridge=predict(ridge.mod,s=bestlam,newx=X[test,])
mean((y.pred.ridge-y[test])^2)


#lasso regression
lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
set.seed(1)
cv.lasso=cv.glmnet(X[train,],y[train],alpha=1)
par(mfrow=c(1,1))
plot(cv.lasso)
bestlam=cv.lasso$lambda.min
y.pred.lasso=predict(lasso.mod,s=bestlam,newx=X[test,])
mean((y.pred.lasso-y[test])^2)

#lasso approximate-variable selection
lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=bestlam)
b=lasso.mod$beta
b=as.matrix(b)
ls.lasso=lm(y[train]~X[train,b!=0])
b.ls=b
b.ls[b!=0]=ls.lasso$coefficients[-1]
y.pred.ls.lasso=X[test,]%*%b.ls+ls.lasso$coefficients[1]
mse.app.ls.lasso=mean((y.pred.ls.lasso-y[test])^2)


#lasso for variable selection NOT COMPLETE
grid=10^seq(5,-2,length=10000)
lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
lambda.max=lasso.mod$lambda[which.min(lasso.mod$df==0)]
grid=10^seq(log(lambda.max)/log(10),-2,length=100)
lasso.mod=glmnet(X[train,],y[train],alpha=1,lambda=grid)
mse.ls.lasso=matrix(0,100)
for (i in 1:100){
  b=lasso.mod$beta[,i]
  b=as.matrix(b)
  ls.lasso=lm(y[train]~X[train,b!=0])
  b.ls=b
  b.ls[b!=0]=ls.lasso$coefficients[-1]
  y.pred.ls.lasso=X[test,]%*%b.ls+ls.lasso$coefficients[1]
  mse.ls.lasso[i]=mean((y.pred.ls.lasso-y[test])^2)
}
plot(log(lasso.mod$lambda),mse.ls.lasso,xlab = "log(Lambda)",type = "l" )
m=which.min(mse.ls.lasso)
points(log(lasso.mod$lambda[m]),mse.ls.lasso[m],col="red",cex=2,pch=20)
mse.ls.lasso[m]

















