rm(list=ls())
cat("\014")
graphics.off()
Hitters=na.omit(Hitters)
n=dim(Hitters)[1]
p=dim(Hitters)[2]-1
library(leaps)
line.best.subset= regsubsets(Salary ~., data=Hitters, nvmax=p)
summary.best.subset=summary(line.best.subset)

plot(summary.best.subset$cp, xlab="Number of Variables", ylab="cp",type="l")
m=which.min(summary.best.subset$cp)
points(m,summary.best.subset$cp[m],col="red",cex=2,pch=20)


#ridge regression
X=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary
library(glmnet)
grid=10^seq(10,-2,length=100)
set.seed(1)
train=sample(1:n,n*0.8)
test=(-train)
cv.ridge=cv.glmnet(X[train,], y[train],alpha=0) #alpha=0 means ridge
plot(cv.ridge)
ridge.mod=glmnet(X[train,], y[train],alpha=0,lambda=cv.ridge$lambda.min)
y.pred.ridge=predict(ridge.mod,s=cv.ridge$lambda.min,newx=X[test,])
mean((y.pred.ridge-y[test])^2)

#lasso regression
cv.lasso=cv.glmnet(X[train,], y[train],alpha=1) #alpha=1 means lasso
plot(cv.lasso)
lasso.mod=glmnet(X[train,], y[train],alpha=1,lambda=cv.lasso$lambda.min)
y.pred.lasso=predict(lasso.mod,s=cv.lasso$lambda.min,newx=X[test,])
mean((y.pred.lasso-y[test])^2)

#regression trees
Hitters.train=Hitters[train,]
Hitters.test=Hitters[test,]
library(tree)
tree.hitters=tree(Salary ~., data= Hitters.train)
summary(tree.hitters)
plot(tree.hitters)
text(tree.hitters,pretty = 0)
y.pred.tree=predict(tree.hitters, Hitters.test)
mean((y.pred.tree-Hitters.test$Salary)^2)

#cross validation regression trees
set.seed(1)
cv.hitters=cv.tree(tree.hitters)
plot(cv.hitters$size,cv.hitters$dev,type="b")
m=which.min(cv.hitters$dev)
points(cv.hitters$size[m],cv.hitters$dev[m],col="red",cex=2,pch=20)

#regression pruned trees
pruned.hitters = prune.tree(tree.hitters, best=cv.hitters$size[m])
plot(pruned.hitters)
text(pruned.hitters,pretty = 0)
y.pred.pruned.tree=predict(pruned.hitters, Hitters.test)
mean((y.pred.pruned.tree-Hitters.test$Salary)^2)

# bagging 
library(randomForest)
set.seed(1)
bag.hitters=randomForest(Salary ~., data=Hitters.train, mtry=p, importance=TRUE)
y.pred.bag=predict(bag.hitters, Hitters.test)
mean((Hitters.test$Salary -y.pred.bag )^2)

# random forest
set.seed(1)
rf.hitters=randomForest(Salary ~., data=Hitters.train, mtry=5, importance=TRUE)
y.pred.rf=predict(rf.hitters, Hitters.test)
mean((Hitters.test$Salary -y.pred.rf )^2)
importance(rf.hitters)












































