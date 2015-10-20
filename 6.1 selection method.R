library(ISLR)
hit <- na.omit(Hitters)

# regsubsets() performs best subset selection by identifying the the best model 
# that contains a given number of predictors, where best is quantified using RSS
library(leaps)
regfit <- regsubsets(Salary~., data=hit,nvmax=19)
reg <- summary(regfit)

##ploting to find the best subset selection
par(mfrow=c(2,2))
plot(reg$rss, xlab = "number of variables", ylab="RSS",type = "l")
plot(reg$adjr2, xlab="number of variables", ylab = "Adjusted R square", type = "l")

which.max(reg$adjr2)
points(11,reg$adjr2[11],col="red",cex=2,pch=20)


plot(reg$cp, xlab = "number of variables", ylab="Cp",type = "l")
which.min(reg$cp)
points(10,reg$cp[10],col="red",cex=2,pch=20)

plot(reg$bic, xlab="number of variables", ylab = "BIC", type = "l")
which.min(reg$bic)
points(6,reg$bic[6] ,col="red",cex=2,pch=20)

##
plot(regfit, scale="r2")
plot(regfit, scale="adjr2")
plot(regfit, scale="Cp")
plot(regfit, scale="bic")

coef(regfit,6)

#==================forward and backward stepwise selection=======================
regfit_fwd <- regsubsets(Salary~., data=hit, nvmax=19, method = "forward")
summary(regfit_fwd)
regfit_bwd <- regsubsets(Salary~., data=hit, nvmax=19, method = "backward")
summary(regfit_bwd)

#===choosing among models using thevalidation set approach and cross-validation====
set.seed(1)
train <- sample(c(T,F),nrow(hit), replace = T)
test =(!train)

regfit_best <- regsubsets(Salary~., data=hit[train,],nvmax=19)
# we use model.matrix() function to build an "X" matrix from the dataset. 
test_mat <- model.matrix(Salary~., data=hit[test,])


val_error <- rep(NA,19)
for( i in 1:19) {
    coef_i <- coef(regfit_best, id=i)
    pred <- test_mat[, names(coef_i)]%*%coef_i
    val_error[i] <- mean((hit$Salary[test]-pred)^2)
}
val_error
a <-which.min(val_error)
coef(regfit_best,a)

# cross validation
k=10
set.seed(1)
# we create vector that allocates each observation to one of k=10 folds, and we 
# create a matrix in which we will store the results. 
folds <- sample(1:k, nrow(hit), replace = T)
cv_error <- matrix(NA,k,19, dimnames = list(NULL,paste(1:19)))
test_mat <- model.matrix(Salary~., data=hit[test,])

for(j in 1:k){
    best_fit <- regsubsets(Salary~., data=hit[folds!=j,], nvmax = 19)
    for(i in 1:19){
     test_mat <- model.matrix(Salary~., data=hit[folds==j,])
     coef_i <- coef(best_fit, id=i)
     pred <- test_mat[, names(coef_i)]%*%coef_i
     #pred <- predict(best_fit, hit[folds==j,],id=i)
     cv_error[j,i] <- mean((hit$Salary[folds==j]-pred)^2)
    }
}

mean_cv_error <- apply(cv_error,2,mean)
par(mfrow=c(1,1)) 
plot(mean_cv_error, type="b")
a<-which.min(mean_cv_error)
reg_best <- regsubsets(Salary~., data=hit, nvmax=19)
coef(reg_best,a)




