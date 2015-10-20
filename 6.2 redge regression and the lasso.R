library(ISLR)
hit <- na.omit(Hitters)
# the model.matrix() is particularly useful because not only does it produce a matrix corresponding
# to the 19 predictors, but it also automatically transforms any qualitative varialbs into 
# dummy variables. 
x <- model.matrix(Salary~., hit)[,-1]
y <- hit$Salary


library(glmnet)
grid <- 10^seq(10,-2,length=100)
ridge_mod <- glmnet(x,y,alpha = 0, lambda = grid)
plot(ridge_mod)
# associated with each value of lamda is a vector of redge regressoin coefficients,
# stored in a matrix that can ba accessed by coef(). in this case, we have 100 lamda
# and we have 20 variables.
coe <- coef(ridge_mod)
dim(coe)
predict(ridge_mod, s=50, type = "coefficients")[1:20,] # lamda=50,

## split a sample into a training set and a test set
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y_test <- y[test]

# we fit a ridge regression model on the training set, and evaluate its MSE on the
# test set, using lamda=4. 
ridge_mod <- glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh=1e-12)
ridge_pred <- predict(ridge_mod, s=4, newx=x[test,])
mean((ridge_pred - y_test)^2)  ## MSE

mean((mean(y[train])-y_test)^2)
ridge_pred <- predict(ridge_mod, s=1e10, newx=x[test,])
mean((ridge_pred - y_test)^2)  ## MSE

## use croos-validation to choose the tuning parameter lamda
set.seed(1)
cv_out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv_out)
best <- cv_out$lambda.min
best
## the value of MSE associated with the best lambda
ridge_pred <- predict(ridge_mod, s=best, newx=x[test,])
mean((ridge_pred - y_test)^2) 
# finally we fit our redige regression model on the full data set, using the value
# of lambda chosen by cross-validation, and examine the coefficient estimates.
out <- glmnet(x,y, alpha = 0)
predict(out, type= "coefficients", s=best)[1:20,]

##========================== lasso regression ==============================================
lasso_mod <- glmnet(x[train,],y[train], alpha = 1, lambda = grid)
plot(lasso_mod)

# perform cross-validation  and compute the associated test error
set.seed(1)
cv_out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv_out)
best <- cv_out$lambda.min
best
lasso_pred <- predict(lasso_mod, s=best, newx=x[test,])
mean((lasso_pred-y_test)^2)

out <- glmnet(x,y, alpha = 1, lambda = grid)
lasso_coef <- predict(out, type= "coefficients", s=best)[1:20,]
lasso_coef