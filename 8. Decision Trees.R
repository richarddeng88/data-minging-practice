library(ISLR)
library(MASS)
library(tree)
##=============================== fitting classification tree====================================
car <- Carseats
high <- ifelse(car$Sales <=8, "No","Yes")
#high <- ifelse(car$Sales <=8, 0,1)
car <- data.frame(car,high)
tree_car <- tree(high~.-Sales, data=car)
# the summary() function lists the variables that are used ias internal nodes in the tree, 
# the number of terminal nodes, and the training error rate. 
summary(tree_car)
# display the tree
plot(tree_car)
text(tree_car, pretty=0)

## in order to properly evaluate the performance of a classification tree, we must
# estimate the test error rather than simply computing the training error. 
# we split the observations into a training set and a test set. 
set.seed(2)
train <- sample(1:nrow(car), 200)
car_test <- car[-train,]
high_test <- high[-train]
tree_car <- tree(high~.-Sales, data=car, subset=train)
tree_pred <- predict(tree_car, car_test, type="class")
table(tree_pred,high_test)
mean(tree_pred==high_test)

## cost complexity pruning is used in order to select a sequence of trees for consideration
set.seed(3)
cv_car <- cv.tree(tree_car,FUN=prune.misclass)

par(mfrow=c(1,2))
plot(cv_car$size, cv_car$dev, type="b")
plot(cv_car$k, cv_car$dev, type = "b")

## we apply the prune.misclas() function in order to prune the tree to obtain the nine-node tree
prune_car <- prune.misclass(tree_car, best=9)
plot(prune_car)
text(prune_car, pretty=0)
# claculate the test error rate to see how well this pruned tree performance
tree_pred <- predict(prune_car, car_test, type="class")
table(tree_pred, high_test)
mean(tree_pred==high_test)

##=========================== fitting regression trees ============================================
library(MASS)
set.seed(1)
bos <- Boston
train <- sample(1:nrow(bos), nrow(bos)/2)
tree_bos <- tree(medv~., data = bos, subset= train)
summary(tree_bos)

plot(tree_bos)
text(tree_bos, pretty=0)

## cross validation method to see the square error in different knots. 
cv_bos <- cv.tree(tree_bos)
plot(cv_bos$size, cv_bos$dev, type = "b")

## use the decision tree at the best condition. 
prune_bos <- prune.tree(tree_bos, best = 5)
plot(prune_bos)
text(prune_bos, pretty=0)

yhat <- predict(prune_bos, newdata = bos[-train,])
bos_test <- bos[-train,"medv"]
plot(yhat, bos_test)
abline(0,1)
mean((yhat-bos_test)^2)

##=============================bagging and random forests=================================
library(ISLR)
library(MASS)
library(randomForest)
bos <- Boston
set.seed(1)
train <- sample(nrow(bos), nrow(bos)/2)
# here the mtry means that all 13 predictors should be considered for each split of 
# the tree. 
bag_bos <- randomForest(medv~., data=bos, subset=train, mtry=13, importance=T) # when mtry=p, it's bagging

yhat_bag <- predict(bag_bos, newdata = bos[-train,])
plot(yhat_bag, bos_test)
mean((yhat_bag - bos_test)^2)

# random forest, let m= sqrt(p)
set.seed(1)
rf_bos <- randomForest(medv~., data=bos, subset = train, mtry=6, importance=T)
yhat_rf <- predict(rf_bos, newdata=bos[-train,])
mean((yhat_rf - bos_test)^2)

# ploting the thes importance measures 
importance(rf_bos)
varImpPlot(rf_bos)

##========================================boosting =============================================
library(ISLR)
library(MASS)
library(gbm)
set.seed(1)
boost_bos <- gbm(medv~., data=bos[train,], distribution= "gaussian")
summary(boost_bos) 
# this produces a relative influence plot and also outputs the relative influence statistics. 

# we can also produce partial dependence plots for these two variables. these plots illustrate the
# marginal effect of the selected variables on the response after integrating out the other variables. 
par(mfrow=c(1,2))
plot(boost_bos, i="rm")
plot(boost_bos, i = "lstat")

yhat_boost <- predict(boost_bos, newdata = bos[-train,], n.trees = 5000)
mean((yhat_boost- bos_test)^2)








