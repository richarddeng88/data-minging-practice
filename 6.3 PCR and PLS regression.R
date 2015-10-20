library(pls)
library(ISLR)
set.seed(2)
hit <- na.omit(Hitters)
x <- model.matrix(Salary~., hit)[,-1]
y <- hit$Salary
pcr_fit <- pcr(Salary~., data=hit, scale=T, validation="CV")

# the resulting fit can be examined using summary()
summary(pcr_fit)
# we can plot the cross-validation scores using validationplot()
validationplot(pcr_fit, val.type = "MSEP")


## we now perform PCR on the training data and evaluate its test set performance.
set.seed(1)
train <- sample(1:nrow(hit), nrow(hit)/2)
test <- (-train)
y_test <- y[test]

pcr_fit <- pcr(Salary~., data=hit, subset=train, scale=T, validation="CV")
validationplot(pcr_fit, val.type = "MSEP")

pcr_pred <- predict(pcr_fit, hit[test,], ncomp=7)
mean((pcr_pred-y_test)^2)

# finnaly, we refit PCR on the full data set, using M=7
pcr_fit <- pcr(y~x, scale= T, ncomp=7)
summary(pcr_fit)
pcr_pred <- predict(pcr_fit, x, ncomp=7)









