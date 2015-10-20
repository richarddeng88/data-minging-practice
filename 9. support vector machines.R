#================================= support vector classifier========================================
set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,]+1
# we check weather the classes are linearly seperable. 
plot(x, col=c(4,2))
## we encode the response as a factor and conbine the variable and response together. 
dat <- data.frame(x=x, y=as.factor(y))

library(e1071)
sumfit <- svm(y~., data=dat, kernel="linear", cost=1000, scale=F)
# let us plot the support vector classifier obtained above. 
plot(sumfit, dat)







