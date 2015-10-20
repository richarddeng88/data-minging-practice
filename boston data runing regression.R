##run regresson on dataset Boston
library(ISLR)
library(MASS)
bos <- Boston
bos1 <- bos[,c(3,6,8,13,14)]
pair(bos1)
apply(bos,2,mean,na.rm=T)
##regression code
lm.fit <- lm(medv~lstat, data = bos)
## regression result
lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
confint(lm.fit)
residuals(lm.fit)
predict(lm.fit)

# the 95% confidence interval with a lstat value of 10 is (24.47, 25.63)
predict(lm.fit, data.frame(lstat=(c(5,10,15))),interval="confidence")
# and the 95% predictionintervalis (12.8,37.28)
predict(lm.fit, data.frame(lstat=(c(5,10,15))),interval="prediction")

##ploting 
with(bos, plot(lstat,medv,pch=20,col="lightgreen"))
abline(lm.fit, col="darkblue",lwd=3)


#testing
plot(1:20,1:20,pch=1:20)


## plot 2 at a time
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit),residuals(lm.fit),pch=20, col="red")
plot(predict(lm.fit),rstudent(lm.fit),pch=20,col="blue")  ## rstudent() returns the studentized residuals

## leverage statistics can be computed for any predictor using the hatvalues() function
hatvalues(lm.fit)
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

## multi regression data






