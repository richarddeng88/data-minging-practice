library(ISLR)
library(MASS)
bos <- Boston
attach(bos)
bos1 <- bos[,c(1,5,6,7,12,13,14)]
pairs(bos1)
apply(bos,2,mean,na.rm=T)

lm.fit <- lm(crim~rm, data = bos)
lm.fit
summary(lm.fit)

# the 95% confidence interval with a lstat value of 10 is (24.47, 25.63)
pred_confi <-predict(lm.fit, data.frame(bos$rm),interval="confidence")
# and the 95% predictionintervalis (12.8,37.28)
predict(lm.fit, data.frame(rm=(2:10)),interval="prediction")

##ploting the original data set and add the linear regression line
with(bos, plot(rm,crim,pch=20,col="black"))
abline(lm.fit, col="blue",lwd=3)

lm.fit <- lm(crim~medv, data = bos)
lm.fit
summary(lm.fit)

lm.fit <- lm(crim~., data = bos)
lm.fit
summary(lm.fit)

## C. 
coef_x <- rep(NA,13)
for(i in 1:13){
    lm.fit <- lm(crim~bos[,(i+1)], data = bos)
    coef_x[i] <- coef(lm.fit)[2]
}

lm.fit <- lm(crim~., data = bos)
coef_y <- coef(lm.fit)[2:14]

plot(coef_x, coef_y, col="black", pch=20,xlab = "the univariate regression coefficients",
     ylab="the multipleregression coefficients")

## D. 
fit <- lm(crim~poly(lstat,3), data=bos)
fit
summary(fit)


