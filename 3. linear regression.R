##run regresson on dataset Boston
library(ISLR)
library(MASS)
bos <- Boston
attach(bos)
bos1 <- bos[,c(3,6,8,13,14)]
pairs(bos1)
apply(bos,2,mean,na.rm=T)

##=====================================simple regression ===================================
##regression code
lm.fit <- lm(medv~lstat, data = bos)
lm.fit
summary(lm.fit)

names(lm.fit)
coef(lm.fit)  ## obtain the coeficient estimates
confint(lm.fit) ## obtain a confidence interval for the coefficient estimates
residuals(lm.fit)
pre <- predict(lm.fit)  ## return a vector of fitted value, which is predicted value
head(pre)

# the 95% confidence interval with a lstat value of 10 is (24.47, 25.63)
predict(lm.fit, data.frame(lstat=(1:5)),interval="confidence")
# and the 95% predictionintervalis (12.8,37.28)
predict(lm.fit, data.frame(lstat=(1:5)),interval="prediction")

##ploting the original data set and add the linear regression line
with(bos, plot(lstat,medv,pch=20,col="lightgreen"))
abline(lm.fit, col="darkblue",lwd=3)

#testing the point sign
plot(1:20,1:20,pch=1:20)

## plot the four diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

plot(predict(lm.fit),residuals(lm.fit),pch=20, col="black")
    
## rstudent() returns the studentized residuals
plot(predict(lm.fit),rstudent(lm.fit),pch=20,col="blue")  

## leverage statistics can be computed for any predictor using the hatvalues() function
hatvalues(lm.fit)
plot(hatvalues(lm.fit))
# which.max() function identifies the index of the largest element of a vector.
which.max(hatvalues(lm.fit))

#================================== multi regression ==================================
lm1 <- lm(medv~lstat+age, bos)
summary(lm1)

lm2 <- lm(medv~., bos)
summary(lm2)
summary(lm2)$r.sq
summary(lm2)$sigma

## claculate variance inflation factors, which assess multicollinearity. a VIF value that 
# exceeds 5 or 10 indicates a problematic amount of collinearity.
# the smallest possible value for VIF is 1, which indicates complete absence of collinearity.
library(car)
vif(lm2)  

lm3 <- lm(medv~.-age-indus,bos)
summary(lm3)

#====================================== Interaction terms=======================================
summary(lm(medv~lstat*age,bos))


#========================= non-linear transformations of predictors============================
lm.fit.log <- lm(medv~log(rm),bos)

lm.fit5 <- lm(medv~poly(lstat,5),bos)
summary(lm.fit5)

lm.fit2 <- lm(medv~lstat+I(lstat^2),bos)
summary(lm.fit2)

# we use anova() function to further quantify the exetent to which the quadratic fit is
# superior to the linear fit
lm.fit <- lm(medv~lstat, bos)
anova(lm.fit,lm.fit2)
par(mfrow=c(2,2))
# when the lstat^2 term is included in the model, there is little discernible pattern in 
# the residuals.
plot(lm.fit2)
plot(lm.fit)

par(mfrow=c(1,1))
plot(bos$lstat,bos$medv,pch=20)

##============================ quanlitative predicotrs==============================
# attempt to predict Sales(child car seat sales) in 400 locations based on a number of predictors.
# the data includes qualitative predictors such as Shelveloc, an indicator of the quality.
car <- Carseats

pairs(car)  
summary(lm(Sales~.+Income:Advertising+Price:Age,car))
contrasts(car$ShelveLoc)







