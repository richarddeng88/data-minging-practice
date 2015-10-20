library(ISLR)
wage <- Wage

fit <- lm(wage~poly(age,4),data=wage)
coef(summary(fit))

age_grid <- seq(range(wage$age)[1],range(wage$age)[2])
pred <- predict(fit, newdata = list(age=age_grid), se=T)
se_bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit-2*pred$se.fit)

# plot the data and add the fit from the degree-4 polynomial
par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(wage$age, wage$wage, xlim=range(wage$age), cex=0.5, col="darkgrey")
title("degree-4 polynomial", outer=T)
lines(age_grid, pred$fit, col="blue", lwd=2)
matlines(age_grid, se_bands, col="blue",lwd=1, lty=3)

## decide on the degree of the polynomial to use
fit1 <- lm(wage~age, data=wage)
fit2 <- lm(wage~poly(age,2), data=wage)
fit3 <- lm(wage~poly(age,3), data=wage)
fit4 <- lm(wage~poly(age,4), data=wage)
fit5 <- lm(wage~poly(age,5), data=wage)
anova(fit1, fit2, fit3, fit4, fit5)

## ANOVA method works whether or not we used orthogonal polynomials. it also works when
# when we have other terms in the model as well. 
fit1 <- lm(wage~education + age, data=wage)
fit2 <- lm(wage~education + poly(age,2), data=wage)
fit3 <- lm(wage~education + poly(age,3), data=wage)
anova(fit1, fit2, fit3)


##
fit <- glm(I(wage>250) ~ poly(age,4), data=wage, family=binomial)
pred <- predict(fit, newdata=list(age=age_grid), se=T)

# for the rest of 7.1 polynomial regressiong
pfit <- exp(pred$fit)/(1+exp(pred$fit))
se_bands_logit <- cbind(pred$fit + 2*pred$fit,pred$fit - 2*pred$fit)
se_bands <- exp(se_bands_logit)/(1+se_bands_logit)

##=================================regression spline===============================================
library(ISLR)
library(MASS)
library(splines)
wage <- Wage
# a cubic spline with three knots has seven degrees of freedom. 
# an instercept plus six basis functions. 
fit <- lm(wage~bs(age, knots = c(25,40,60)), data=wage)
pred <- predict(fit, newdata=list(age=age_grid), se=T)
plot(wage$age,wage$wage, col="gray")
lines(age_grid,pred$fit, lwd=2)
lines(age_grid, pred$fit+2*pred$se.fit, lty="dashed")
lines(age_grid, pred$fit-2*pred$se.fit, lty="dashed")

bs <- bs(wage$age,knots=c(25,40,60))
bs1 <- bs(wage$age, df=6)
attr(bs(wage$age,df=6), "knots")

##================================ natural spline =======================================
fit2 <- lm(wage~ns(age,df=4), data=wage)
pred2 <- predict(fit, newdata=list(age=age_grid), se=T)
lines(age_grid,pred2$fit, lwd=2, col="red")


##===============================smoothing spline========================================
plot(wage$age, wage$wage, cex=0.5, col="darkgrey",main = "smoothing spline")
fit <- smooth.spline(wage$age, wage$wage, df=16)
# in the next model, we select the smoothnes level by cross-validation. this reuslt a value
# of lambda that yelds 6.8 degrees of freedom
fit2 <- smooth.spline(wage$age, wage$wage, cv=T)
fit2$df

lines(fit,col="red",lwd=2)
lines(fit2, col="blue",lwd=2)
legend("topright", legend=c("16 DF","6.8 DF"), col = c("red","blue"))












