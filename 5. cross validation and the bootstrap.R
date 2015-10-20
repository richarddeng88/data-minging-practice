## =======================THE VALIDATION SET APPROACh=====================================
library(ISLR)
set.seed(3)
train <- sample(392,196)
auto <- Auto

# calculate the MSE of the 196 observations in the validation set. 

lm.fit <- lm(mpg~horsepower, data=auto, subset=train)
mean((auto$mpg-predict(lm.fit, auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower,2), data=auto,subset=train)
mean((auto$mpg-predict(lm.fit2, auto))[-train]^2)

lm.fit3 <- lm(mpg~poly(horsepower,3), data=auto,subset=train)
mean((auto$mpg-predict(lm.fit3, auto))[-train]^2)

lm.fit4 <- lm(mpg~poly(horsepower,4), data=auto,subset=train)
mean((auto$mpg-predict(lm.fit4, auto))[-train]^2)

##===========================leave-one-out cross-validation=======================================
glm.fit <- glm(mpg~horsepower, data=auto)
coef(glm.fit)

library(boot)
glm.fit <- glm(mpg~horsepower, data=auto)
cv.err <- cv.glm(auto,glm.fit)
cv.err$delta

n <- 10
cv_error <- rep(0,n)
tt <- rep(0,n)
for (i in 1:n) {
    glm.fit <- glm(mpg ~ poly(horsepower,i), data=auto)
    cv_error[i] <- cv.glm(auto, glm.fit)$delta[1]
    tt[i] <- cv.glm(auto,glm.fit)$delta[2]
}
cv_error
tt

##==================================k-fold cross-validation=======================================
set.seed(17)
n = 10
cv_error1 <- rep(0,n)
tt <- rep(0,n)
for (i in 1:n){
    glm.fit <- glm(mpg ~ poly(horsepower,i), data=auto)
    cv_error1[i] <- cv.glm(auto,glm.fit, K=10)$delta[1]
    tt[i] <- cv.glm(auto,glm.fit, K=10)$delta[2]
}
cv_error1
tt

##=================================KNN corss-validation===========================================










