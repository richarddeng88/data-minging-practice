x <- rnorm(100)
y <- rnorm(100)
pdf()
plot(y,x)
lm <- lm(y~x)
abline(lm,col="red",lwd=3)
dev.off()


## countour plot
x <- seq(1,5)
y <- seq(1,5)

#The outer product of the arrays X and Y is the array A with dimension c(dim(X), dim(Y))
f=outer(x,y,function(x,y)cos(y)/(1+x^2))

#?? how to explain this 
contour(x,y,f,nlevels = 45, add=T)

#t(f)is the transpose matrix of f
fa<-(f-t(f))/2
contour(x,y,fa,nlevels = 100)

#image() is also known as a heatmap, and sometimes used to plot temperature in weather forecasts. 
#persp() can be used to produce a three-dimensional plot. the arguments theta and phi control
#the angles at which the plot is viewed. 
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta = 30, phi = 20)

## 2.3.3 indexing data
library(MASS)
library(ISLR)
auto <- Auto
fix(auto)
auto$cylinders <- as.factor(auto$cylinders)
auto$origin <- as.factor(auto$origin)

plot(auto$cylinders,auto$mpg,col="red",varwidth=T, xlab="cylinders",ylab="mpg")

hist(auto$mpg,col="lightgreen",breaks=15)
pairs(~mpg+displacement+horsepower+weight+acceleration,auto)

plot(auto$horsepower,auto$mpg,pch=20,col=auto$cylinders)
identify(auto$horsepower,auto$mpg,auto$origin)  ## when do we use identify() in real work. 

plot(auto$weight,auto$horsepower,pch=20,col=auto$cylinders)

