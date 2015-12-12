library(MASS);library(ISLR);library(dplyr)
def <- Default
prop.table(table(def$default))
par(mfrow=c(1,2))

plot(def$balance,def$income,xlab="balance",ylab="income",col=def$default,pch=20)
plot(def$balance,def$income,xlab="balance",ylab="income",col=def$student,pch=20)

    # tak out all points that balance =0
    df_no_0 <- filter(def, balance!=0)
    plot(df_no_0$balance,df_no_0$income, pch=20)
    
    
    # try tree model
    library(caret)
    intrain <- createDataPartition(def$default, p=0.8, list=F)
    training <- def[intrain,]; testing<- def[-intrain,]
    tree_model <- train(default~.,
                        data=training,
                        mothod="rpart")
    tree_pred <- predict(tree_model, testing)
    confusionMatrix(tree_pred, testing$default)
    
    

par(mfrow=c(1,2))
plot(def$default,def$balance,col=c("lightblue","lightgreen"),
     xlab="Default",ylab="Credit card Balance")
plot(def$default,def$income,col=c("lightblue","lightgreen"),
     xlab="Default",ylab="Income")

plot(def$student,def$balance,col=c("lightblue","green"),
     xlab="Student",ylab="Credit card Balance")
plot(def$student,def$income,col=c("lightblue","green"),
     xlab="Student",ylab="Income")

plot(def$balance,def$student,col=def$default)

###-==========================ploting test=========================================
library(ggplot2)
qplot(balance,income,data=def,color=default,fill=default,geom=c("point","smooth"))

library(lattice)
xyplot(income~balance|default,data=def)
##======================================================================
# run logistic regression
contrasts(def$default)
def.glm <- glm(default~balance+income+student, family = "binomial", data = def)
summary(def.glm)

def.glm <- glm(default~balance+income+student, family = "binomial", data = def)
summary(def.glm)

# try to plot
plot(def$balance,def$default)
abline(def.glm)

##===============================R LAB==================================================
stock <- Smarket
cor(stock[,-9])

par(mfrow=c(1,1))
stock$Year <- factor(stock$Year,levels=c(2001,2002,2003,2004,2005))
plot(stock$Year,stock$Volume)
plot(stock$Volume)

##===================================logistic regression=================================
# glm(): generalized linear model
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=stock,family="binomial")
summary(glm.fit)

ontrasts(stock$Direction) #check if a dummy variable with a 1 for up?
glm.probs <- predict(glm.fit, type="response")
glm.probs[1:10]

#convert the predicted probabilities in to class lables, up or down.
glm.pred <- rep("Down",1250)
glm.pred[glm.probs > 0.5] <- c("Up")
table(glm.pred, stock$Direction)

mean(glm.pred==stock$Direction) #compute the rate of correct predictions. 

#### split the data into train and test
stock_test <- stock[stock$Year==2005,]
stock_train <- stock[stock$Year!=2005,]

glm.fit<- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=stock_train,family="binomial")
glm.probs <- predict(glm.fit, stock_test, type="response")

glm.pred <- rep("Down",dim(stock_test)[1])
glm.pred[glm.probs > 0.5] <- c("Up")
table(glm.pred, stock_test$Direction)

mean(glm.pred==stock_test$Direction)

## change ligistic regression prediction
stock_test <- stock[stock$Year==2005,]
stock_train <- stock[stock$Year!=2005,]

glm.fit<- glm(Direction~Lag1+Lag2, data=stock_train,family="binomial")
glm.probs <- predict(glm.fit, stock_test, type="response")

glm.pred <- rep("Down",dim(stock_test)[1])
glm.pred[glm.probs > 0.5] <- c("Up")
table(glm.pred, stock_test$Direction)

mean(glm.pred==stock_test$Direction)

## predict on a day when lag1 and lag2 equal 1.2 and 1.1
predict(glm.fit, newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)), type="response")


##===========================linear discriminant analysis=======================================
library(MASS)
lda.fit=lda(Direction ~ Lag1+Lag2, data=stock_train)
lda.fit
plot(lda.fit)

predict(lda.fit,stock_t)




##====================================KNN(K-Nearest Neighbors)==================================

train_x <- stock_train[,c("Lag1","Lag2")]
test_x <- stock_test[,c("Lag1","Lag2")]
train_direction <- stock_train[,c("Direction")]
test_direction <- stock_test[,c("Direction")]

library(class)
set.seed(1)
knn.pred <- knn(train_x, test_x, train_direction, k=1)
table(knn.pred, test_direction)

###===========================an application to caravan insurance data==========================















