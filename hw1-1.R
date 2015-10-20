set.seed(13)
split <- sample(150,100)
train_x <- iris[split,-5]
test_x <- iris[-split,-5]
train_species <- iris[split,5]
test_species <- iris[-split,5]

library(class)
#Q2
knn.pred <- knn(train_x, test_x, train_species, k=3,prob = T)
table(knn.pred, test_species)
sum(knn.pred!=test_species)
mean(knn.pred==test_species)

#Q3
k_num <- c(1:10,20,30,40,50)
mis_num <- rep(0,length(k_num))
mis_rate <- rep(0,length(k_num))
t=0
for (i in k_num) {
    knn.pred <- knn(train_x, test_x, train_species, k=i)
    t=t+1
    mis_num[t] <- sum(knn.pred!=test_species)
    mis_rate[t] <- 1-mean(knn.pred==test_species)
}
mis_num
mis_rate

#Q4.a
plot(k_num,mis_num,type = "l",xlab = "k",ylab="number misclassified")
#Q4.b
plot(log(k_num),mis_num,type = "l",xlab = "Log k",ylab="number misclassified")
#===========================================================================
set.seed(17)
split <- sample(150,100)
train_x <- iris[split,-5]
test_x <- iris[-split,-5]
train_species <- iris[split,5]
test_species <- iris[-split,5]

k_num <- c(1:10,20,30,40,50)
mis_num2 <- rep(0,length(k_num))
mis_rate2 <- rep(0,length(k_num))
t=0
for (i in k_num) {
    knn.pred <- knn(train_x, test_x, train_species, k=i)
    t=t+1
    mis_num2[t] <- sum(knn.pred!=test_species)
    mis_rate2[t] <- 1-mean(knn.pred==test_species)
}
mis_num2
mis_rate2

#Q4.c
plot(k_num,mis_num,col="red", type = "l",xlab = "k",ylab="number misclassified")
points(k_num,mis_num2,type = "l",col="blue")
legend("topleft",legend = c("seed=13","seed=17"),lty=1,col=c("red","blue"))

