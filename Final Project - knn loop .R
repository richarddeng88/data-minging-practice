# devide the half train data into 100,200,300......9000,
library(caret); set.seed(1001)
train_pml <-train_pml[sample(dim(train_pml)[1], 18000),] # get 18,000 observation to wrok on
intrain <- sample(dim(train_pml)[1], 9000)
training <- train_pml[intrain,]; testing <- train_pml[-intrain,]

    # knn loop with different training siz
    train_size <- seq(200, 9000, 200); t=0 
    knn_accuracy <- rep(NA,length(train_size))
    knn_train_accuracy <- rep(NA, length(train_size))
    best_k <- rep(NA,length(train_size)); x <- c(100,200,900)
    for (i in train_size){
        split <- sample(dim(training)[1], i)
        knn_train <- training[split,]
        t=t+1
        
        # train knn model
        ctrl <- trainControl(method = "cv", number = 10) 
        grid <- expand.grid(k=c(5:10,12,14,16,18,20,22,24,26,28,30))
        knn_model <- train(classe~., 
                           data=knn_train, 
                           method="knn", 
                           #preProcess=c("center","scale"),
                           tuneGrid = grid,
                           #tuneLength = 40,
                           trControl=ctrl) # need to set up different k values as i want
        
        # evaluate performance
        # plot(knn_model)
        knn_pred <- predict(knn_model, testing)
        best_k[t] <- as.numeric(knn_model$bestTune)
        knn_accuracy[t] <- confusionMatrix(knn_pred, testing$classe)$overall[1]
        
        knn_train_pred <- predict(knn_model, knn_train)
        knn_train_accuracy[t] <- confusionMatrix(knn_train_pred, knn_train$classe)$overall[1]
        
    }
    
        #ploting
        plot(train_size,knn_accuracy, type = "l")
        plot(train_size,knn_train_accuracy, type = "l")
        plot(train_size,best_k, type="l")
    
    # QDA loop with different training size
    train_size <- seq(600, 9000, 200); t=0
    qda_test_accuracy <- rep(NA,length(train_size)) 
    qda_train_accuracy <- rep(NA,length(train_size))
    for (i in train_size){
        split <- sample(dim(training)[1], i)
        qda_training <- training[split,]
        t=t+1
        
        # train qda model
        ctrl <- trainControl(method = "cv", number = 10) 
        qda_model <- train(classe~.,
                           data = qda_training,
                           method="qda",
                           trControl=ctrl)
        
        # evaluate performance
        # plot(qda_model)
        qda_pred <- predict(qda_model, testing)
        qda_test_accuracy[t] <- confusionMatrix(qda_pred, testing$classe)$overall[1]
        qda_train_pred <- predict(qda_model, qda_training)
        qda_train_accuracy[t] <- confusionMatrix(qda_train_pred, qda_training$classe)$overall[1]
    }
        
    plot(train_size,qda_test_accuracy, type = "l")
    plot(train_size,qda_train_accuracy, type = "l")
    #plot(train_size,best_mtry, type="l")  
    
    
    # RF loop with different training size
    train_size <- seq(200, 9000, 200); t=0 ; 
    rf_accuracy <- rep(NA,length(train_size)); 
    rf_train_accuracy <- rep(NA,length(train_size))
    best_mtry <- rep(NA,length(train_size)); 
    x <- c(100,200,900)
    for (i in train_size){
        split <- sample(dim(training)[1], i)
        rf_training <- training[split,]
        t=t+1
        
        # train rf model
        ctrl <- trainControl(method = "cv", number = 10) 
        grid <- expand.grid(.mtry=c(2,4,6,8,10,12))
        rf_model <- train(classe~.,
                          data=rf_training,
                          method="rf",
                          tuneGrid = grid,
                          trControl=ctrl)
        
        #evaluate performance
        rf_pred <- predict(rf_model, testing)
        best_mtry[t] <- as.numeric(rf_model$bestTune)
        rf_accuracy[t] <- confusionMatrix(rf_pred, testing$classe)$overall[1]
        
        rf_train_pred <- predict(rf_model, rf_training)
        rf_train_accuracy[t] <- confusionMatrix(rf_train_pred,rf_training$classe)$overall[1]
        
    }
  
        plot(train_size,rf_accuracy, type = "l")
        plot(train_size,best_mtry, type="l")
        plot(train_size,rf_train_accuracy, type = "l")


        # VISULIZATION
        qda_train_accuracy <- c(0.9902,0.9901,qda_train_accuracy)
        qda_train_accuracy <- c(0.9902,0.9901,qda_train_accuracy)
        train_size <- seq(200, 9000, 200)
        
        knn <- data.frame(size=train_size, test_ac=knn_accuracy, train_ac=knn_train_accuracy,methods="knn")
        qda <- data.frame(size=train_size, test_ac=qda_test_accuracy,train_ac=qda_train_accuracy, methods="qda")
        rf <- data.frame(size=train_size, test_ac=rf_accuracy, train_ac=rf_train_accuracy, methods="rf")
        df <- rbind(knn,qda,rf)
        
        library(ggplot2);library(dplyr)
        #df1 <- group_by(df, methods)
        ggplot(df, aes(x=size,y=test_ac, col=methods))+ geom_line()
        ggplot(df, aes(x=size,y=train_ac, col=methods))+ geom_line()

        ##have a try 2
        knn1 <- rbind(data.frame(size=train_size, accuracy=knn_train_accuracy,type="knn-train",method="knn"),
                      data.frame(size=train_size, accuracy=knn_accuracy, type="knn-test", method="knn"))
        qda1 <- rbind(data.frame(size=train_size, accuracy=qda_train_accuracy,type="qda-train",method="qda"),
                      data.frame(size=train_size, accuracy=qda_test_accuracy, type="qda-test", method="qda"))
        rf1 <- rbind(data.frame(size=train_size, accuracy=rf_train_accuracy,type="rf-train",method="rf"),
                     data.frame(size=train_size, accuracy=rf_accuracy , type="rf-test", method="rf"))
        
        ggplot(knn1, aes(x=size, y=accuracy, col=type))+geom_line()
        ggplot(qda1, aes(x=size, y=accuracy, col=type))+geom_line()
        ggplot(rf1, aes(x=size, y=accuracy, col=type))+geom_line()
        
        
        #plot(train_size,knn_accuracy,type="l", ylim = c(0,1))
        #points(train_size,qda_test_accuracy,type="l",col="red")
        #points(train_size,rf_accuracy, type = "l", col="blue")
        
        
        #best mtry
        plot(train_size,best_mtry,type="l",ylim = c(2,12), xlim = c(0,9000),xlab = "Train Size",
             ylab = "Best Mtry",
             main = "optimum Mtry in random forest method")
        #best k
        plot(train_size,best_k,type="l",ylim = c(0,30), xlim = c(0,9000),xlab = "Train Size",
             ylab = "Best K",
             main = "optimum k in KNN method")


        
        qqplot(iris,aes(Sepal.Length,Sepal.Width, color=Species))+geom_line()

