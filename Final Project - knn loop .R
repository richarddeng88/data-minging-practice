# devide the half train data into 100,200,300......9000,
library(caret); set.seed(1001)
intrain <- createDataPartition(y=train_pml$classe, p=0.5, list = F)
training <- train_pml[intrain,]; testing <- train_pml[-intrain,]

    # knn loop with different training size
    train_size <- seq(100, 1000, 100); t=0 ; knn_accuracy <- rep(NA,length(train_size)); best_k <- rep(NA,length(train_size)); x <- c(100,200,900)
    for (i in train_size){
        split <- sample(dim(training)[1], i)
        knn_train <- training[split,]
        t=t+1
        
        # train knn model
        ctrl <- trainControl(method = "cv", number = 10) 
        grid <- expand.grid(k=c(1:10,12,14,16,18,seq(20,80,3)))
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
        
    }
    
        #ploting
        plot(train_size,knn_accuracy, type = "l")
        plot(train_size,best_k, type="l")
    
    # QDA loop with different training size
    train_size <- seq(100, 9000, 4000); t=0 ; 
    qda_test_accuracy <- rep(NA,length(train_size)); qda_train_accuracy <- rep(NA,length(train_size)); 
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
    
    # RF loop with different training size
    train_size <- seq(100, 9000, 200); t=0 ; rf_accuracy <- rep(NA,length(train_size)); best_mtry <- rep(NA,length(train_size)); x <- c(100,200,900)
    for (i in train_size){
        split <- sample(dim(training)[1], i)
        rf_training <- training[split,]
        t=t+1
        
        # train knn model
        ctrl <- trainControl(method = "cv", number = 10) 
        grid <- expand.grid(.mtry=c(2,4,6,8,10,12) )
        rf_model <- train(classe~.,
                          data=rf_training,
                          method="rf",
                          tuneGrid = grid,
                          trControl=ctrl)
        
        #evaluate performance
        rf_pred <- predict(rf_model, testing)
        best_mtry[t] <- as.numeric(rf_model$bestTune)
        rf_accuracy[t] <- confusionMatrix(rf_pred, testing$classe)$overall[1]
        
    }
















