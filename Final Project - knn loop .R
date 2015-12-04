# devide the half train data into 100,200,300......9000,
library(caret); set.seed(1001)
intrain <- createDataPartition(y=train_pml$classe, p=0.5, list = F)
training <- train_pml[intrain,]; testing <- train_pml[-intrain,]

    # knn loop
    train_size <- seq(100, 9000, 100); t=0 ; accuracy <- rep(NA,length(x)); best_k <- rep(NA,length(x)); x<- c(100,200,900)
    for (i in x){
        split <- sample(dim(training)[1], i)
        knn_train <- training[split,]
        t=t+1
        
        # train knn model
        ctrl <- trainControl(method = "cv", number = 10) 
        knn_model <- train(classe~., 
                           data=knn_train, 
                           method="knn", 
                           preProcess=c("center","scale"),
                           tuneLength = 40,
                           trControl=ctrl) # need to set up different k values as i want
        
        # evaluate performance
        # plot(knn_model)
        knn_pred <- predict(knn_model, testing)
        best_k[t] <- as.numeric(knn_model$bestTune)
        accuracy[t] <- confusionMatrix(knn_pred, testing$classe)$overall[1]
}


















