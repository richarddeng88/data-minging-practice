library(caret)
intrain <- sample(150,100)
training <- iris[intrain,]
testing <- iris[-intrain,]


    grid <- expand.grid(k=c(1:10,12,14,16,18,seq(20,80,3)))
    knn_model <- train(Species~., 
                       data=training, 
                       method="knn",
                       #preProcess=c("center","scale"),
                       tuneGrid = grid)

}

knn_model
