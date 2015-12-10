################################### DATA MINING PROJECT DRAFT #############################################
##DATA READING
train_pml <- read.csv("data/practical_machine_l/pml-training.csv",stringsAsFactors = T)
test_pml <- read.csv("data/practical_machine_l/pml-testing.csv",stringsAsFactors = T)
## PREPROCESSING

    # FEATURE SELECTION (PREPROCESSING)
        # DEAL COMLUMNS WITH NAs
        na <- sapply(test_pml, function(x){sum(is.na(x))})
        train_pml <- subset(train_pml, select = !(names(train_pml) %in% names(na[na==20])))
        
        # REMOVE ZERO COVARIATE
        nsv <- nearZeroVar(train_pml) # optiong:  nearZeroVar(train_pml,saveMetrics = T)
        train_pml <- train_pml[,-nsv]
        
        # REMOVE DESCREIPTIVE FEATURES. 
        excludecols <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", 
                         "cvtd_timestamp", "num_window","new_window")
        train_pml <- train_pml[, !names(train_pml) %in% excludecols]
        
        # CLEANING
        rm(test_pml,excludecols,na,nsv)
        
    # DATA SPLITING
        library(caret); set.seed(1001)
        #intrain <- createDataPartition(y=train_pml$classe, p=0.5, list = F)
        intrain <- sample(dim(train_pml)[1],9000)
        training <- train_pml[intrain,]; testing <- train_pml[-intrain,]


## TRAIN MODELS
        # KNN
            #using Preprossing to standardize the data first. 
            proobj <- preProcess(training[,-53], method = c("center", "scale")) # center and scale the data
            #train_body <- predict(proobj, training[,-53])
            test_body <- predict(proobj, testing[,-53])
            #knn_training <- cbind(train_body,training[,53])
            
            ## to create a control object named ctrl that uses 10-fold CV and the oneSE secetion function. 
            library(caret)
            ctrl <- trainControl(method = "cv", number = 10) 
            grid <- expand.grid(k=c(5,7,9))
            knn_model <- train(classe~., 
                               data=training, 
                               method="knn", 
                               preProcess=c("center","scale"),
                               tuneGrid = grid,
                               #tuneLength = 40,
                               trControl=ctrl) # need to set up different k values as i want
            
            # evaluate performance
            plot(knn_model)
            knn_pred <- predict(knn_model, testing)
            confusionMatrix(knn_pred, testing$classe)

        # TREE
            library(caret)
            ctrl <- trainControl(method = "cv", number = 10) # 10 fold cross validation
            
            tree_model <- train(classe~.,
                                data=training,
                                method="rpart",
                                trControl=ctrl)
            print(tree_model$finalModel)
            library(rattle);library(rpart.plot)
            fancyRpartPlot(tree_model$finalModel)
            rpart.plot(tree_model$finalModel)
        
            # evaluate performance
            tree_pred<- predict(tree_model, testing)
            confusionMatrix(tree_pred, testing$classe)
                
                # using TREE code in the testbook
                tree1 <- tree(classe~., data = training)
                summary(tree1)
                cv1 <- cv.tree(tree1)
                plot(cv1$size,cv1$dev, type = "b")
                m=which.min(cv1$dev)
                points(cv1$size[m],cv1$dev[m], col="red", cex=2, pch=20)
                # pruned tree
                pruned <- prune.tree(tree1,best=cv1$size[m])
                tree1_pred <- predict(pruned, testing[-53])
                confusionMatrix(tree1_pred, testing$classe)
                
        
        # RF 
            library(caret)
            ctrl <- trainControl(method = "cv", number = 10) 
            grid <- expand.grid(.mtry=c(2,4,6,8,10,12,14,16,18,20))
            rf_model <- train(classe~.,
                                data=training,
                                method="rf",
                                importance=T,
                                tuneGrid = grid,
                                trControl=ctrl)
            print(rf_model$finalModel)
            
            #evaluate performance
            rf_pred <- predict(rf_model, testing)
            confusionMatrix(rf_pred, testing$classe)
            #plot the importance
            importance(rf_model$finalModel)
            varImpPlot(rf_model$finalModel)
            
                #using text code, get importance 
                library(randomForest)
                rf_model_textbook <- randomForest(classe~., data=training, mtry=12, importance=TRUE)
                rf_pred_textbook=predict(rf_model_textbook, testing)
                confusionMatrix(rf_pred_textbook,testing$classe)
                importance(rf_model_textbook)
                # plot the importance 
                varImpPlot(rf_model_textbook)

            
        # bagging
            library(caret);library(randomForest)
            ctrl <- trainControl(method = "cv", number = 10) 
            bag_model <- train(classe~.,
                               data = training,
                               #trControl=ctrl,
                               method="bag")
            bag_pred <- predict(bag_model, testing)
            confusionMatrix(bag_pred, testing$classe)   
          

            # using code from testbook. 
            bag_model <- randomForest(classe~., data=training, mtry=53)
            bag_pred <- predict(bag_model, testing)
            confusionMatrix(bag_pred, testing$classe) 
            
        # boosting,     here caret package help us find the best parameters.
            library(caret)
            ctrl <- trainControl(method = "cv", number = 10) 
            gbm_model <- train(classe~.,
                               data = training,
                               method="gbm",
                               #trControl=ctrl,
                               verbose=F)
            gbm_pred <- predict(gbm_model, testing)
            confusionMatrix(gbm_pred, testing$classe)
            
              # text book without CV, using default parameter. 
              gbm_model_text <- gbm(classe~., 
                                    data=training,
                                    distribution="gaussian",
                                    n.tree=5000,
                                    interaction.depth=4)
              summary(gbm_model_text)
              
              # 
              par(mfrow=c(1,2))
              plot(boost, i='rm')
              plot(boost, i='lstat')
            
        # QDA and LDA
            # LDA
            library(caret)
            ctrl <- trainControl(method = "cv", number = 10) 
            lda_model <- train(classe~.,
                               data = training,
                               method="lda",
                               trControl=ctrl)
            lda_pred <- predict(lda_model, testing)
            confusionMatrix(lda_pred, testing$classe)
            
            # QDA
            library(caret)
            ctrl <- trainControl(method = "cv", number = 10) 
            qda_model <- train(classe~.,
                               data = training,
                               method="qda",
                               trControl=ctrl)
            qda_pred <- predict(qda_model, testing)
            confusionMatrix(qda_pred, testing$classe)
            
        
        # Neural network
            #using Preprossing to standardize the data first. 
            proobj <- preProcess(training[,-53], method = c("center", "scale")) # center and scale the data
            train_body <- predict(proobj, training[,-53])
            test_body <- predict(proobj, testing[,-53])
            nn_training <- cbind(train_body,training[,53])
            nn_testing <- cbind(test_body, testing[53])
            
            # using "caret" package
            library(caret)
            ctrl <- trainControl(method = "cv", number = 10) 
            nn_model <- train(classe~.,
                               data = training,
                               method="nnet",
                               trControl=ctrl)
            nn_pred <- predict(nn_model, testing)
            confusionMatrix(nn_pred, testing$classe)
            
            # using "nnet"package
            library(nnet)
            a = nnet(classe~., data=training,size=5,maxit=10000)
            
            ## EVALUATING MODEL PERFORMANCE
            nn_pred <- predict(a,newdata=nn_testing,type="class")
            print(confusionMatrix(nn_pred, nn_testing[,53]))
            
                ## IMPROVING PERFORMANCE
                # we trying differnet hidden node size =10
                a = nnet(classe~., data=training,size=10,maxit=10000)
                nn_pred <- predict(a,newdata=validation,type="class")
                print(confusionMatrix(nn_pred, validation[,53])) # 0/9324497
                
                # we trying differnet hidden node size=15 
                a = nnet(classe~., data=training,size=15,maxit=10000)
                nn_pred <- predict(a,newdata=validation,type="class")
                print(confusionMatrix(nn_pred, validation[,53])) # 0.9454499
    
            
        

    # bar plot the response
    table(testing$classe)
    library(ggplot2)
    ggplot(testing,aes(x=classe,fill=classe))+geom_bar()
    
    # confusion matrix
    confusionMatrix(knn_pred, testing$classe)$table
    confusionMatrix(rf_pred, testing$classe)$table
    confusionMatrix(lda_pred, testing$classe)$table
    confusionMatrix(qda_pred, testing$classe)$table
    confusionMatrix(tree_pred, testing$classe)$table
    confusionMatrix(bag_pred, testing$classe)$table
    
