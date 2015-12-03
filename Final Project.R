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
                         "cvtd_timestamp", "num_window")
        train_pml <- train_pml[, !names(train_pml) %in% excludecols]
        
        # CLEANING
        rm(test_pml,excludecols,na,nsv)
        
    # DATA SPLITING
        library(caret); set.seed(1001)
        intrain <- createDataPartition(y=train_pml$classe, p=0.35, list = F)
        training <- train_pml[intrain,]; testing <- train_pml[-intrain,]


## TRAIN MODELS
        # KNN
            #using Preprossing to standardize the data first. 
            proobj <- preProcess(training[,-53], method = c("center", "scale")) # center and scale the data
            #train_body <- predict(proobj, training[,-53])
            test_body <- predict(proobj, testing[,-53])
            #knn_training <- cbind(train_body,training[,53])
            
            ## to create a control object named ctrl that uses 10-fold CV and the oneSE secetion function. 
            ctrl <- trainControl(method = "cv", number = 10) 
            knn_model <- train(classe~., 
                               data=training, 
                               method="knn", 
                               preProcess=c("center","scale"),
                               tuneLength = 20,
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
            rf_model <- train(classe~.,
                                data=training,
                                method="rf",
                                trControl=ctrl)
            print(rf_model$finalModel)
            
            #evaluate performance
            rf_pred <- predict(rf_model, testing)
            confusionMatrix(rf_pred, testing$classe)

            
        # bagging
            
        # boosting
            gbm_model <- train(classe~.,
                               data = training,
                               method="gbm",
                               trControl=ctrl)
            gbm_pred <- predict(gbm_model, testing)
            confusionMatrix(gbm_pred, testing$classe)
            
        # QDA and LDA
            # LDA
            lda_model <- train(classe~.,
                               data = training,
                               method="lda",
                               trControl=ctrl)
            lda_pred <- predict(lda_model, testing)
            confusionMatrix(lda_pred, testing$classe)
            
            # QDA
            qda_model <- train(classe~.,
                               data = training,
                               method="qda",
                               trControl=ctrl)
            qda_pred <- predict(qda_model, testing)
            confusionMatrix(qda_pred, testing$classe)
            
        # NB method
            nb_model <- train(classe~.,
                               data = training,
                               method="nb",
                               trControl=ctrl)
            nb_pred <- predict(nb_model, testing)
            confusionMatrix(nb_pred, testing$classe)
        
        # Neural network





