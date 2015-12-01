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
        intrain <- createDataPartition(y=train_pml$classe, p=0.75, list = F)
        training <- train_pml[intrain,]; testing <- train_pml[-intrain,]


## TRAIN MODELS
        # KNN
            #using Preprossing to standardize the data first. 
            #proobj <- preProcess(training[,-53], method = c("center", "scale")) # center and scale the data
            #train_body <- predict(proobj, training[,-53])
            #test_body <- predict(proobj, testing[,-53])
            #knn_training <- cbind(train_body,training[,53])
            
            ## to create a control object named ctrl that uses 10-fold CV and the oneSE secetion function. 
            ctrl <- trainControl(method = "cv", number = 10) 
            knn_model <- train(classe~., 
                               data=training, 
                               method="knn", 
                               preProcess=c("center","scale")) #trControl=ctrl














