library(h2o)
library(dplyr)

## Create dir if needed
ifelse(!dir.exists('models'), dir.create('models'), FALSE)

createModels <- function(data, n = 1, ratios=0.05) {
  
  ## Build x Random Deep Learning Models
  for (i in 1:n) {
    
    train <- h2o.splitFrame(data=data, ratios=ratios)
    
    # Create a training set from the 1st dataset in the split
    train <- train[[1]]
    
    rand_ntrees <- sample(1:1000,1)
    rand_max_depth <- sample(1:15,1)
    rand_nbins <- sample(2:40,1)
    rand_balance_classes <- as.logical(sample(0:1,1))
    rand_binomial_double_trees <- as.logical(sample(0:1,1))
    rand_nfolds <- sample(2:500,1)
    rand_stopping_rounds <- sample(1:5,1)
    rand_stopping_tolerance <- runif(1, 0, 1e-2)
    rand_stopping_metric <- c("logloss","AUC", "misclassification")[sample(1:3,1)]
    
    
    fit <- h2o.randomForest(
      training_frame=train,
      x=names(train), 
      y='Response',
      seed = 1208,
      
      ### Random parameters
      ntrees = rand_ntrees,
      max_depth = rand_max_depth,
      nbins = rand_nbins,
      nfolds=rand_nfolds,
      stopping_metric=rand_stopping_metric,
      stopping_rounds=rand_stopping_rounds,
      stopping_tolerance=rand_stopping_tolerance,
      balance_classes=rand_balance_classes,
      binomial_double_trees=rand_binomial_double_trees
    )                                
    
    h2o.saveModel(fit, path = "models/", force = FALSE)
    
    params <- fit@allparameters
    params$x <- paste(params$x,collapse=" ")
    params$y <- paste(params$y,collapse=" ")
    params$id <- fit@model_id
    
    write.table(as.data.frame(params),file="modelParams.csv", quote=F,sep=",",row.names=F,col.names=F,append=T)
    
    ## Clean up
    # h2o.removeAll()
    originalTrainId <- h2o.getId(data)
    for (item in h2o.ls()$key) {
      if (as.character(item) != originalTrainId) {
        h2o.rm(item)
      }
    }
    
  }
}

localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '14g', nthreads = 7)
train <- h2o.importFile("data/trainFeat.hex")
# Savings acct
train$Response <- train$prodSavingsAccount
train <- train[,c(1:19,44:59)] # Only relevant cols

createModels(train,10,0.03)

h2o.shutdown(prompt = FALSE)
