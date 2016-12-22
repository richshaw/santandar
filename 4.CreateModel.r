library('data.table')
library('xgboost')
library('Matrix') 
library('caret') 
set.seed(1208)

train <- readRDS('data/trainFirst.rds')
train$custId <- NULL
train$dateStart <- NULL
train$dateFetched <- NULL
# Encode
catEncode <- function(data)
{
  for(i in 1:ncol(data))
  {        
    if(class(data[[i]]) %in% c("factor","character"))
    {
        data[[i]] <- as.numeric(as.factor(data[[i]]))
    }
  }
  return(data)
}

trainN <- catEncode(train)
x <- as.matrix(trainN[,1:31])
y <- as.numeric(as.factor(train$response))-1

xgb <- xgboost(data = x, 
               label = y, 
               eval_metric = "mlogloss",
               objective = "multi:softprob",
               nthread = 7,
               num_class = 24,
               nrounds = 10
)

names <- dimnames(data.matrix(x))[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
xgb.plot.importance(importance_matrix[1:10,])

pred <- predict(xgb, data.matrix(x[c(30,156),]))
unique(train$response)
res <- as.data.frame(matrix(pred, ncol = 24, byrow = T))
colnames(res) <- unique(train$response)
