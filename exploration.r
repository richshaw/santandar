#load("data/train.rdata")
train <- readRDS("data/trainWIP.rds")
length(unique(train$fecha_dato)) # = 17 months

# How many customers do we have full data for?
cust_records <- table(train$ncodpers)
cust_records <- data.table(cust_records)
cust_records <- cust_records[cust_records$N == 17,]

# Filter out partial records
train <- train[train$ncodpers %in% cust_records$V1,]
saveRDS(train,file="data/train_fullrec.rdata")


# Lets collapse product choice into single var
train$prodCode <- strtoi(apply(train[,25:48],1,paste,collapse=""),2)

# One product used by almost every body
barplot(colSums(train[,25:48]))

# Clean up
train$fecha_dato <- as.factor(train$fecha_dato)
train$prodCode <- as.factor(train$prodCode)

# Make small training dataset
# Any NA data
str(train[complete.cases(train),])

# Some NA,lets filter out those as well
train <- train[complete.cases(train),]

sIds <- sample(unique(train$ncodpers),100000)
train <- train[train$ncodpers %in% sIds,]

colnames(train.h2o)
x <- c(5,6)
y <- 49 

model <- h2o.randomForest(x,y,train.h2o)


