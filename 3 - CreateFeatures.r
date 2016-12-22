library('data.table')
library('lubridate')
library('tidyr')
set.seed(1208)


custId <- unique(train$custId)
dates <- unique(train$dateFetched)
products <- colnames(train[,21:44])
indVars <- colnames(train[,3:20])
# Change rows
changes <- list()
for(p in products) {
  for(i in 1:length(dates)) {
    changes[[p]] <- c(changes[[p]],which(train[[p]][train$dateFetched == dates[i]] != train[[p]][train$dateFetched == dates[i+1]])) 
  }
}

past<- function(data,var,n = 3) {
  v <- c('custId','dateFetched',var)
  t <- data[,mget(v)]
  t$dateOrg <- t$dateFetched
  setkey(data,custId,dateFetched)
  setkey(t,custId,dateFetched)
  
  for (i in 1:n) {
    t$dateFetched <- (t$dateOrg + months(i))
    data <- merge(data,t[,mget(v)],by=c('custId','dateFetched'),all.x = TRUE,suffixes = c('',paste0('_',i)))
  }
  data
}

train <- past(train,'income')
train <- past(train,'age')

# Month
train$dataFetchedMonth <- month(train$dateFetched)
# Age banding
train$ageBand <- NA
train$ageBand[train$age >= 19 & train$age < 29] <- 'young adult'
train$ageBand[train$age >= 29 & train$age < 60] <- 'adult'
train$ageBand[train$age >= 60] <- 'oap'

#Income percentage change
train[,income_1_chg := (income_1 - income) / income * 100]
train[,income_2_chg := (income_2 - income_1) / income_1 * 100]
train[,income_3_chg := (income_3 - income_2) / income_2 * 100]

#Age change
train[,age_1_chg := age == age_1]
train[,age_2_chg := age_1 == age_2]
train[,age_3_chg := age_2 == age_3]

setkey(train,custId,dateFetched)

nm1 <- colnames(train[,20:43])
nm2 <- paste("lead", nm1, sep=".")
train <- train[, (nm2) :=  shift(.SD, type="lead"), by=custId, .SDcols=nm1]

train[,20:43] <- NULL

for (j in 35:58) {
  train[which(train[[j]] == FALSE),j] <- NA
}

train <- melt(train,id.vars = c(1:34),measure.vars = c(35:58),na.rm = TRUE)
trainFirst <- train[!duplicated(train[,c(1,35,36)]),]

train$value <- NULL
train$response <- train$variable
train$variable <- NULL

saveRDS(changes,file="data/changes.rds")
saveRDS(train,file="data/trainFeat.rds")
saveRDS(trainFirst,file="data/trainFirst.rds")


