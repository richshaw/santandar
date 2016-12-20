library('data.table')
library('lubridate')
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

income <- train[,(custId,dateFetched,income)]
income$dateFetched <- (income$dateFetched + months(1))
train <- left_join(train,income,by = c('custId','dateFetched'),suffix=c('','_1'))


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

saveRDS(changes,file="data/changes.rds")
saveRDS(train,file="data/trainFeat.rds")


