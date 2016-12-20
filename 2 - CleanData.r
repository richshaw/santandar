library('data.table')
library('dplyr')
library('h2o')
set.seed(1208)

blankToNA <- function(data)
{
  for(i in 1:ncol(data))
  {        
    if(class(data[[i]]) %in% c("character"))
    {
      if(sum(data[[i]] == ''))
      {
        cat(".")
        data[data[[i]] == '',i] <- NA
      }
    }
  }
  return(data)
}

# Get rid of blanks
train <- blankToNA(train)
test <- blankToNA(test)
# And pesky -99999
train$customerSeniority[train$customerSeniority == -999999] <- NA
test$customerSeniority[test$customerSeniority == -999999] <- NA
# Make dates dates
train$dateFetched <- as.POSIXct(strptime(train$dateFetched,format="%Y-%m-%d"))
train$dateStart <- as.POSIXct(strptime(train$dateStart,format="%Y-%m-%d"))

test$dateFetched <- as.POSIXct(strptime(test$dateFetched,format="%Y-%m-%d"))
test$dateStart <- as.POSIXct(strptime(test$dateStart,format="%Y-%m-%d"))

# Age median impute
train$age[is.na(train$age)] <- median(train$age,na.rm = TRUE)
train$age <- round(train$age)

# Sex make unknown
train$sex[is.na(train$sex)] <- 'Unknown'

# Customer senority
table(train$customerSeniority,useNA = 'always')
# Can't impute customer senority from start date..
table(train$dateStart[is.na(train$customerSeniority)],useNA = 'always')
# Use median instead
train$customerSeniority[is.na(train$customerSeniority)] <- median(train$customerSeniority,na.rm = TRUE)
train$customerSeniority <- round(train$customerSeniority)

# Primary customer impute with most common
train$primaryCustomer[is.na(train$primaryCustomer)] <- 1

# Customer type
# Customer type at the beginning of the month ,1 (First/Primary customer), 2
# (co-owner ),P (Potential),3 (former primary), 4(former co-owner)
# Fix mis-codes, dataset currently has 1.0 and 1 as same category
train$customerType[train$customerType == '1.0'] <- 1
train$customerType[train$customerType == '2.0'] <- 2
train$customerType[train$customerType == '3.0'] <- 3
train$customerType[train$customerType == '4.0'] <- 4
# Majority of NA likely primary
table(train$primaryCustomer[is.na(train$customerType)],train$customerType[is.na(train$customerType)],useNA = 'always')
train$customerType[is.na(train$customerType)] <- 1

# Active at start of month
# TODO If important var could probably better impute this by looking at neighbouring months
train$customerActiveStartMonth[is.na(train$customerActiveStartMonth)] <- 'Unknown'

# Resident / foreigner mirror each other
table(train$resident,train$foreigner,useNA = 'always')
# There's a bunch of values that just look bad, lets remove them
bad <- is.na(train$resident) & is.na(train$income)
train <- train[!bad,]

# Employee spouse
# Very unlikely to be spouse
train$employeeSpouse[is.na(train$employeeSpouse)] <- 'N'

# Channel
# Hmms this is a mess needs some sort of dimensonality reduction
train$channel[is.na(train$channel)] <- 'Unknown'

# Proviince Name
train$provinceName[is.na(train$provinceName)] <- 'Unknown'

# Income, probably super important so lets spend sometime to impute the NA's
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, max_mem_size = '14g', nthreads = -1)

#H20 Doesn't understand POSIXct
train$dateFetched <- as.Date(train$dateFetched)
train$dateStart <- as.Date(train$dateStart)

incomeTrain <- as.h2o(train[!is.na(train$income),])

incomeTrain$country <- as.factor(incomeTrain$country)
incomeTrain$sex <- as.factor(incomeTrain$sex)
incomeTrain$provinceName <- as.factor(incomeTrain$provinceName)
incomeTrain$addressType <- as.factor(incomeTrain$addressType)
incomeTrain$country <- as.factor(incomeTrain$country)

x <- c("country","sex","age","newCustomer","customerSeniority","resident","deceased","addressType","provinceName","customerActive","segment" )
y <- "income"

incomeModel <- h2o.randomForest(x,y,incomeTrain)

rm(incomeTrain)

incomePredict <- as.h2o(train[is.na(train$income),])

incomePredict$country <- as.factor(incomePredict$country)
incomePredict$sex <- as.factor(incomePredict$sex)
incomePredict$provinceName <- as.factor(incomePredict$provinceName)
incomePredict$addressType <- as.factor(incomePredict$addressType)
incomePredict$country <- as.factor(incomePredict$country)

incomeMissing <- h2o.predict(object = incomeModel, newdata = incomePredict)
train$income[is.na(train$income)] <- as.vector(incomeMissing)
h2o.shutdown()

# Segment 
train$segment[is.na(train$segment)] <- 'Unknown'

# Impute to most common 
train$prodPayroll2[is.na(train$prodPayroll2)] <- 0
train$prodPension2[is.na(train$prodPension2)] <- 0

# Drop unhelpful columns
# Address Type , all == 1 drop column
# Province code drop because we can use provience name
# Resident = Just a repeat of foreign
train$addressType <- NULL
train$provinceCode <- NULL
train$resident <- NULL
# 99% NA
train$lastDatePrimaryCustomer <- NULL

# Sanity check
which(sapply(train,function(x)any(is.na(x))))

# Fix types
train$dateFetched <- as.Date(train$dateFetched)
train$dateStart <- as.Date(train$dateStart)

train$custId <- as.factor(train$custId)
train$sex <- as.factor(train$sex)
train$provinceName <- as.factor(train$provinceName)
train$country <- as.factor(train$country)
train$employee <- as.factor(train$employee)
train$customerType <- as.factor(train$customerType)
train$customerActiveStartMonth <- as.factor(train$customerActiveStartMonth)
train$channel <- as.factor(train$channel)
train$segment <- as.factor(train$segment)

train$foreigner[train$foreigner == 'N'] <- FALSE
train$foreigner[train$foreigner == 'S'] <- TRUE
train$foreigner <- as.logical(train$foreigner)

train$employeeSpouse[train$employeeSpouse == 'N'] <- FALSE
train$employeeSpouse[train$employeeSpouse == 'S'] <- TRUE
train$employeeSpouse <- as.logical(train$employeeSpouse)

train$deceased[train$deceased == 'N'] <- FALSE
train$deceased[train$deceased == 'S'] <- TRUE
train$deceased <- as.logical(train$deceased)

train$customerActive <- as.logical(train$customerActive)
train$newCustomer <- as.logical(train$newCustomer)
train$primaryCustomer <- as.logical(train$primaryCustomer)

for(i in 21:ncol(train))
{        
  train[[i]] <- as.logical(train[[i]])
}

# Save
saveRDS(train,file="data/trainClean.rds")




