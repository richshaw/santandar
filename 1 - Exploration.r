library('data.table')
library('ggplot2')
set.seed(1208)

test <- fread("data/test.csv")
train <- readRDS("data/train.rds")

# Add descriptive colnames
cNames <- c('dateFetched',
'custId',
'employee',
'country',
'sex',
'age',
'dateStart',
'newCustomer',
'customerSeniority',
'primaryCustomer',
'lastDatePrimaryCustomer',
'customerType',
'customerActiveStartMonth',
'resident',
'foreigner',
'employeeSpouse',
'channel',
'deceased',
'addressType',
'provinceCode',
'provinceName',
'customerActive',
'income',
'segment',
'prodSavingsAccount',
'prodGuarantees',
'prodCurrentAcct',
'prodDerividaAcct',
'prodPayrollAcct',
'prodJuniorAcct',
'prodMasParticularAcct',
'prodParticularAcct',
'prodParticularPlusAcct',
'prodShortTermDep',
'pordMedTermDep',
'prodLongTermDep',
'prodEAcct',
'prodFund',
'prodMortage',
'prodPension',
'prodLoan',
'prodTaxes',
'prodCreditCard',
'prodSecurities',
'prodHomeAcct',
'prodPayroll2',
'prodPension2',
'prodDirectDebit')

colnames(train) <- cNames
colnames(test) <- cNames[1:24]

# Describe datasets
str(train)
str(test)

# How many unique custs vs number of records
train.unique.id <- unique(train$custId)
length(train.unique.id)
length(train$custId)

test.unique.id <- unique(test$custId)
length(test.unique.id)
length(test$custId)

# There's no customer history in test data, which rules out predictions based on
# sequences e.g. markov chains

## Age
ggplot(data=train,aes(x=age)) + 
  geom_bar(alpha=0.75,fill="blue",color="black") +
  ggtitle("Age Distribution Train")

ggplot(data=test,aes(x=age)) + 
  geom_bar(alpha=0.75,fill="red",color="black") +
  ggtitle("Age Distribution Test")

# Hmm there are people with very low ages but also very high ages 100+?
# Also distribution is binomial I high number of univerity students + middle ages people
# We also have deceased column, are all the people aged 10+ deceases?

table(train$age[train$age >= 100],train$deceased[train$age >= 100])
prop.table(table(train$age[train$age >= 100],train$deceased[train$age >= 100]),1)

# Hmm apparently the majority of people aren't deceased including some 164 year olds

# Are all under 18's junior account?
table(train$age[train$age <= 19],train$prodJuniorAcct[train$age <= 19])
prop.table(table(train$age[train$age <= 19],train$prodJuniorAcct[train$age <= 19]),1)
# Yep looks like most of the them are ~90%

# NOTE: in some contries you're forced to convert your junior account when you hit a certain age
# Looks like 18 is the start of the cut off here, big drop in junior accounts from 18 - 19 yoa

## Country
table(train$country)
# Vaerity of different countries but strongly skewed to Spain 
prop.table(table(train$country[train$country != 'ES'],train$foreigner[train$country != 'ES']),1)
# People outside of Spain are generally considered "foreign" but that is not always the case

## Sex
ggplot(data=train,aes(x=sex)) + 
  geom_bar(alpha=0.75,fill="blue",color="black") +
  ggtitle("Sex Distribution Train")

ggplot(data=test,aes(x=sex)) + 
  geom_bar(alpha=0.75,fill="red",color="black") +
  ggtitle("Sex Distribution Test")

## Date start
table(train$dateStart)
# NOTE can use this to impute customer senority

## Customer senority
# -999999 rather than NA causes problems
table(train$customerSeniority)
train$customerSeniority[train$customerSeniority == -999999] <- NA
test$customerSeniority[test$customerSeniority == -999999] <- NA

ggplot(data=train,aes(x=customerSeniority)) + 
  geom_bar(alpha=0.75,fill="blue",color="black") +
  ggtitle("Customer Senority Train")

ggplot(data=test,aes(x=customerSeniority)) + 
  geom_bar(alpha=0.75,fill="red",color="black") +
  ggtitle("Customer Senority Test")

# NOTE: Test data isn't as smooth as train data. Also notice spikes roughly
# every 12 months. Create feture based on month.

## Primary customer
prop.table(table(train$primaryCustomer))
prop.table(table(test$primaryCustomer))
# NOTE: 99% of customers are the primary customer on the account. Probably not useful.
# Maybe remove secondary customers to reduce noise.
tbale(train$lastDatePrimaryCustomer)
# Majority blank, not sure what use this is.

## Resident
prop.table(table(train$resident))
# 99% resident
prop.table(table(train$resident,train$foreigner),1)
chisq.test(train$resident,train$foreigner)
# Foreigner == Non-resident, Non-foreigner == Resident

## Employee spouse
table(train$employeeSpouse)
# Mostly blank, probably useless

## Channel 
table(train$channel)
# Alot of blanks, highly dimensional factor
# Some numeric, some three letter code. 
# NOTE: Possibly could be ueful with dimensionality reduction

## Address type
table(train$addressType,useNA = "always")
table(test$addressType,useNA = "always")
# Mostly == 1 some NA, probably useless.
# All vals == 1 in test

## Province code
table(train$provinceCode, useNA = "always")
# NOTE: Some NA, vals are numeric and will need typecasting to factor

## Customer active
prop.table(table(train$customerActive, useNA = "always"))
prop.table(table(test$customerActive, useNA = "always"))
# Some NA in train. Split roughly half active vs inactive

## Income
ggplot(data=train,aes(x=income)) + 
  geom_bar(alpha=0.75,fill="blue",color="black") +
  ggtitle("Income Train")

# NOTE: Continous, will maybe benefit from banding
# Some very wealthy inviduals with a high spike in the middle

ggplot(data=test,aes(x=income)) + 
  geom_bar(alpha=0.75,fill="red",color="black") +
  ggtitle("Income Test")
# Test data has a similar distribution

# Segment
table(train$segment,useNA = 'always')
# Lots of blanks, possibly useful factor

# Products
colSums(train[,25:48])
qplot(colnames(train[,25:48]),colSums(train[,25:48]))
# Almost everyone has a current account
# prodPayroll2 and prodPension2 have no values, exlclude
# prod guarantees are very rare


## Conclusions

# dateFetch = Probably useful, extract month and day as new features
# employee = Probably not useful
# country = Could do with recoding foreign or resident probably more useful
# sex = roughly equal split useful
# age = include need to fix NA try banding
# dateStart = Use to impute missing customer senority?
# newCustomer = Possibly useful
# customerSeniority = Useful need to impute missing value
# primaryCustomer = Probably noy useful
# lastDatePrimaryCustomer = Probbaly not useful, likely triggered after the fact by product change, needs more investigation
# channel = Highly dimensional needs recoding somehow
# deceased = Maybe not useful in itself but could calculate probabily customer is deceased as new feature and see if that correlates to product
# addressType = No use all ones
# provinceCode = Highly dimensional consider clustering
# customerActive= Useful
# income = Useful consider banding
# segment = Useful many blanks




