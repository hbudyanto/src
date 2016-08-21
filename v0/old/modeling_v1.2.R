###############################################################################
### R code for Modeling 
### Author : Adi Budyanto
### Date : 9 August 2016

require(data.table)
require(plyr)
#require(futile.logger)
require(xgboost)
library(caret)
library(lubridate)

# Set Appropriate Path to the link
setwd("~/PycharmProjects/dissertation/raw_data")

# Load the appropriate data
trans <- read.csv('data/trans.csv')

# Create a smaller set based on unique cust id
set.seed(568)
custid = unique(trans$customer_id)[sample(c(1:length(unique(trans$customer_id))),1500)]
trans1 = trans[which(trans$customer_id %in% custid),]

# Create a subset of key variables which determines a transaction
# eliminate duplication (caused by left & right eye contact lens)
# formatting back the data
key = trans1[,c('customer_id','order_no','order_date','product_id')]
key = key[!duplicated(key),]
key = key[with(key, order(customer_id)), ]
key$order_date <- as.POSIXct(key$order_date)

# New variable to determine the year of the transaction 
# for the sake of constructing train, valid, test sets
key$is2015 <- year(key$order_date) == 2015

# Store the key to resort the data back to origin form later on
key$key = paste(key$customer_id, key$order_no, sep="")
write.table(key, 'data/keys/customerid_productid.key', row.names = F, col.names = T, sep=',')

### We'll split all of the 2014 data into the training set and a
### portion of the 2015 data as the validation set
train <- subset(key, !is2015)
pre2015 <- 1:nrow(train)
year2015 <- subset(key, is2015)

## Now randomly select some 2015 data for tuning parameters and add it
## back into the existing training data
set.seed(568)
tmp15.custid = unique(year2015$customer_id)
# take 25% proportion of it for validation proses
inTrain = tmp15.custid[sample(1:length(tmp15.custid),as.integer(length(tmp15.custid)*0.25))]
valid <- year2015[ which(year2015$customer_id %in% inTrain),]
test  <- year2015[ which(!(year2015$customer_id %in% inTrain)),]
#training <- rbind(training, training2)

############################## SPLITTING POSITIVE NEGATIVE SAMPLES ########################
### create positive samples :  TARGET 1 
# TRAINING DATA
dataTrain1 = data.table(train)
dataTrain1$product_id = as.character(dataTrain1$product_id)
dataTrain1$key = as.character(dataTrain1$key)

dataTrain1 <- dataTrain1[,c('key', 'product_id'), with=F]
dataTrain1 <- dataTrain1[!duplicated(dataTrain1)]
dataTrain1$target <- rep(1,nrow(dataTrain1))

# VALIDATION DATA
dataVal1 = data.table(train)
dataVal1$product_id = as.character(dataVal1$product_id)
dataVal1$key = as.character(dataVal1$key)

dataVal1 <- dataVal1[,c('key', 'product_id'), with=F]
dataVal1 <- dataVal1[!duplicated(dataVal1)]
dataVal1$target <- rep(1,nrow(dataVal1))

# TESTING DATA
dataTest1 = data.table(test)
dataTest1$product_id = as.character(dataTest1$product_id)
dataTest1$key = as.character(dataTest1$key)

dataTest1 <- dataTest1[,c('key', 'product_id'), with=F]
dataTest1 <- dataTest1[!duplicated(dataTrain1)]
dataTest1$target <- rep(1,nrow(dataTest1))

### Create negative samples :  TARGET 0
# TRAINING DATA
# Set random sampling from top-25 (aman: all missing as negative)
aman <- ddply(dataTrain1, .(product_id), function(x) c(numOrder = nrow(x)))
aman <- aman[with(aman, order(-numOrder)), ]
aman <- aman[1:25,1]

# Create unique list of customer_id 
purchases = unique(dataTrain1$key)
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

# Set k = 3 items to be randomly sampled
k = 3
for (i in 1:length(purchases)){ # k random items for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(aman, k)
}

# Remove purchased items from above list
items <- lapply(1:length(items), function(i){ 
  setdiff(items[[i]], dataTrain1$product_id[which(dataTrain1$key == purchases[i])])
})
reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
dataTrain0 <- data.frame(cbind('key' = rep(purchases, reps), 'product_id'=unlist(items)))
dataTrain0$target <- rep(0,nrow(dataTrain0))

# TESTING DATA
# Set random sampling from top-25 (aman: all missing as negative)
# Should consider for 16 new products to be sampled as well
aman <- ddply(dataTest1, .(product_id), function(x) c(numOrder = nrow(x)))
aman <- aman[with(aman, order(-numOrder)), ]
aman <- aman[1:25,1]

# Create unique list of customer_id 
purchases = unique(dataTest1$key)
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

# Set k = 3 items to be randomly sampled
k = 3
for (i in 1:length(purchases)){ # k random items for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(aman, k)
}

# Remove purchased items from above list
items <- lapply(1:length(items), function(i){ 
  setdiff(items[[i]], dataTest1$product_id[which(dataTest1$key == purchases[i])])
})
reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
dataTest0 <- data.frame(cbind('key' = rep(purchases, reps), 'product_id'=unlist(items)))
dataTest0$target <- rep(0,nrow(dataTest0))

# VALIDATION DATA
# Set random sampling from top-25 (aman: all missing as negative)
# Should consider for 16 new products to be sampled as well
aman <- ddply(dataVal1, .(product_id), function(x) c(numOrder = nrow(x)))
aman <- aman[with(aman, order(-numOrder)), ]
aman <- aman[1:25,1]

# Create unique list of customer_id 
purchases = unique(dataTest1$key)
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

# Set k = 3 items to be randomly sampled
k = 3
for (i in 1:length(purchases)){ # k random items for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(aman, k)
}

# Remove purchased items from above list
items <- lapply(1:length(items), function(i){ 
  setdiff(items[[i]], dataVal1$product_id[which(dataVal1$key == purchases[i])])
})
reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
dataVal0 <- data.frame(cbind('key' = rep(purchases, reps), 'product_id'=unlist(items)))
dataVal0$target <- rep(0,nrow(dataVal0))

############################## GETTING BACK CUSTOMER ID TO EACH KEY ########################
key = key[,c('customer_id','order_no','order_date','key')]

## TRAINING DATA
featTrain.1 = merge(dataTrain1,key,by=c('key'))
featTrain.0 = merge(dataTrain0,key,by=c('key'))
featTrain.1 = data.frame(featTrain.1)[,c(4:6,2:3)]
featTrain.0 = data.frame(featTrain.0)[,c(4:6,2:3)]

## VALIDATION DATA
featVal.1 = merge(dataVal1,key,by=c('key'))
featVal.0 = merge(dataVal0,key,by=c('key'))
featVal.1 = data.frame(featVal.1)[,c(4:6,2:3)]
featVal.0 = data.frame(featVal.0)[,c(4:6,2:3)]

## TESTING DATA
featTest.1 = merge(dataTest1,key,by=c('key'))
featTest.0 = merge(dataTest0,key,by=c('key'))
featTest.1 = data.frame(featTest.1)[,c(4:6,2:3)]
featTest.0 = data.frame(featTest.0)[,c(4:6,2:3)]

########################### ENGINEERING FEATURES #######################
## CUSTOMER DATA
feat.cust.global <- read.csv('features/features.cust.global.csv')
# Selectively choose variables for modeling purpose
feat.cust.global <- feat.cust.global[,c(1,14:24)]
## ITEM DATA
feat.item.global <- read.csv('features/features.item.global.csv')

## TRAINING DATA
featTrain.1 = merge(featTrain.1,feat.cust.global, by=c('customer_id'))
featTrain.1 = merge(featTrain.1,feat.item.global, by=c('product_id'))

featTrain.0 = merge(featTrain.0,feat.cust.global, by=c('customer_id'))
featTrain.0 = merge(featTrain.0,feat.item.global, by=c('product_id'))

## TESTING DATA
featTest.1 = merge(featTest.1,feat.cust.global, by=c('customer_id'))
featTest.1 = merge(featTest.1,feat.item.global, by=c('product_id'))

featTest.0 = merge(featTest.0,feat.cust.global, by=c('customer_id'))
featTest.0 = merge(featTest.0,feat.item.global, by=c('product_id'))

dfTrain = data.frame(rbind(featTrain.1,featTrain.0))
dTest = data.frame(rbind(featTest.1,featTest.0))

# release some memory
rm(list=ls()[grep("feat", ls(), fixed = TRUE)])
rm(list=ls()[grep("data", ls(), fixed = TRUE)])


############################## BEGIN DATA PROCESSING ########################
dTrain <- dfTrain[,c(3,5:61)]
dTrain<- noZV(dTrain)
str(dTrain)
colnames(dTrain)[F]
names(dTrain)

# formatting factor
F=c(2,4:56)
#F=c(7:59)
#F=c(2)
for(i in F) dTrain[,i]= factor(dTrain[,i], labels = c('N','Y'))
#for(i in F) dTrain[,i]= factor(dTrain[,i], labels = unique(dTrain[,i]))

## Check levelling before
# levels(dTrain[,2])
# make.names(levels(dTrain[,2]))

# train.matrix = as.matrix(dTrain[,2])
# mode(train.matrix) = "numeric"

# formatting character
F=c(1)
for(i in F) dTrain[,i]=as.character(dTrain[,i])

options(mc.cores = 10)
colnames(dTrain)<- make.names(colnames(dTrain))

# create additional vars for numericals -> transform them to log and rename the variable
for (i in 3:ncol(dTrain)){
  if(is.numeric(dTrain[,i])){
    dTrain = cbind(dTrain,log(dTrain[,i]+1))
    colnames(dTrain)[ncol(dTrain)] <- paste("log",names(dTrain)[i],sep = "")}
}
colnames(dTrain)<- make.names(colnames(dTrain))
dTrain <- as.data.frame(dTrain)

response  <- dTrain[,2] 
predictor <- data.matrix(dTrain[,c(3:57)])


str(predictor)
str(response)
# Training Dataset

# pack the training control parameters
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


xgbTree_caret <- train(x = predictor,
                       y = response,
                       method = "xgbTree",
                       metric = "ROC",
                       tuneGrid = expand.grid(nrounds = 10*(5:20), #10*(15:50)
                                              eta = c(0.05,0.1,0.5),
                                              max_depth = c(2, 4, 6, 8, 10),
                                              gamma = 0,               #default=0
                                              colsample_bytree = 1,    #default=1
                                              min_child_weight = 1),   #default=1
                       trControl = xgb_trcontrol,
                       maximize = TRUE,
                       subsample = 0.8,
                       verbose = 1,
                       #colsample_bytree =0.8,
                       base_score = 0.5,
                       nthread = 10
)

result.xgbTree_caret = xgbTree_caret$results
plot(xgbTree_caret)

# best result AUC achieved
result.xgbTree_caret[which(result.xgbTree_caret$ROC == max(result.xgbTree_caret$ROC)),c(1:9)]

## scatter plot of the AUC against max_depth and eta
ggplot(xgbTree_caret$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")


#########
# resource :
# 1. AUC Plot Visualisation    : http://goo.gl/fg8mPV
# 2. Check Class Probabilities : http://goo.gl/NDmWqE
# 3. Compare various model     : http://goo.gl/5iaYvb
# 4. XGBoost Input must be numeric : http://goo.gl/Me7DZf
# 5. Tune XGBoost Article : http://goo.gl/jhKWGl
# 6. Great Way Tuning XGBoost : https://goo.gl/YzZhvo
# -> optunity package



