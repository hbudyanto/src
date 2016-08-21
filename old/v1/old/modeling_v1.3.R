###############################################################################
### R code for Modeling 
### Author : Adi Budyanto
### Date : 9 August 2016

# collaborative filter : https://cran.r-project.org/web/packages/recommenderlab/vignettes/recommenderlab.pdf
# alternate : https://ashokharnal.wordpress.com/2014/12/18/using-recommenderlab-for-predicting-ratings-for-movielens-data/
# alternate : http://www.salemmarafi.com/code/collaborative-filtering-r/

require(data.table)
require(plyr)
#require(futile.logger)
require(xgboost)
library(caret)
library(lubridate)
library(doMC)
registerDoMC(12)

# Set Appropriate Path to the link
setwd("~/PycharmProjects/dissertation/raw_data")

# Load the appropriate data
trans <- read.csv('data/trans.csv')
## Remove all datasets in mainframe except trans
# rm(list=setdiff(ls(), "trans"))

# Create a smaller set based on unique cust id
set.seed(568)
rand = 2500
custid = unique(trans$customer_id)[sample(c(1:length(unique(trans$customer_id))),rand)]
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
test <- subset(key, is2015)

############################## SPLITTING POSITIVE NEGATIVE SAMPLES ########################
### create positive samples :  TARGET 1 
# TRAINING DATA
dataTrain1 = data.table(train)
dataTrain1$product_id = as.character(dataTrain1$product_id)
dataTrain1$key = as.character(dataTrain1$key)

dataTrain1 <- dataTrain1[,c('key', 'product_id'), with=F]
dataTrain1 <- dataTrain1[!duplicated(dataTrain1)]
dataTrain1$target <- rep(1,nrow(dataTrain1))

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

############################## GETTING BACK CUSTOMER ID TO EACH KEY ########################
key = key[,c('customer_id','order_no','order_date','key')]

## TRAINING DATA
featTrain.1 = merge(dataTrain1,key,by=c('key'))
featTrain.0 = merge(dataTrain0,key,by=c('key'))
featTrain.1 = data.frame(featTrain.1)[,c(4:6,2:3)]
featTrain.0 = data.frame(featTrain.0)[,c(4:6,2:3)]

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
dfTest = data.frame(rbind(featTest.1,featTest.0))

# release some memory
rm(list=ls()[grep("feat", ls(), fixed = TRUE)])
rm(list=ls()[grep("data", ls(), fixed = TRUE)])

############################## BEGIN DATA PROCESSING ########################
# Select the final dataset
dTrain <- dfTrain[,c(3,5:61)]
dTest <- dfTest[,c(3,5:61)]
# Drop near-zero variance predictors
dTrain <- noZV(dTrain) # colnameTrain = colnames(dTrain)
dTest <- noZV(dTest)

#dTest  <- dTest[,colnameTrain]
# names(dTrain)
# names(dTest)

# Formatting non-numerical variables as factor (categorical variable)
F= c(2,4:55); for(i in F) dTrain[,i]= factor(dTrain[,i], labels = c('N','Y'))
F= c(2,4:53); for(i in F) dTest[,i]= factor(dTest[,i], labels = c('N','Y'))

# stopifnot(colnames(dTrain)[ncol(dTrain)]==colnames(dTest)[ncol(dTest)])
ifelse(colnames(dTrain)[ncol(dTrain)]==colnames(dTest)[ncol(dTest)],
       print('colnames between Train and Test is match'),
       print('colnames between Train and Test is not match'))

# colnames(dTrain)[F]
# colnames(dTest)[F]
# str(dTrain)
# str(dTest)

# Formating character variable in order_no
F=c(1); for(i in F) dTrain[,i]=as.character(dTrain[,i])
F=c(1); for(i in F) dTest[,i]=as.character(dTest[,i])

options(mc.cores = 10)
colnames(dTrain)<- make.names(colnames(dTrain))
colnames(dTest)<- make.names(colnames(dTest))

# create additional vars for numericals -> transform them to log and rename the variable
for (i in 3:ncol(dTrain)){
  if(is.numeric(dTrain[,i])){
    dTrain = cbind(dTrain,log(dTrain[,i]+1))
    colnames(dTrain)[ncol(dTrain)] <- paste("log",names(dTrain)[i],sep = "")}
}

for (i in 3:ncol(dTest)){
  if(is.numeric(dTest[,i])){
    dTest = cbind(dTest,log(dTest[,i]+1))
    colnames(dTest)[ncol(dTest)] <- paste("log",names(dTest)[i],sep = "")}
}

colnames(dTrain) <- make.names(colnames(dTrain))
colnames(dTest) <- make.names(colnames(dTest))

dTrain <- as.data.frame(dTrain)
dTest <- as.data.frame(dTest)

# Separate variables to follow xgboost specification
response  <- dTrain[,2] 
predictor <- data.matrix(dTrain[,c(3:ncol(dTrain))])

dir.create('features/features.cust.item.random')
write.table(dTrain, paste('features/features.cust.item.random/train.features.cust.item',rand,'.seed',568,'.csv',sep=''), col.names = T, sep = ',')
write.table(dTest,  paste('features/features.cust.item.random/test.features.cust.item',rand,'.seed',568,'.csv',sep=''), col.names = T, sep = ',')

# names(dTrain[,c(3:ncol(dTrain))])

################################################################################
### Extreme Gradient Boosting 

# Pack the training control parameters
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",   # save losses across all models
  classProbs = TRUE,      # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)


# Train XGboost model using caret package
xgbTree_caret <- train(x = predictor,
                       y = response,
                       method = "xgbTree",
                       metric = "ROC",
                       tuneGrid = expand.grid(nrounds = 10*(5:20), #10*(15:50)
                                              eta = c(0.05,0.1,0.5), #c(0.05,0.1,0.5)
                                              max_depth = c(2, 4, 6, 8, 10), #c(0.05,0.1,0.5)
                                              gamma = 0,               #default=0
                                              colsample_bytree = 1,    #default=1
                                              min_child_weight = 1),   #default=1
                       trControl = xgb_trcontrol,
                       maximize = TRUE,
                       subsample = 0.8,
                       verbose = 1,
                       base_score = 0.5,
                       nthread = 10
)

dir.create('features/model')
saveRDS(xgbTree_caret, "features/model/xgbTree_caret.rds")

print(xgbTree_caret)

# Get the result from 
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

# Create new testSet 
# predictionTest <- dTest

############################## BEGIN PREDICT TEST SET ########################
# Check if there any difference in predictors structure
# Caret package cannot predict if the test structure's col != train's colname
missPred <- setdiff(colnames(dTrain), colnames(dTest))
missTest <- data.frame(matrix(0, nrow = nrow(dTest) , ncol = length(missPred)))
colnames(missTest) <- missPred

# Binding missTest back to original Prediction Test
dTest <- cbind(dTest,missTest)
dTest <- dTest[,colnames(dTrain)]
setdiff(colnames(dTrain), colnames(dTest))

############################## BEGIN PREDICT TEST SET - XGBOOST ########################

# Compute prediction Test (XGBOOST)
predRes.xgbTree_caret <- data.frame(order_no = dTest$order_no ,obs = dTest$target)
predRes.xgbTree_caret <- predRes.xgbTree_caret[with(predRes.xgbTree_caret, order(order_no)), ]
predRes.xgbTree_caret$prob <- predict(xgbTree_caret, newdata = 
                                  data.matrix(dTest[,c(3:ncol(dTest))]),type='prob')[,'Y']
predRes.xgbTree_caret$pred <- predict(xgbTree_caret, newdata = data.matrix(dTest[,c(3:ncol(dTest))]))

# Reformating data for interpreting the resutls
predRes.xgbTree_caret$obs = factor(predRes.xgbTree_caret$obs,labels=c(0,1))
predRes.xgbTree_caret$pred = factor(predRes.xgbTree_caret$pred,labels=c(0,1))

predRes.xgbTree_caret$label <- ifelse(predRes.xgbTree_caret$obs == 1,
                                      "True Outcome: Purchase", 
                                      "True Outcome: Did Not Purchase")

### Plot the probability of bad credit
histogram(~prob|label,
          data = predRes.xgbTree_caret,
          layout = c(2, 1),
          nint = 20,
          xlab = "Probability of Purchase",
          type = "count")

### Create the confusion matrix from the test set.
confusionMatrix(data = predRes.xgbTree_caret$pred, 
                reference = predRes.xgbTree_caret$obs)

### ROC curves:
library(pROC)
purchaseROC <- roc(relevel(predRes.xgbTree_caret$obs, "1"), predRes.xgbTree_caret$prob)
coords(purchaseROC, "all")[,1:3]

### Compute ROC and its 95 Confidential Level
auc(purchaseROC)
ci.auc(purchaseROC)


################################################################################
### Penalized Models 
ctrl <- trainControl(method = "LGOCV",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = TRUE)

# lasso, ridge and elastic net by configuring the alpha parameter to 1, 0 or in [0,1]
glmnGrid <- expand.grid(alpha = c(0,  .1), #c(0,  .1,  .2, .4, .6, .8, 1)
                        lambda = seq(.01, .2, length = 5))

# Formatting data
# prediciting
colTest <- colnames(dTest)[3:ncol(dTest)]
response  <- colTest[,2] 
#predictor <- data.matrix(dTrain[,c(3:ncol(dTrain))])
predictor <- data.matrix(dTrain[,colTest])

setdiff(colnames(dTrain[3:ncol(dTrain)]),colTest)
names(dTrain)

set.seed(476)
glmnFit <- train(x = predictor,#dTrain[,-c(1:2)], 
                 y = response,#dTrain$target,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl)

# Get the result from glmnet
print(glmnFit)
result.glmnFit <- glmnFit$result
plot(glmnFit)

# best result AUC achieved
result.glmnFit[which(result.glmnFit$ROC == max(result.glmnFit$ROC)),c(1:8)]

ggplot(glmnFit$results, aes(x = as.factor(alpha), y = lambda, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

############################## BEGIN PREDICT TEST SET ########################
# Create new testSet 
#predictionTest <- dTest

# Check if there any difference in predictors structure
# Caret package cannot predict if the test structure's col != train's colname
missPred <- setdiff(colnames(dTrain), colnames(dTest))
missTest <- data.frame(matrix(0, nrow = nrow(dTest) , ncol = length(missPred)))
colnames(missTest) <- missPred

# Binding missTest back to original Prediction Test
dTest <- cbind(dTest,missTest)
dTest <- predictionTest[,colnames(dTrain)]
setdiff(colnames(dTrain), colnames(dTest))

#stopifnot(length(setdiff(colTrain, colnames(predictionTest)))==0)

# Compute prediction Test (GLMNET)
predRes.glmnFit <- data.frame(order_no = dTest$order_no ,obs = dTest$target)
predRes.glmnFit <- predictResult[with(predRes.glmnFit, order(order_no)), ]
predRes.glmnFit$prob <- predict(glmnFit, newdata = 
                                data.matrix(dTest[,c(3:ncol(dTest))]),type='prob')[,'Y']
predRes.glmnFit$pred <- predict(glmnFit, newdata = data.matrix(dTest[,c(3:ncol(dTest))]))
predRes.glmnFit$label <- ifelse(predRes.glmnFit$obs == 1,
                              "True Outcome: Purchase", 
                              "True Outcome: Did Not Purchase")

# Reformating data for interpreting the resutls
predRes.glmnFit$obs = factor(predRes.glmnFit$obs,labels=c(0,1))
predRes.glmnFit$pred = factor(predRes.glmnFit$pred,labels=c(0,1))
### Plot the probability of bad credit
histogram(~prob|label,
          data = predRes.glmnFit,
          layout = c(2, 1),
          nint = 20,
          xlab = "Probability of Purchase",
          type = "count")

### Create the confusion matrix from the test set.
confusionMatrix(data = predRes.glmnFit$pred, 
                reference = predRes.glmnFit$obs)

### ROC curves:
library(pROC)
purchaseROC <- roc(relevel(predRes.glmnFit$obs, "1"), predRes.glmnFit$prob)
coords(purchaseROC, "all")[,1:3]

### Compute ROC and its 95 Confidential Level
auc(purchaseROC)
ci.auc(purchaseROC)

fun.auc(predRes.xgbTree_caret$prob, predRes.xgbTree_caret$obs)

# Run the function
fun.aucplot(predRes.xgbTree_caret$prob, predRes.xgbTree_caret$obs, "My AUC Plot")


## cool AUC plot : https://davidrroberts.wordpress.com/2015/09/22/quick-auc-function-in-r-with-rocr-package/
