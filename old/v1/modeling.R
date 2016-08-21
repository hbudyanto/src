###############################################################################
### R code for Modeling 
### Author : Adi Budyanto
### Date : 10 August 2016

require(data.table)
require(plyr)
#require(futile.logger)
require(xgboost)
library(caret)
library(lubridate)
library(doMC)
registerDoMC(12)
library(pROC)

# Run function batch code
#source('~/Dropbox/dissertation/src/func.R')
source('~/PycharmProjects/dissertation/src/v1/func_v2.R')

# Set Appropriate Path to the link
#setwd("~/Dropbox/dissertation/raw_data")
setwd("~/PycharmProjects/dissertation/raw_data")
#setwd("~/PycharmProjects/dissertation/dropbox")


# Load the appropriate data
trans <- read.csv('data/trans.csv')
## Remove all datasets in mainframe except trans
rm(list=setdiff(ls(), "trans"))

# Create a smaller set based on unique cust id
set.seed(568)

rand = 500 # default : at least 10,000 custs
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
# # Should consider for 16 new products to be sampled as well
# aman <- ddply(dataTest1, .(product_id), function(x) c(numOrder = nrow(x)))
# aman <- aman[with(aman, order(-numOrder)), ]
# aman <- aman[1:25,1]

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

# dTest  <- dTest[,colnameTrain]
# names(dTrain)
# names(dTest)

maxColTrain <- which(colnames(dTrain) == 'brandbrandrenu')
maxColTest <- which(colnames(dTest) == 'brandbrandrenu')

# Formatting non-numerical variables as factor (categorical variable)
F= c(2,4:maxColTrain); for(i in F) dTrain[,i]= factor(dTrain[,i], labels = c('N','Y'))
F= c(2,4:maxColTest); for(i in F) dTest[,i]= factor(dTest[,i], labels = c('N','Y'))

stopifnot(length(dTrain)==length(dTest))
setdiff(colnames(dTrain), colnames(dTest))

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

dir.create('features/features.cust.item.random')
write.table(dTrain, paste('features/features.cust.item.random/train.features.cust.item',rand,'.seed',568,'.csv',sep=''), col.names = T, sep = ',')
write.table(dTest,  paste('features/features.cust.item.random/test.features.cust.item',rand,'.seed',568,'.csv',sep=''), col.names = T, sep = ',')

# Store as Rdata data fike
colTrain <- colnames(dTrain)
colTest <- colnames(dTest)

save(dTrain,dTest,dfTrain,dfTest, file = paste('features/features.cust.item.random/purchaseData',rand,'.seed',568,'.rda',sep=''))
save(colTrain,colTest, file= paste('features/features.cust.item.random/purchaseVector',rand,'.seed',568,'.rda',sep=''))

######
# dTrain <- data.frame(fread(paste('features/features.cust.item.random/train.features.cust.item',rand,'.seed',568,'.csv',sep='')))
# dTest <- data.frame(fread(paste('features/features.cust.item.random/tesr.features.cust.item',rand,'.seed',568,'.csv',sep='')))
# rand = 500
# load(paste('features/features.cust.item.random/purchaseVector',rand,'.seed',568,'.rda',sep=''))
# load(paste('features/features.cust.item.random/purchaseData',rand,'.seed',568,'.rda',sep=''))
# str(dTrain)
# str(dTest)
####

############################## BEGIN MODELLING PROCESS #########################

# Formatting data
response  <- dTrain[,2] 
predictor <- data.matrix(dTrain[,c(3:ncol(dTrain))])

#### Save Image
dir.create('image')

#### Save the model to disk
dir.create('model')

################################################################################
### Model : Extreme Gradient Boosting

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
set.seed(568)
xgbTree_caret <- train(x = xgbTree_caret,
                       y = response,
                       method = "xgbTree",
                       metric = "ROC",
                       tuneGrid = expand.grid(nrounds = 10*(30:50), #10*(15:50)
                                              eta = c(0.1, 0.2, 0.4, .6), #c(0.1, 0.2, 0.4, 0.6, 0.8, 1)
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
print(xgbTree_caret)
# Save Model
saveRDS(xgbTree_caret, paste('model/xgbTree_caret.rds',rand,'.seed',568,'.rds',sep=''))
####### 
# xgbTree_caret <- readRDS(paste('model/xgbTree_caret.rds',rand,'.seed',568,'.rds',sep=''))
####### 

# Get confusion matrix
xgbTree_caret2014 <- merge(xgbTree_caret$pred,  xgbTree_caret$bestTune)
xgbTree_caretCM <- confusionMatrix(xgbTree_caret2014, norm = "none")
xgbTree_caretCM

# List the predictors were used in the model (pg 325)
predictors(xgbTree_caret)

# List importance predictors 
importance.xgbTree_caret <- varImp(xgbTree_caret)[[1]]
importance.xgbTree_caret$predictors <- rownames(importance.xgbTree_caret)
rownames(importance.xgbTree_caret) <- NULL
# export to csv
write.csv(importance.xgbTree_caret, file = paste('image/importance.xgbTree_caret',rand,'.seed',568,'.csv',sep=''))

# Get the result from 
result.xgbTree_caret = xgbTree_caret$results

# best result AUC achieved
result.xgbTree_caret[which(result.xgbTree_caret$ROC == max(result.xgbTree_caret$ROC)),c(1:9)]

### xgboostModel Predictions and Performance
# Make predictions using the test data set http://blog.revolutionanalytics.com/2016/05/using-caret-to-compare-models.html
xgb.pred <- predict(xgbTree_caret,predictor)

#Look at the confusion matrix  
confusionMatrix(xgb.pred,dTrain$target)   

#Draw the ROC curve 
xgb.probs <- predict(xgbTree_caret,predictor,type="prob")
#head(xgb.probs)
xgb.ROC <- roc(predictor=xgb.probs$Y,
               response=dTrain$target,
               levels=rev(levels(dTrain$target)))

xgb.ROC$auc
# Area under the curve: 0.8857

plot(xgb.ROC,main="xgboost ROC")

# Create Plot
png(filename=paste('image/xgbTree_caret_grid_tune.plot',rand,'.seed',568,'.png',sep=''))
plot(xgbTree_caret)
dev.off()

## scatter plot of the AUC against max_depth and eta
png(filename=paste('image/xgbTree_caret_AUC_against_maxdepth_eta.plot',rand,'.seed',568,'.png',sep=''))
ggplot(xgbTree_caret$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")
dev.off()



#########
# resource :
# 1. AUC Plot Visualisation    : http://goo.gl/fg8mPV
# 2. Check Class Probabilities : http://goo.gl/NDmWqE
# 3. Compare various model     : http://goo.gl/5iaYvb
# 4. XGBoost Input must be numeric : http://goo.gl/Me7DZf
# 5. Tune XGBoost Article : http://goo.gl/jhKWGl
# 6. Great Way Tuning XGBoost : https://goo.gl/YzZhvo
# -> optunity package

################################################################################
### Model : Neural Networks
ctrl <- trainControl( method = "cv",
                      number = 5,  # default 5
                      verboseIter = TRUE,
                      returnData = FALSE,
                      returnResamp = "all",   # save losses across all models
                      classProbs = TRUE,      # set to TRUE for AUC to be computed
                      summaryFunction = twoClassSummary,
                      allowParallel = TRUE,
                      savePredictions = TRUE)

nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$size)
fullSet <- 3:ncol(dTrain)

# ctrl <- trainControl(method = "LGOCV",
#                      summaryFunction = twoClassSummary,
#                      classProbs = TRUE,
#                      savePredictions = TRUE)

set.seed(568)
nnetFit <- train(x = predictor, 
                 y = response,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = 1*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                 trControl = ctrl)
print(nnetFit)
# Save Model
saveRDS(nnetFit, paste('model/nnetFit.rds',rand,'.seed',568,'.rds',sep=''))
####### 
# nnetFit <- readRDS(paste('model/nnetFit.rds',rand,'.seed',568,'.rds',sep=''))
####### 
set.seed(568)
nnetFit2 <- train(x = predictor, 
                  y = response,
                  method = "nnet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 1*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  trControl = ctrl)
print(nnetFit2)
# Save Model
saveRDS(nnetFit2, paste('model/nnetFit2.rds',rand,'.seed',568,'.rds',sep=''))
####### 
nnetFit2 <- readRDS(paste('model/nnetFit2.rds',rand,'.seed',568,'.rds',sep=''))
####### 

nnetGrid$bag <- FALSE

set.seed(476)
nnetFit3 <- train(x = predictor, 
                  y = response,
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  tuneGrid = nnetGrid,
                  repeats = 10,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 10*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  allowParallel = FALSE, ## this will cause to many workers to be launched.
                  trControl = ctrl)
print(nnetFit3)
saveRDS(nnetFit3, paste('model/nnetFit3.rds',rand,'.seed',568,'.rds',sep=''))
####### 
# nnetFit3 <- readRDS(paste('model/nnetFit3.rds',rand,'.seed',568,'.rds',sep=''))
####### 

set.seed(476)
nnetFit4 <- train(x = predictor, 
                  y = response,
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  repeats = 10,
                  MaxNWts = 10*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  allowParallel = FALSE, 
                  trControl = ctrl)
print(nnetFit4)
saveRDS(nnetFit4, paste('model/nnetFit4.rds',rand,'.seed',568,'.rds',sep=''))
#######
nnetFit4 <- readRDS(paste('model/nnetFit4.rds',rand,'.seed',568,'.rds',sep=''))
#######

# rules about MaxNWts : http://stackoverflow.com/questions/25668873/error-when-using-neural-networks-caret-package

# Get the result from 
result.nnetFit4 = nnetFit4$results

# best result AUC achieved
result.nnetFit4[which(result.nnetFit4$ROC == max(result.nnetFit4$ROC)),c(1:8)]

nnetFit4$pred <- merge(nnetFit4$pred,  nnetFit4$bestTune)
nnetCM <- confusionMatrix(nnetFit4, norm = "none")
nnetCM

nnet1 <- nnetFit$results
nnet1$Transform <- "No Transformation"
nnet1$Model <- "Single Model"

nnet2 <- nnetFit2$results
nnet2$Transform <- "Spatial Sign"
nnet2$Model <- "Single Model"

nnet3 <- nnetFit3$results
nnet3$Transform <- "No Transformation"
nnet3$Model <- "Model Averaging"
nnet3$bag <- NULL

nnet4 <- nnetFit4$results
nnet4$Transform <- "Spatial Sign"
nnet4$Model <- "Model Averaging"
nnet4$bag <- NULL

nnetResults <- rbind(nnet1, nnet2, nnet3, nnet4)
nnetResults$Model <- factor(as.character(nnetResults$Model),
                            levels = c("Single Model", "Model Averaging"))
library(latticeExtra)
  png(filename=paste('image/nnet_AUC.plot',rand,'.seed',568,'.png',sep=''))
  useOuterStrips(
    xyplot(ROC ~ size|Model*Transform,
           data = nnetResults,
           groups = decay,
           as.table = TRUE,
           type = c("p", "l", "g"),
           lty = 1,
           ylab = "ROC AUC (2008 Hold-Out Data)",
           xlab = "Number of Hidden Units",
           auto.key = list(columns = 4, 
                           title = "Weight Decay", 
                           cex.title = 1)))
  dev.off()

nnetRoc <- roc(response = nnetFit4$pred$obs,
               predictor = nnetFit4$pred$Y,
               levels = rev(levels(nnetFit4$pred$obs)))
plot(nnetRoc, type = "s", legacy.axes = TRUE)

################################################################################
### Model : Penalized Regression Logistics

# lasso, ridge and elastic net by configuring the alpha parameter to 1, 0 or in [0,1]
glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), #c(0,  .1,  .2, .4, .6, .8, 1)
                        lambda = seq(.01, .2, length = 50))

set.seed(476)
glmnFit <- train(x = predictor,#dTrain[,-c(1:2)], 
                 y = response,#dTrain$target,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl)
print(glmnFit)
saveRDS(glmnFit, paste('model/glmnFit.rds',rand,'.seed',568,'.rds',sep=''))
#######
glmnFit <- readRDS(paste('model/glmnFit.rds',rand,'.seed',568,'.rds',sep=''))
#######

# Get the result from glmnet
print(glmnFit)
result.glmnFit <- glmnFit$result

# best result AUC achieved
result.glmnFit[which(result.glmnFit$ROC == max(result.glmnFit$ROC)),c(1:8)]

# Get Plot of AUC against alpha and lambda
png(filename=paste('image/glmnFit_grid_tune.plot',rand,'.seed',568,'.png',sep=''))
plot(glmnFit)
dev.off()

# Get Plot of AUC against alpha and lambda (bubble chart)
png(filename=paste('image/glmnFit_AUC_against_alpha_lambda.plot',rand,'.seed',568,'.png',sep=''))
ggplot(glmnFit$results, aes(x = as.factor(alpha), y = lambda, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")
dev.off()

# Get confusion matrix
glmnet2014 <- merge(glmnFit$pred,  glmnFit$bestTune)
glmnetCM <- confusionMatrix(glmnFit, norm = "none")
glmnetCM

glmnetRoc <- roc(response = glmnet2014$obs,
                 predictor = glmnet2014$Y,
                 levels = rev(levels(glmnet2014$obs)))


glmnFit0 <- glmnFit
glmnFit0$results$lambda <- format(round(glmnFit0$results$lambda, 3))

glmnPlot <- plot(glmnFit0,
                 plotType = "level",
                 cuts = 15,
                 scales = list(x = list(rot = 90, cex = .65)))

png(filename=paste('image/glmnplot_AUC_Ridge_Lasso.plot',rand,'.seed',568,'.png',sep=''))
update(glmnPlot,
       ylab = "Mixing Percentage\nRidge <---------> Lasso",
       sub = "",
       main = "Area Under the ROC Curve",
       xlab = "Amount of Regularization")
dev.off()

plot(glmnetRoc, type = "s", legacy.axes = TRUE)

################################################################################
### Model : Support Vector Machines

library(kernlab)

# Running SVM Radial Kernel
set.seed(201)
sigmaRangeFull <- sigest(data.matrix(dTrain[,c(3:ncol(dTrain))]))
svmRGridFull <- expand.grid(sigma =  as.vector(sigmaRangeFull)[1],
                            C = 2^(-3:4))
set.seed(476)
svmRFitFull <- train(x = predictor, 
                     y = response,
                     method = "svmRadial",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     tuneGrid = svmRGridFull,
                     trControl = ctrl)
print(svmRFitFull)
saveRDS(svmRFitFull, paste('model/svmRFitFull.rds',rand,'.seed',568,'.rds',sep=''))
#######
# svmRFitFull <- readRDS(paste('model/svmRFitFull.rds',rand,'.seed',568,'.rds',sep=''))
#######

# Get the result from glmnet
print(svmRFitFull)
result.svmRFitFull<- svmRFitFull$result

# best result AUC achieved
result.svmRFitFull[which(result.svmRFitFull$ROC == max(result.svmRFitFull$ROC)),c(1:8)]

# Get Plot of AUC against alpha and lambda
png(filename=paste('image/svmRFitFull_grid_tune.plot',rand,'.seed',568,'.png',sep=''))
plot(svmRFitFull)
dev.off()

# Get Plot of AUC against sigma
png(filename=paste('image/svmRFitFull_AUC_against_sigma.plot',rand,'.seed',568,'.png',sep=''))
ggplot(svmRFitFull$results, aes(x = as.factor(sigma), y = C , size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")
dev.off()

svmRFitFull$pred <- merge(svmRFitFull$pred,  svmRFitFull$bestTune)
svmRCM <- confusionMatrix(svmRFitFull, norm = "none")
svmRRoc <- roc(response = svmRFitFull$pred$obs,
               predictor = svmRFitFull$pred$Y,
               levels = rev(levels(svmRFitFull$pred$obs)))

# # Running SVM Polynomial Kernel
# svmPGrid <-  expand.grid(degree = 1:2,
#                          scale = c(0.01, .005),
#                          C = 2^(seq(-6, -2, length = 10)))
# set.seed(476)
# svmPFitFull <- train(x = predictor, 
#                      y = response,
#                      method = "svmPoly",
#                      metric = "ROC",
#                      preProc = c("center", "scale"),
#                      tuneGrid = svmPGrid,
#                      trControl = ctrl)
# store model
# saveRDS(svmPFitFull, "model/svmPFitFull.rds")

# svmPFitFull$pred <- merge(svmPFitFull$pred,  svmPFitFull$bestTune)
# svmPCM <- confusionMatrix(svmPFitFull, norm = "none")
# svmPRoc <- roc(response = svmPFitFull$pred$obs,
#                predictor = svmPFitFull$pred$Y,
#                levels = rev(levels(svmPFitFull$pred$obs)))


# Gather results from Radial Kernel
svmRadialResults <- svmRFitFull$results
svmRadialResults$Set <- c(rep("Full Set", nrow(svmRFitFull$result)))
svmRadialResults$Sigma <- paste("sigma = ", 
                                format(svmRadialResults$sigma, 
                                       scientific = FALSE, digits= 5))

svmRadialResults <- svmRadialResults[!is.na(svmRadialResults$ROC),]
xyplot(ROC ~ C|Set, data = svmRadialResults,
       groups = Sigma, type = c("g", "o"),
       xlab = "Cost",
       ylab = "ROC (2008 Hold-Out Data)",
       auto.key = list(columns = 2),
       scales = list(x = list(log = 2)))

# Gather results from Polynomial Kernel
# svmPolyResults <- svmPFitFull$results
# svmPolyResults$Set <- c(rep("Full Set", nrow(svmPFitFull$result)))
# svmPolyResults <- svmPolyResults[!is.na(svmPolyResults$ROC),]
# svmPolyResults$scale <- paste("scale = ", 
#                               format(svmPolyResults$scale, 
#                                      scientific = FALSE))
# 
# svmPolyResults$Degree <- "Linear"
# svmPolyResults$Degree[svmPolyResults$degree == 2] <- "Quadratic"
# useOuterStrips(xyplot(ROC ~ C|Degree*Set, data = svmPolyResults,
#                       groups = scale, type = c("g", "o"),
#                       xlab = "Cost",
#                       ylab = "ROC (2008 Hold-Out Data)",
#                       auto.key = list(columns = 2),
#                       scales = list(x = list(log = 2))))

png(filename=paste('image/ROCplot_allmodels',rand,'.seed',568,'.png',sep=''))
plot(xgb.ROC, type = "s", col = 'red', legacy.axes = TRUE)
plot(nnetRoc, type = "s", add = TRUE, col = 'blue', legacy.axes = TRUE)
plot(svmPRoc, type = "s", add = TRUE,col = 'green', legacy.axes = TRUE)
plot(glmnetRoc, type = "s", add = TRUE, col = 'brown', legacy.axes = TRUE)
legend(0.7, .2, c('Extreme Gradient Boosting', 'Neural Nets', 'Support Vector Machine',' Ridge Regression') , 
       lty=1, col=c('red','blue','green','brown'), bty='n', cex=.95)

dev.off()
#

?legend

############################## BEGIN PREDICT TEST SET ########################

################################################################################
### Model : Extreme Gradient Boosting

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

# Run a more comprehensive analysis AUC
fun.auc(predRes.xgbTree_caret$prob, predRes.xgbTree_caret$obs)
fun.aucplot(predRes.xgbTree_caret$prob, predRes.xgbTree_caret$obs, "My AUC Plot")


