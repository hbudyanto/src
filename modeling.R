### R code for Modeling Process
### Author : Adi Budyanto
### Date : 20 August 2016

require(data.table)
require(plyr)
library(lubridate)
require(doParallel)
require(caret)

# load pre-defined functions
source('~/PycharmProjects/dissertation/src/v2/func.R')

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data")  
} else {
  setwd("~/Dropbox/dissertation/raw_data")
}

######################################################################
#### Read in the transaction data in it's raw form 
trans <- read.csv('data/trans.csv')
# remove all datasets in mainframe except trans
# rm(list=setdiff(ls(), "trans"))

# format back selected variables to proper format
# date
trans$order_date <- as.POSIXct(trans$order_date)
trans$invoice_date <- as.POSIXct(trans$invoice_date)
trans$shipment_date <- as.POSIXct(trans$shipment_date)
trans$reminder_date <- as.POSIXct(trans$reminder_date)
# character
trans$customer_id <- as.character(trans$customer_id)
trans$product_id <- as.character(trans$product_id)
trans$order_no <- as.character(trans$order_no)
# numeric
trans$global_line_total_exc_vat <- as.numeric(as.character(trans$global_line_total_exc_vat))
trans$global_line_total_exc_vat[is.na(trans$global_line_total_exc_vat)] <- 0
trans$discount_percent <- as.numeric(as.character(trans$discount_percent))
trans$discount_percent[is.na(trans$discount_percent)] <- 0

######################################################################
#### Random select smaller set of 2014 customers for faster simulation (temp)
# filter customer in the year of 2014 
cust14 <- unique(trans[which(year(trans$order_date) == 2014),c("customer_id")])
set.seed(123); nsample <- 200
tmp.cust14 <- sample(cust14,nsample)
# elicit corresponding transaction nbs based on above customer nb
tmp.train <- trans[which(trans$customer_id %in% tmp.cust14),c('customer_id','order_no','order_date','product_id')]
# new variable to determine the year of the transaction 
# for the sake of constructing train, valid, test sets
tmp.train$is2015 <- year(tmp.train$order_date) == 2015
# eliminate duplicate values while counting freq of same transaction
tmp.train <- count(tmp.train)

######################################################################
#### For each transaction order, create unique key of single identifier
tmp.train$key = paste(tmp.train$customer_id, tmp.train$order_no, sep="")

######################################################################
#### Splitting data for constructing positive and negative target/class
# positive samples / class = 1
data1 <- data.table(tmp.train)
data1 <- data1[,c('key', 'product_id'), with=F]
data1$target <- rep(1,nrow(data1))

# negative samples / class = 0
# firstly, create target product_id for random sampling
aman <- ddply(data1, .(product_id), function(x) c(numOrder = nrow(x)))
aman <- aman[with(aman, order(-numOrder)), ]
aman <- aman[1:10,1]

# create unique list of key identifier (cust_id + order_no)
purchases = unique(data1$key)
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

# set k = 3 items to randomly sampled as negative target
k = 3
for (i in 1:length(purchases)){ # k random items for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(aman, k)
}

# remove purchased items from aforementioned list
items <- lapply(1:length(items), function(i){ 
  setdiff(items[[i]], data1$product_id[which(data1$key == purchases[i])])
})
reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
data0 <- data.frame(cbind('key' = rep(purchases, reps), 'product_id'=unlist(items)))
data0$target <- rep(0,nrow(data0))

######################################################################
#### Binding data1 and data0 together and get back the unique identifiers
data <- rbind(data1,data0)
data <- data[with(data, order(key)),]
colnames(tmp.train)[-4]

# Link back to key data to get unique identifiers
purchaseData <- merge(data,tmp.train[,-4], by=c("key"))

#####################################################################################
##### Load relevant data (purchase dummy data - matrix similarity) generated from similariy.R

temp = list.files(path = "features/features.itembasedCF", pattern="*.Rdata")
for (i in 1:length(temp)) {load(paste("features/features.itembasedCF/",temp[i],sep=""))}

temp = list.files(path = "features/features.matrix.cust.vars", pattern="*.Rdata")
for (i in 1:length(temp)) {load(paste("features/features.matrix.cust.vars/",temp[i],sep=""))}

#####################################################################################
# Reformating purchaseData as dataframe and get the final form of targetData
# Load product table
feat.item.global <- read.csv('features/features.item.global.csv')
# names(feat.item.global)
item.features = colnames(feat.item.global)[c(1:7,143:ncol(feat.item.global))]

purchaseData <- data.frame(purchaseData)
targetData <- join(purchaseData[,c("order_no","target","customer_id","product_id","is2015")], feat.item.global[,item.features], by=c('product_id'))
# get unique customer_id
id <- unique(targetData$customer_id)

#####################################################################################
### Constructing features from three sources : Item profiles, Customer, and ItembasedCF

# Get relevant itembasedCF per customer basis
# 1 Product ID (p1,p2,p3)
tmp.data <- useritemList[[14]]
tmp.matrix <- CFlist[[14]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreProd <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","product_id")])

# 2 Sub Category (dailies, two-weeklies, etc)
tmp.data <- useritemList[[17]]
tmp.matrix <- CFlist[[16]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcategory <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcategory")])

# 3 Category (dailies, non-dailies, other)
tmp.data <- useritemList[[4]]
tmp.matrix <- CFlist[[3]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCategory <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category")])

# 4 Lens type (spherical, toric,)
tmp.data <- useritemList[[10]]
tmp.matrix <- CFlist[[12]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLens<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lenstype")])

# 5 Brand (acuvue,etc)
tmp.data <- useritemList[[1]]
tmp.matrix <- CFlist[[1]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreBrand<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","brand")])

# 6 Manufacturer (jnj, etc)
tmp.data <- useritemList[[13]]
tmp.matrix <- CFlist[[13]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","manufacturer")])

# 7 Category Lens (dailies.toric)
tmp.data <- useritemList[[5]]
tmp.matrix <- CFlist[[5]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatLens<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category.lens")])

# 8 Category Brand (dailies.acuvue)
tmp.data <- useritemList[[3]]
tmp.matrix <- CFlist[[4]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatBrand <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category.brand")])

# 9 Category Manufacturer (dailies_jnj)
tmp.data <- useritemList[[7]]
tmp.matrix <- CFlist[[6]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","category.manu")])

# 10 Lenstype Brand (toric_acuvue)
tmp.data <- useritemList[[8]]
tmp.matrix <- CFlist[[8]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensBrand <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.brand")])

# 11 Lenstype Manufacturer (toric_jnj)
tmp.data <- useritemList[[11]]
tmp.matrix <- CFlist[[10]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.manu")])

# 12 Lenstype Sub category (toric_dailies)
tmp.data <- useritemList[[12]]
tmp.matrix <- CFlist[[11]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensSubcat <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.subcat")])

# 13 Subcategory Brand (dailies_acuvue)
tmp.data <- useritemList[[15]]
tmp.matrix <- CFlist[[15]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcatBrand <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcat.brand")])

# 14 Subcategory Manufacturer (dailies_jnj)
tmp.data <- useritemList[[18]]
tmp.matrix <- CFlist[[18]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcatManu<- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcat.manu")])

# 15 Category Lenstype Manufacturer (nondailies_toric_jnj)
tmp.data <- useritemList[[6]]
tmp.matrix <- CFlist[[7]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreCatLensManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","cat.lens.manu")])

# 16 Subcategory Lenstype Manufacturer (two_weeklies_toric_jnj)
tmp.data <- useritemList[[16]]
tmp.matrix <- CFlist[[17]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreSubcatLensManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","subcat.lens.manu")])

# 17 Lenstype Brand Manufacturer (toric_acuvue_jnj)
tmp.data <- useritemList[[9]]
tmp.matrix <- CFlist[[9]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreLensBrandManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","lens.brand.manu")])

# 18 Brand Manufacturer (toric_acuvue_jnj)
tmp.data <- useritemList[[2]]
tmp.matrix <- CFlist[[2]]
tmp.holder <- getUserItemBasedScore(data = tmp.data[tmp.data$customer_id %in% id,], matrix_similarity =tmp.matrix)
targetData$scoreBrandManu <- getAffinityScore(holder= tmp.holder, test = targetData[,c("customer_id","brand.manu")])

tmp <- names(targetData)[6:24]
targetFinalData <- targetData[, !(colnames(targetData) %in% tmp)]

# save original files before data splitting
save(purchaseData, targetFinalData, targetData, file = paste('features/features.cust.item.random/purchaseData',nsample,'.seed',123,'.rda',sep=''))

#####################################################################################
### We'll split all of the 2014 data into the training set and a portion of the 2015 data too

training <- subset(targetFinalData, !is2015)
pre2015 <- 1:nrow(training)
year2015 <- subset(targetFinalData, is2015)

## Now randomly select some 2015 data for model training and add it
## back into the existing training data
set.seed(568)
inTrain <- createDataPartition(year2015$target, p = 1/2)[[1]]
training2 <- year2015[ inTrain,]
testing   <- year2015[-inTrain,]
training <- rbind(training, training2)

training$is2015 <- testing$is2015 <- NULL

training <- noZV(training)
testing <- testing[, names(training)]

# save original files before data splitting
save(training, pre2015, testing, file = paste('features/features.cust.item.random/preModelData',nsample,'.seed',123,'.rda',sep=''))

# create new folders to store resutls and model
dir.create('results')
dir.create('images')
dir.create('models')

#####################################################################################
### See effect in AUC from data splitting strategy using SVM
fullSet <- names(training)[!(names(training) %in% c("target","order_no","customer_id","product_id"))]

pre2015Data <- training[pre2015,]
year2015Data <- rbind(training[-pre2015,], testing)

set.seed(552)
test2015 <- createDataPartition(year2015Data$target, p = .5)[[1]]

allData <- rbind(pre2015Data, year2015Data[-test2015,])
holdout2015 <- year2015Data[test2015,]

## Use a common tuning grid for both approaches. 
svmrGrid <- expand.grid(sigma = c(.00007, .00009, .0001, .0002),
                        C = 2^(-3:8))

## Evaluate the model using overall 10-fold cross-validation
ctrl0 <- trainControl(method = "cv",
                      summaryFunction = twoClassSummary,
                      classProbs = TRUE)
set.seed(477)
svmFit0 <- train(pre2015Data[,fullSet], pre2015Data$target,
                 method = "svmRadial",
                 tuneGrid = svmrGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = ctrl0)
svmFit0

### Now fit the single 2008 test set
ctrl00 <- trainControl(method = "LGOCV",
                       summaryFunction = twoClassSummary,
                       classProbs = TRUE,
                       index = list(TestSet = 1:nrow(pre2015Data)))

set.seed(476)
svmFit00 <- train(allData[,fullSet], allData$target,
                  method = "svmRadial",
                  tuneGrid = svmrGrid,
                  preProc = c("center", "scale"),
                  metric = "ROC",
                  trControl = ctrl00)
svmFit00

## Combine the two sets of results and plot

grid0 <- subset(svmFit0$results,  sigma == svmFit0$bestTune$sigma)
grid0$Model <- "10-Fold Cross-Validation"

grid00 <- subset(svmFit00$results,  sigma == svmFit00$bestTune$sigma)
grid00$Model <- "Single 2008 Test Set"

plotData <- rbind(grid00, grid0)

plotData <- plotData[!is.na(plotData$ROC),]
write.table(dTrain, paste('results/plotData_datasplittingSVM',nsample,'.seed',476,'.csv',sep=''), col.names = T, sep = ',')
png(filename=paste('imagess/datasplitSVM',nsample,'.seed',476,'.png',sep=''))
xyplot(ROC ~ C, data = plotData,
       groups = Model,
       type = c("g", "o"),
       scales = list(x = list(log = 2)),
       auto.key = list(columns = 1))
dev.off()

#####################################################################################
### Preprocessing before feeding the data into machine learning algorithms

fullSet <- names(training)[!(names(training) %in% c("target","order_no","customer_id","product_id"))]

training[,2]= factor(training[,2], labels = c('N','Y'))
testing[,2]= factor(testing[,2], labels = c('N','Y'))

colnames(training)<- make.names(colnames(training))
colnames(testing)<- make.names(colnames(testing))

### Model : Extreme Gradient Boosting

# Pack the training control parameters
xgb_trcontrol = trainControl(
  method = "LGOCV",
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",   # save losses across all models
  classProbs = TRUE,      # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  index = list(TestSet = pre2015),
  savePredictions = TRUE,
  allowParallel = TRUE
)

set.seed(568)
xgbTree_caret <- train(x = training[,fullSet],
                       y = training$target,
                       method = "xgbTree",
                       metric = "ROC",
                       tuneGrid = expand.grid(nrounds = 10*(30:50), #10*(15:50)
                                              eta = c(0.1, 0.2, 0.4, 0.6, 0.8, 1), #c(0.1, 0.2, 0.4, 0.6, 0.8, 1)
                                              max_depth = c(2, 4, 6, 8, 10), #c(0.05,0.1,0.5)
                                              gamma = 0,               #default=0
                                              colsample_bytree = 0.8,    #default=1
                                              min_child_weight = 0.8),   #default=1
                       trControl = xgb_trcontrol,
                       preProc = c("center", "scale"),
                       maximize = TRUE,
                       subsample = 0.8,
                       verbose = 1,
                       base_score = 0.5,
                       nthread = 10
)

# Save Model
saveRDS(xgbTree_caret, paste('model/xgbTree_caret.rds',nsample,'.seed',123,'.rds',sep=''))

# Get the result from 
result.xgbTree_caret = xgbTree_caret$results
# best result Spec
result.xgbTree_caret[which(result.xgbTree_caret$Spec == max(result.xgbTree_caret$Spec)),c(1:9)]

# See in a glance the predictive power
xgb.pred <- predict(xgbTree_caret,testing[,fullSet])

#Look at the confusion matrix  
confusionMatrix(xgb.pred,testing$target)  

plot(xgbTree_caret)

### Model : Neural Networks
nnetGrid <- expand.grid(size = 1:10, decay = c(0, .1, 1, 2))
maxSize <- max(nnetGrid$size)
set.seed(568)

# model 1 : no transformation
nnetFit <- train(x = training[,fullSet], 
                 y = training$target,
                 method = "nnet",
                 metric = "ROC",
                 preProc = c("center", "scale"),
                 tuneGrid = nnetGrid,
                 trace = FALSE,
                 maxit = 2000,
                 MaxNWts = 1*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                 trControl = xgb_trcontrol)
print(nnetFit)
# Save Model
saveRDS(nnetFit, paste('model/nnetFit.rds',nsample,'.seed',123,'.rds',sep=''))


# model 2 : spatial sign transformation
set.seed(568)
nnetFit2 <- train(x =  training[,fullSet], 
                  y = training$target,
                  method = "nnet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 1*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  trControl = xgb_trcontrol)
print(nnetFit2)
# Save Model
saveRDS(nnetFit2, paste('model/nnetFit2.rds',nsample,'.seed',123,'.rds',sep=''))


# model 3 : repeat the model 10 times, and take the average results
nnetGrid$bag <- FALSE

set.seed(476)
nnetFit3 <- train(x = training[,fullSet], 
                  y = training$target,
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale"),
                  tuneGrid = nnetGrid,
                  repeats = 10,
                  trace = FALSE,
                  maxit = 2000,
                  MaxNWts = 10*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  allowParallel = FALSE, ## this will cause to many workers to be launched.
                  trControl = xgb_trcontrol)
print(nnetFit3)
# Save Models
saveRDS(nnetFit3, paste('model/nnetFit3.rds',nsample,'.seed',123,'.rds',sep=''))

# model 4 : repeat the model 10 times, and take the average results plus tranformation
set.seed(476)
nnetFit4 <- train(x = training[,fullSet], 
                  y = training$target,
                  method = "avNNet",
                  metric = "ROC",
                  preProc = c("center", "scale", "spatialSign"),
                  tuneGrid = nnetGrid,
                  trace = FALSE,
                  maxit = 2000,
                  repeats = 10,
                  MaxNWts = 10*(maxSize * (length(fullSet) + 1) + maxSize + 1),
                  allowParallel = FALSE, 
                  trControl = xgb_trcontrol)
print(nnetFit4)
# Save Models
saveRDS(nnetFit4, paste('model/nnetFit4.rds',nsample,'.seed',123,'.rds',sep=''))

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

nnetRoc <- roc(response = nnetFit4$pred$obs,
               predictor = nnetFit4$pred$Y,
               levels = rev(levels(nnetFit4$pred$obs)))
plot(nnetRoc, type = "s", legacy.axes = TRUE)


# See in a glance the predictive power
nnetFit4.pred <- predict(nnetFit4,testing[,fullSet])

#Look at the confusion matrix  
confusionMatrix(nnetFit4.pred,testing$target)  

#### Model : Penalized Regression Logistics
# lasso, ridge and elastic net by configuring the alpha parameter to 1, 0 or in [0,1]
glmnGrid <- expand.grid(alpha = c(0,  .1,  .2, .4, .6, .8, 1), #c(0,  .1,  .2, .4, .6, .8, 1)
                        lambda = seq(.01, .2, length = 50))

set.seed(476)
glmnFit <- train(x = training[,fullSet], 
                 y = training$target,
                 method = "glmnet",
                 tuneGrid = glmnGrid,
                 preProc = c("center", "scale"),
                 metric = "ROC",
                 trControl = xgb_trcontrol)
# Get the result from glmnet
print(glmnFit)
# Save Models
saveRDS(glmnFit, paste('model/glmnFit.rds',nsample,'.seed',123,'.rds',sep=''))

result.glmnFit <- glmnFit$result

# best result AUC achieved
result.glmnFit[which(result.glmnFit$Spec == max(result.glmnFit$Spec)),c(1:8)]
plot(glmnFit)

# See in a glance the predictive power
glmnFit.pred <- predict(glmnFit,testing[,fullSet])

#Look at the confusion matrix  
confusionMatrix(glmnFit.pred,testing$target)  

### Model : Support Vector Machines
library(kernlab)

# Running SVM Radial Kernel
set.seed(201)
sigmaRangeFull <- sigest(data.matrix(training[,fullSet]))
svmRGridFull <- expand.grid(sigma =  as.vector(sigmaRangeFull)[1],
                            C = 2^(-3:4))
set.seed(476)
svmRFitFull <- train(x = training[,fullSet], 
                     y = training$target,
                     method = "svmRadial",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     tuneGrid = svmRGridFull,
                     trControl = xgb_trcontrol)
print(svmRFitFull)
# Save Models
saveRDS(svmRFitFull, paste('model/svmRFitFull.rds',nsample,'.seed',123,'.rds',sep=''))

result.svmRFitFull<- svmRFitFull$result
# best result AUC achieved
result.svmRFitFull[which(result.svmRFitFull$Spec == max(result.svmRFitFull$Spec)),c(1:8)]
plot(svmRFitFull)

# See in a glance the predictive power
svmRFitFull.pred <- predict(svmRFitFull,testing[,fullSet])

#Look at the confusion matrix  
confusionMatrix(svmRFitFull.pred,testing$target)  

#SVM Polynomial Kernel
svmPGrid <-  expand.grid(degree = 1:2,
                         scale = c(0.01, .005),
                         C = 2^(seq(-6, -2, length = 10)))
set.seed(476)
svmPFitFull <- train(x = training[,fullSet], 
                     y = training$target,
                     method = "svmPoly",
                     metric = "ROC",
                     preProc = c("center", "scale"),
                     tuneGrid = svmPGrid,
                     trControl = xgb_trcontrol)

print(svmPFitFull)
# Save Models
saveRDS(svmPFitFull, paste('model/svmPFitFull.rds',nsample,'.seed',123,'.rds',sep=''))

result.svmPFitFull<- svmPFitFull$result
# best result AUC achieved
result.svmPFitFull[which(result.svmPFitFull$Spec == max(result.svmPFitFull$Spec)),c(1:8)]
plot(result.svmPFitFull)

# See in a glance the predictive power
svmPFitFull.pred <- predict(svmPFitFull,testing[,fullSet])

#Look at the confusion matrix  
confusionMatrix(svmPFitFull.pred,testing$target)  

#####################################################################################
### Create Prediction

predRes.xgbTree <- data.frame(order_no = testing$order_no ,obs = testing$target)
predRes.xgbTree$prob <- predict(xgbTree_caret, testing[,fullSet],type='prob')[,'Y']
predRes.xgbTree$pred <- predict(xgbTree_caret, testing[,fullSet])

# Reformating data for interpreting the resutls
predRes.xgbTree$obs = factor(predRes.xgbTree$obs,labels=c(0,1))
predRes.xgbTree$pred = factor(predRes.xgbTree$pred,labels=c(0,1))

predRes.xgbTree$label <- ifelse(predRes.xgbTree$obs == 1,
                                      "True Outcome: Purchase", 
                                      "True Outcome: Did Not Purchase")

### Plot the probability of bad credit
histogram(~prob|label,
          data = predRes.xgbTree,
          layout = c(2, 1),
          nint = 20,
          xlab = "Probability of Purchase",
          type = "count")

### Create the confusion matrix from the test set.
confusionMatrix(data = predRes.xgbTree$pred, 
                reference = predRes.xgbTree$obs)
### Plot the probability of bad credit
histogram(~prob|label,
          data = predRes.xgbTree,
          layout = c(2, 1),
          nint = 20,
          xlab = "Probability of Purchase",
          type = "count")

### ROC curves:
library(pROC)
purchaseROC <- roc(relevel(predRes.xgbTree$obs, "1"), predRes.xgbTree$prob)
coords(purchaseROC, "all")[,1:3]

### Compute ROC and its 95 Confidential Level
auc(purchaseROC)
ci.auc(purchaseROC)
