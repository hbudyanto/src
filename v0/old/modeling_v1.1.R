###############################################################################
### R code for Modeling 
### Author : Adi Budyanto
### Date : 1 August 2016

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
set.seed(568)
custid = unique(trans$customer_id)[sample(c(1:length(unique(trans$customer_id))),1000)]
trans1 = trans[which(trans$customer_id %in% custid),]

key = trans1[,c('customer_id','order_no','order_date','product_id')]
key = key[!duplicated(key),]
key = key[with(key, order(customer_id)), ]
key$order_date <- as.POSIXct(key$order_date)
key$is2015 <- year(key$order_date) == 2015

key$key = paste(key$customer_id, key$order_no, sep="")
write.table(key, 'data/keys/customerid_productid.key', row.names = F, col.names = T, sep=',')

### We'll split all of the pre-2015 data into the training set and a
### portion of the 2015 data too
training <- subset(key, !is2015)
pre2015 <- 1:nrow(training)
year2015 <- subset(key, is2015)

## Now randomly select some 2008 data for model training and add it
## back into the existing training data
set.seed(568)
x = unique(year2015$customer_id)
inTrain = x[sample(1:length(x),350)]

training2 <- year2015[ which(year2015$customer_id %in% inTrain),]
testing   <- year2015[ which(!(year2015$customer_id %in% inTrain)),]
training <- rbind(training, training2)

############################## SPLITTING POSITIVE NEGATIVE SAMPLES ########################
## TARGET 1 
data1 = data.table(training)
data1$product_id = as.character(data1$product_id)
data1$key = as.character(data1$key)

data1 <- data1[,c('key', 'product_id'), with=F]
data1 <- data1[!duplicated(data1)]
data1$target <- rep(1,nrow(data1))

# TARGET 0
# set random sampling
# dailies = unique(as.character(data1$product_id))

# set random sampling from top-25
dailies <- ddply(data1, .(product_id), function(x) c(numOrder = nrow(x)))
dailies <- dailies[with(dailies, order(-numOrder)), ]
dailies <- dailies[1:25,1]

# create unique list of customer_id 
purchases = unique(data1$key)
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

# set k = 5 items to be randomly sampled
k = 5
for (i in 1:length(purchases)){ # k random items for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(dailies, k)
}

items <- lapply(1:length(items), function(i){ # remove purchased items from above list
  setdiff(items[[i]], data1$product_id[which(data1$key == purchases[i])])
})

reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
data0 <- data.frame(cbind('key' = rep(purchases, reps), 'product_id'=unlist(items)))
data0$target <- rep(0,nrow(data0))

#### Checker to ensure no duplicate is found
# could be implemented in log
# data <- rbind(data0,data1)
# data$target <- NULL
# duplicate <- data[duplicated(data)]
# x0 = data0[which(data0$key == 'u101425VD1802992'),]
# x1 = data1[which(data1$key == 'u101425VD1802992'),]
# x0 = x0[with(x0, order(order_no)), ]
# x1 = x1[with(x1, order(order_no)), ]


############################## GETTING BACK CUSTOMER ID TO EACH KEY ########################
key = key[,c('customer_id','order_no','order_date','key')]
feat.1 = merge(data1,key,by=c('key'))
feat.0 = merge(data0,key,by=c('key'))

feat.1 = data.frame(feat.1)
feat.1 = feat.1[,c(4:6,2:3)]

feat.0 = data.frame(feat.0)
feat.0 = feat.0[,c(4:6,2:3)]

########################### ENGINEER CUSTOMERS FEATURES #######################
feat.cust.global <- read.csv('features/features.cust.global.csv')
feat.cust.global <- feat.cust.global[,c(1,14:24)]

### POSITIVE TARGET
feat.0 = merge(feat.0,feat.cust.global, by=c('customer_id'))

### NEGATIVE TARGET
feat.1 = merge(feat.1,feat.cust.global, by=c('customer_id'))

########################### ENGINEER ITEM FEATURES #######################
feat.item.global <- read.csv('features/features.item.global.csv')

### POSITIVE TARGET
feat.0 = merge(feat.0,feat.item.global, by=c('product_id'))

### NEGATIVE TARGET
feat.1 = merge(feat.1,feat.item.global, by=c('product_id'))

# x0 = feat.0[which(feat.0$customer_id == 'u98835'),]
# x1 = feat.1[which(feat.1$customer_id == 'u98835'),]
# x0 = x0[with(x0, order(order_no)), ]
# x1 = x1[with(x1, order(order_no)), ]

########################### ENGINEER FEATURES FROM TRANSACTION DATA #######################
### Engineer features based on unique order no
### Examples include : payment type, order type, shipping country id, courrier

# Sample features to create : order_type, payment_method, order_date
temp1.trans.order = trans1[,c("customer_id","order_no","order_date","order_type","payment_method")]

str(data1)
str(temp1.trans.order)
x = merge(data1,temp1.trans.order,by=c('customer_id','order_no','order_date'))


x0 = data.table(x0)
x0[,sum(product_id),by='customer_id']
x0[,(as.numeric(max(order_date, na.rm=T))-as.numeric(min(order_date, na.rm=T)))/(60*60*24),by='customer_id']$V1



########################### BEGIN MODELLING #######################
df = rbind(feat.1,feat.0)
train = data.frame(df)

## best setting
model1 <- xgb.model.Dtrain.make(file = train, 
                                eta=0.05, subsample=0.6, colsample=0.025, depth=1, metric='auc', rounds=100,
                                cv = T, seed=29, feat.imp=T)

xgb.plot.importance(model1$feat.imp)

########################### Use grid search to find  the max-depth that maximizes AUC-ROC #######################
param_tune_xg <- function(x,y,xtest, ytest, max.depth) {
  ## train xgboost :
  bst <- xgboost(data=  x, label=y, max.depth= round(max.depth),
                 eta=1, nthread= 2 , nround=2, objective = "binary:logistic")
predict(bst,xtest)
}

## Use Grid Search to Find Max Depth maximizing AUC-ROC
cv  <- cv.setup(x = data.matrix(train[,-c(1:5)]), y = train$target, score=auc_roc,
                num_folds=5, num_iter=2)

Dtrain = xgb.DMatrix(data=data.matrix(file[,-c(1:5)]), label=train$target, missing=NA)

xgb.grid <-  expand.grid(
            nrounds=1000,
            eta=c(0.1,0.01,0.001),
            max_depth=c(2,4,6,8,10,12,14),
            gamma=1,
            colsample_bytree = 0.5,
            min_child_weight = 1)

cv.ctrl = trainControl(
  method= "cv",
  number=5,
  verboseIter = TRUE,
  returnData= FALSE,
  classProbs= TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

train$target = as.factor(train$target)
names(train)
str(train)

F=c(5,7:59,61:68)
for(i in F) train[,i]=as.factor(train[,i])

names(train)
feature.names = names(train[,c(-(1:4),-6,-60)])
for (f in feature.names) {
  if (class(train[[f]])=="factor") {
    levels <- unique(c(train[[f]]))
    train[[f]] <- factor(train[[f]],
                         labels=make.names(levels))
  }
}

str(train)

xgb_tune = train(x=data.matrix(train[,-c(1:5)]), y=train$target,
                 method = 'xgbTree',
                 trControl = cv.ctrl,
                 tuneGrid = xgb.grid,
                 verbose = T,
                 subsample = 0.5,
                 colsample_bytree = 0.5,
                 seed =1,
                 eval_metric = "roc",
                 objective = "multi:softprob",
                 num_class = 12,
                 nthread = 3
)



### Engineer features based on unique order no
### Examples include : payment type, order type, shipping country id, courrier

# Sample features to create : order_type, payment_method, order_date
temp1.trans.order = trans1[,c("customer_id","order_no","order_date","order_type","payment_method")]
str(data1)
str(temp1.trans.order)
x = merge(data1,temp1.trans.order,by=c('customer_id','order_no','order_date'))

x[which(x$customer_id=='u100073'),]
trans1[which(trans1$customer_id=='u100073'),c(2:6)]


###################################################
df = rbind(feat.1,feat.0)
train = data.frame(df)

str(train)
names(train)

trainSmall = train[,c(1:6,61,23:28,36:60)]
names(trainSmall)
str(trainSmall)

F=c(5,8:38)
for(i in F) trainSmall[,i] = factor(trainSmall[,i], labels = c('no','yes'))

