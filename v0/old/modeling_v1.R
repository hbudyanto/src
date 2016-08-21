###############################################################################
### R code for Modeling 
### Author : Adi Budyanto
### Date : 1 August 2016

require(data.table)
require(plyr)
require(futile.logger)
require(xgboost)
library(caret)
library(lubridate)

# Set up the appropriate path
setwd("~/Dropbox/dissertation/raw_data")

# load trans data
trans <- read.csv('data/trans.csv')

cores <- 16
if(cores > 1) {
  library(doMC)
  registerDoMC(cores)
}

## Create random sampling of 10000 users
set.seed(568)
custid = unique(trans$customer_id)
custid.sampling = custid[sample(c(1:length(custid)),10000)]

trans1 = trans[which(trans$customer_id %in% custid.sampling),]

########################### BEGIN FEATURIZE ORDER DATA #######################
### Engineer features based on unique order no
### Examples include : payment type, order type, shipping country id, courrier

# Sample features to create : order_type, payment_method, order_date
temp.trans.order = trans1[,c("customer_id","order_no","order_date","order_type","payment_method")]

# Feature engineering order_type
temp.trans.order$order_type <- tolower(as.character(temp.trans.order$order_type))
temp.trans.order$order_type[temp.trans.order$order_type ==""] <- "Unk"
temp.trans.order$order_type <- factor(paste("orderType",temp.trans.order$order_type, sep=""))

# Feature engineering payment type
temp.trans.order$payment_method <- gsub(" ", "", tolower(as.character(temp.trans.order$payment_method)))
temp.trans.order$payment_method[temp.trans.order$payment_method =="null"] <- "Unk"
temp.trans.order$payment_method <- factor(paste("paymentType",temp.trans.order$payment_method, sep=""))

## Calculate the total number of order made by each customer
x = temp.trans.order[,c(1,2)]; x = x[!duplicated(x),]
nbOrder = ddply(x, .(customer_id), function(x) c(numOrder = nrow(x)))

temp.trans.order = temp.trans.order[!duplicated(temp.trans.order),]
#head(temp.trans.order)

# Calculate the total number of order made based on order_type
nbOrderType <- ddply(temp.trans.order, .(customer_id),
                     function(x) as.data.frame(t(as.matrix(table(x$order_type)))),
                     .parallel = cores > 1)
nbOrderType <- noZV(nbOrderType)

# Calculate the total number of order made based on order_type
nbOrderPayment <- ddply(temp.trans.order, .(customer_id),
                        function(x) as.data.frame(t(as.matrix(table(x$payment_method)))),
                        .parallel = cores > 1)
nbOrderPayment <- noZV(nbOrderPayment)

########################### BEGIN FEATURIZE PRODUCT ID #######################
### Engineer features based on products that customer bought
### Examples include : sum of transaction value, avg of discount gotten by cust, nb.products

temp.trans.prod = trans1[,c("customer_id","order_no","order_date","product_id","global_line_total_exc_vat",
                            "discount_percent")]

temp.trans.prod$key = paste(temp.trans.prod$customer_id, temp.trans.prod$order_no, sep="")
temp.trans.prod$key = paste(temp.trans.prod$key, temp.trans.prod$product_id, sep="")

# Count number of products each customer buys
nbProd = ddply(temp.trans.prod, .(key), function(x) c(numProd = nrow(x)))

temp.trans.prod$global_line_total_exc_vat = as.numeric(as.character(temp.trans.prod$global_line_total_exc_vat))
temp.trans.prod$global_line_total_exc_vat[which(is.na(temp.trans.prod$global_line_total_exc_vat))] <- 0

temp.trans.prod$discount_percent = as.numeric(as.character(temp.trans.prod$discount_percent))
temp.trans.prod$discount_percent[which(is.na(temp.trans.prod$discount_percent))] <- 0

# Sum total payment per order and average discount customer receive
sumProdPaymentDiscount <- ddply(temp.trans.prod, .(key),
                                function(x) {
                                  data.frame(
                                    sum_global_line_total_exc_vat = sum(x$global_line_total_exc_vat, na.rm = TRUE),
                                    mean_discount_percent = mean(x$discount_percent, na.rm = TRUE))
                                },
                                .parallel = cores > 1)
sumProdPaymentDiscount <- noZV(sumProdPaymentDiscount)

# load feat.item.global
feat.item.global <- read.csv('features/features.item.global.csv')
temp.trans.prod <- join(temp.trans.prod,feat.item.global, by=c("product_id"))

# feature engineering for manufacturer
colnames(temp.trans.prod)[grep("manufacturer", names(temp.trans.prod), fixed = TRUE)]

countProdManufacturer <- ddply(temp.trans.prod, .(key),
                               function(x) {
                                 data.frame(
                                   manufacturerother = sum(x$manufacturerother, na.rm = TRUE),
                                   manufactureramo = sum(x$manufactureramo, na.rm = TRUE),
                                   manufacturerbausch_Lomb = sum(x$manufacturerbausch_Lomb, na.rm = TRUE),
                                   manufacturerciba_vision = sum(x$manufacturerciba_vision, na.rm = TRUE),
                                   manufacturercoopervision = sum(x$manufacturercoopervision, na.rm = TRUE),
                                   manufacturerjohnson_johnson = sum(x$manufacturerjohnson_johnson, na.rm = TRUE))
                               },
                               .parallel = cores > 1)
countProdManufacturer <- noZV(countProdManufacturer)

# feature engineering for lenstype
colnames(temp.trans.prod)[grep("category", names(temp.trans.prod), fixed = TRUE)]

countProdCategory <- ddply(temp.trans.prod, .(key),
                           function(x) {
                             data.frame(
                               categorydailies = sum(x$categorydailies, na.rm = TRUE),
                               categoryother = sum(x$categoryother, na.rm = TRUE),
                               categorysolution = sum(x$categorysolution, na.rm = TRUE),
                               categorycolours = sum(x$categorycolours, na.rm = TRUE),
                               categoryeye_care = sum(x$categoryeye_care, na.rm = TRUE),
                               categorynondailies = sum(x$categorynondailies, na.rm = TRUE))
                           },
                           .parallel = cores > 1)
countProdCategory <- noZV(countProdCategory)

# feature engineering for lenstype
colnames(temp.trans.prod)[grep("brand", names(temp.trans.prod), fixed = TRUE)]

countProdBrand <- ddply(temp.trans.prod, .(key),
                        function(x) {
                          data.frame(
                            brandbrandacuvue = sum(x$brandbrandacuvue, na.rm = TRUE),
                            brandbrandad.hoc = sum(x$brandbrandad.hoc, na.rm = TRUE),
                            brandbrandair = sum(x$brandbrandair, na.rm = TRUE),
                            brandbrandaura = sum(x$brandbrandaura, na.rm = TRUE),
                            brandbrandbiofinity = sum(x$brandbrandbiofinity, na.rm = TRUE),
                            brandbrandavaira = sum(x$brandbrandavaira, na.rm = TRUE),
                            brandbrandbiomedics = sum(x$brandbrandbiomedics, na.rm = TRUE),
                            brandbrandblink = sum(x$brandbrandblink, na.rm = TRUE),
                            brandbrandboston = sum(x$brandbrandboston, na.rm = TRUE),
                            brandbrandcomplete = sum(x$brandbrandcomplete, na.rm = TRUE),
                            brandbranddailies = sum(x$brandbranddailies, na.rm = TRUE),
                            brandbrandeverclear = sum(x$brandbrandeverclear, na.rm = TRUE),
                            brandbrandeye = sum(x$brandbrandeye, na.rm = TRUE),
                            brandbrandfocus = sum(x$brandbrandfocus, na.rm = TRUE),
                            brandbrandfrequency = sum(x$brandbrandfrequency, na.rm = TRUE),
                            brandbrandfreshlook = sum(x$brandbrandfreshlook, na.rm = TRUE),
                            brandbrandhycosan = sum(x$brandbrandhycosan, na.rm = TRUE),
                            brandbrandhydron = sum(x$brandbrandhydron, na.rm = TRUE),
                            brandbrandopti.free = sum(x$brandbrandeye, na.rm = TRUE),
                            brandbrandfocus = sum(x$brandbrandother, na.rm = TRUE),
                            brandbrandproclear = sum(x$brandbrandproclear, na.rm = TRUE),
                            brandbrandpurevision = sum(x$brandbrandregard, na.rm = TRUE),
                            brandbrandrenu = sum(x$brandbrandrenu, na.rm = TRUE),
                            brandbrandsoflens = sum(x$brandbrandsoflens, na.rm = TRUE))
                        },
                        .parallel = cores > 1)
countProdBrand <- noZV(countProdBrand)

########################### MERGE BOTH USER & ITEM FEATURES #######################

data = trans1[,c("customer_id","order_date","order_no","product_id")]
data = data[!duplicated(data),]
data$key = paste(data$customer_id, data$order_no, sep="")
data$key = paste(data$key, data$product_id, sep="")

summarized = merge(nbProd, countProdManufacturer)
summarized = merge(summarized,countProdCategory)
summarized = merge(summarized ,countProdManufacturer)
summarized = merge(summarized ,sumProdPaymentDiscount)
summarized = merge(summarized,data)
summarized = summarized[,c(17:20,2:16)]

summarized = merge(summarized,nbOrder)
summarized = merge(summarized,nbOrderPayment)
summarized = merge(summarized,nbOrderType)

########################### BEGIN CREATE USER FEATURESs #######################
# load feat.user.global
feat.cust.global <- read.csv('features/features.cust.global.csv')
summarized= merge(summarized,feat.cust.global)

# drop dates
summarized$first_order_date <- NULL
summarized$last_order_date <- NULL

# partition data into training set and validation set based year of transaction
summarized$order_date <- as.POSIXct(summarized$order_date)
summarized$is2015 <- year(summarized$order_date) == 2015
summarized <- noZV(summarized)


########################### DATA SPLITING#######################
## release some memory 
rm(countProdBrand, countProdCategory, countProdManufacturer, nbProd,
   nbOrderPayment, nbOrderType, nbOrder, sumProdPaymentDiscount)

### We'll split all of the pre-2015 data into the training set and a
### portion of the 2015 data too
training <- subset(summarized, !is2015)
pre2015 <- 1:nrow(training)
year2015 <- subset(summarized, is2015)

## Now randomly select some 2008 data for model training and add it
## back into the existing training data
set.seed(568)
x = unique(year2015$customer_id)
inTrain = x[sample(1:length(x),2500)]

training2 <- year2015[ which(year2015$customer_id %in% inTrain),]
testing   <- year2015[ which(!(year2015$customer_id %in% inTrain)),]
training <- rbind(training, training2)

mod.training = training
mod.training$order_date <- NULL
mod.training$order_no <- NULL

fullSet <- names(mod.training)[!(names(mod.training) %in% c('customer_id','product_id'))]
#fullSet <- names(mod.training)

### Some are extremely correlated, so remove
predCorr <- cor(mod.training[,fullSet])
highCorr <- findCorrelation(predCorr, .99)
fullSet <- fullSet[-highCorr]

isNZV <- nearZeroVar(training[,fullSet], saveMetrics = TRUE, freqCut = floor(nrow(mod.training)/5))
fullSet <-  rownames(subset(isNZV, !nzv))
str(fullSet)

## Delete highly sparse predictors and zero-variance predicors
reducedSet <- rownames(subset(isNZV, !nzv & freqRatio < floor(nrow(mod.training)/50)))

mod.training.reducedSet = mod.training[,reducedSet]
mod.training.reducedSet = cbind(mod.training[,c(1:2)],mod.training.reducedSet)

############################## MODELING BEGIN ########################

# TARGET 1 (USER Product Interaction)
data1 = data.table(mod.training.reducedSet)
data1$product_id = as.character(data1$product_id)
data1 <- data1[,c('customer_id', 'product_id'), with=F]
data1 <- data1[!duplicated(data1)]
data1$target <- rep(1,nrow(data1))

# TARGET 0
# set product to select
dailies = unique(as.character(data1$product_id))

# create unique list of customer_id 
purchases = unique(as.character(data1$customer_id))
seeds <- sample(1:(10*length(purchases)), length(purchases))
items <- list()

purchases = purchases[1:5]

# set k = 5 items to be randomly sampled
k = 3
for (i in 1:length(purchases)){ # k random items for each week purchaser
  set.seed(seeds[i])                    
  items[[i]] <- sample(dailies, k)
}

items <- lapply(1:length(items), function(i){ # remove purchased items from above list
  setdiff(items[[i]], data1$product_id[which(data1$customer_id == purchases[i])])
})

reps <- unlist(lapply(1:length(items), function(i){length(items[[i]])}))
data0 <- data.frame(cbind('customer_id' = rep(purchases, reps), 'product_id'=unlist(items)))
data0$target <- rep(0,nrow(data0))

data <- rbind(data0, data1)
df <- join(data, mod.training.reducedSet, by=c('customer_id', 'product_id'))

train = data.frame(df)
model1 <- xgb.model.Dtrain.make(file = train, 
                                eta=0.005, subsample=0.6, colsample=0.025, depth=1, metric='auc', rounds=10,
                                cv = T, seed=29, feat.imp=T)

x = train[which(train$customer== 'u100017'),]

