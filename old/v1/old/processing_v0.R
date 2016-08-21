#########################################
## ========== PROCESSING
#########################################

## required input : customer.Rdata, trans.Rdata, report2.Rdata, dotmailer.Rdata, product.Rdata
## main obj : subset data only for dailies customer & subtract variables from all files that are useful for features
## ouput : prod.csv, web.csv, cust.csv, subscribe.csv, trans.csv
## author : HAB, 21 July 2016

#########################################
## ========== Clean Up Transaction Data
#########################################
# Seth Working Path
setwd("~/PycharmProjects/dissertation/raw_data")

flog.info("Load Transaction Table")
# Load Transactions Data
load('transaction.Rdata')
trans = trans[which(!(trans$order_no %in% c(90,180,20,30,160,170,10,105,70,175,60,110,15,95,140,80,120,150,75,40,165,5,145,50,130
                                            ,35,85,45,55,155,115,65,135,100,"")) & !(trans$customer_id %in% c('NULL','',-1)) &
                      !is.na(trans$order_date) & !(trans$product_id %in% c('NULL',23.2758,"","GB","Seoul"))),]

tmp.key = trans[ ,c(2,3,4,18)]
#tmp.key$order_date = as.POSIXct(as.Date(tmp.key$order_date,"%Y-%m-%d"))
#tmp.key$product_id = as.factor(tmp.key$product_id)

flog.info("Load Product Table")
# Load Product Description
load('product.Rdata')
colnames(prod)[1] = 'product_id'
tmp.prod = prod[, c('product_id','category')]
#t.prod$product_id = as.factor(t.prod$product_id)
tmp.prod$category = as.factor(tmp.prod$category)

# Join the tables
tmp.key = merge(x=tmp.key, y=tmp.prod, by= "product_id", all.x=T)
tmp.key = tmp.key[!duplicated(tmp.key),]
# arrange by order_no
tmp.key = tmp.key[ order(tmp.key[,c("order_no")]), ]

# create contingential tables for "category"
tmp.key = data.frame(cbind(tmp.key, data.frame(model.matrix(~category-1, tmp.key))))
tmp.key[,c("product_id","category")] <- NULL

# require reshape2 or data.table
tmp.melt <- melt(tmp.key, id = c("order_date", "order_no","customer_id"))
tmp= dcast(tmp.melt, order_date + order_no + customer_id ~ variable, sum)
# get rid of transactions which involve purchase of two-weeklies and monthlies
flog.info("Get the Key IDs : Order_Date, OrderNo, Customer_ID")
key = tmp[-which(tmp$categoryTwo.Weeklies > 0 | tmp$categoryMonthlies > 0),c("order_date", "order_no","customer_id")]
rm(tmp, tmp.melt, tmp.key ) # clear up some space 

# get unique customer_id
key.cust = unique(key$customer_id)

###################################################################################
## ========== Clean Up Customer data
###################################################################################
flog.info("Clean Up Customer Table")
load('customer_v1.Rdata')
colnames(cust)[1] <- 'customer_id'
# get selected variables to be stored further
cust = cust[which(cust$customer_id %in% key.cust),c(1:4,8,10,11,13:ncol(cust))]

cust$created_at = as.POSIXct(as.Date(cust$created_at,"%Y-%m-%d"))
cust$updated_at= as.POSIXct(as.Date(cust$updated_at,"%Y-%m-%d"))
cust$first_order_date = as.POSIXct(as.Date(cust$first_order_date,"%Y-%m-%d")) 
cust$last_order_date = as.POSIXct(as.Date(cust$last_order_date,"%Y-%m-%d"))

# length(unique(c.cust$customer_id)) # count of unique user_id : 264876
# Features : - days_tenure_date : testing_week - first_order_date, - days_since_last_order : testing_week - last_order_date
# gender, type of days_worn, store_name, business_channel, days_since_last_update : testing_week - last_update
# TBE :  -

###################################################################################
## ========== Clean Up Dotmailer
###################################################################################
flog.info("Clean Up Dotmailer Table")
load('dotmailer.Rdata')
colnames(dotmailer)[1] <- 'customer_id'
dotmailer = dotmailer[which(dotmailer$customer_id %in% key.cust),]

dotmailer$Date = as.POSIXct(as.Date(dotmailer$Date, "%d/%m/%Y"))
dotmailer$store_name <- NULL

# Features : - Subscribed Date, Unsubsribed Date, Subscription Status
# TBE :  -

# ORIGINAL
# length(unique(dotmailer$customer_id)) #326586 unique users
# min(dotmailer$Date) # 2nd Sept 2014
# AFTER FILTERING OUT
# length(unique(dotmailer$customer_id)) #86072 unique users

###################################################################################
## ========== Clean Up Report 1 Data
###################################################################################
flog.info("Clean Up Web Report 1 Data")
load('report1.Rdata')
str(report1); colnames(report1)[3] = 'customer_id'
# length(unique(report1$customer_id)) # 243597 unique users
# min(as.POSIXct(as.Date(as.character(report1$date), "%Y%m%d"))) # 2nd Sept 2014
report1$date = as.POSIXct(as.Date(as.character(report1$date), "%Y%m%d"))
report1$customer_id = as.character(report1$customer_id)
# 
# # Features : - minutes spend on the website during the website, source medium, device category
# # TBE :  - whether session is ended with purchase (if possible), something from pagepath, contentGroup2 (page_views)
# 
# # note : report 1 will not be used as report 2 contains more information

###################################################################################
## ========== Clean Up Report 2 Data
###################################################################################
flog.info("Clean Up Web Report 2 Data")
load('report2.Rdata') 
report2 = report2[which(report2$customer_id %in% key.cust), -9]

report2$date = as.POSIXct(report2$date)
report2$customer_id = as.character(report2$customer_id)

# ORIGINAL
# length(unique(report2$customer_id)) #326586 unique users
# min(as.POSIXct(report2$date)) # 2nd Sept 2014
# AFTER FILTERING OUT
# length(unique(report2$customer_id)) #215639 unique users
# min(report2$date) # 2nd Sept 2014

# Features : source medium, device category, type of produce user looked at (use for random sampling) eventCategory == 'add-to-basket'
# TBE :  - minutes spend on the website during the website, other information from eventCat, eventAct, product viewed but not purchased

###################################################################################
## ========== Reindexing User_ID
###################################################################################
# create directories for storing data
dir.create('data')
dir.create('data/keys')

C <- levels(as.factor(cust$customer_id))
D <- levels(as.factor(dotmailer$customer_id))
R2 <- levels(as.factor(report2$customer_id))
R1 <- levels(as.factor(report1$customer_id))

customer_id <- unique(c(key.cust,C,D,R1,R2))
tmp <- data.frame(cbind('customer_id'=customer_id, 'cust_id'= paste0('u', 1:length(customer_id))))
rownames(tmp) <- tmp$customer_id
write.csv(tmp, 'data/keys/customer_id.key')

cust$customer_id <- tmp[as.character(cust$customer_id), 2]
dotmailer$customer_id <- tmp[as.character(dotmailer$customer_id), 2]
report2$customer_id <- tmp[as.character(report2$customer_id), 2]

write.csv(report2, 'data/web.csv', row.names = F, col.names = T, sep=',')
write.csv(cust, 'data/cust.csv', row.names = F, col.names = T, sep=',')
write.csv(dotmailer, 'data/subscribe.csv', row.names = F, col.names = T, sep=',')

# Do the same for trans but first filter only selected customer_id
colnames(trans)[1] = 'store_name'
#colnames(trans)[-c(7:8,11,13:15,17,20:28,35:36,41:42)] # drop those columns in trans
trans = trans[which(trans$customer_id %in% customer_id),-c(7:8,11,13:15,17,20:28,35:36,41:42)]
trans$customer_id <- tmp[as.character(trans$customer_id), 2]
trans$order_date = as.POSIXct(as.Date(trans$order_date,"%Y-%m-%d"))
trans$invoice_date = as.POSIXct(as.Date(trans$invoice_date,"%Y-%m-%d"))
trans$shipment_date = as.POSIXct(as.Date(trans$shipment_date,"%Y-%m-%d"))
trans$reminder_date = as.POSIXct(as.Date(trans$reminder_date,"%Y-%m-%d"))
trans$product_id = as.factor(trans$product_id)

#release some memory
rm(tmp,C,D,R, customer_id, key.cust)

#########################################
## ========== Get New Product Table
#########################################
# get unique product_id list (should be without two-weeklies & monthlies)
prod_id = unique(trans$product_id)
prod = prod[which(prod$product_id %in% prod_id),c(-7)]
prod$product_id = as.factor(prod$product_id)

prod$product_id = as.factor(prod$product_id)
prod$created_at = as.POSIXct(as.Date(prod$created_at,"%Y-%m-%d"))
prod$updated_at= as.POSIXct(as.Date(prod$updated_at,"%Y-%m-%d"))


T = levels(trans$product_id)
P = levels(prod$product_id)
prod_id = unique(c(T,P))
tmp <- data.frame(cbind('product_id'=prod_id, 'prod_id'=paste0('p', 1:length(prod_id))))
rownames(tmp) <- tmp$product_id
write.csv(tmp, 'data/keys/purchase_id.key')
trans$product_id <- tmp[as.character(trans$product_id), 2]
prod$product_id <- tmp[as.character(prod$product_id), 2]

# clear some space
rm(T,P,prod_id,tmp, tmp.prod)

write.csv(prod, 'data/prod.csv', row.names = F, col.names = T, sep=',')
write.csv(trans, 'data/trans.csv', row.names = F, col.names = T, sep = ',')
