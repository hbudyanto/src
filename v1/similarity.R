### R code for Item to Item Collaborative Filtering
### Author : Adi Budyanto
### Date : 15 August 2016

require(data.table)
require(plyr)
require(doParallel)

# Detect Cores that workable in CPU
detectCores()
getDoParWorkers()


# Load the function to run item-to-item similarity distance
source('~/PycharmProjects/dissertation/src/v1/func_v2.R')
#source("~/Dropbox/dissertation/src/v1/func.R")

# Set current working path
# setwd("~/Dropbox/dissertation/raw_data")
setwd("~/PycharmProjects/dissertation/raw_data")
# rm(list=setdiff(ls(), "trans"))

# Load the original transaction data
trans <- read.csv('data/trans.csv')

key = trans[,c('customer_id','order_no','order_date','product_id','is_lens')]
key = key[!duplicated(key),]
key = key[with(key, order(customer_id)), ]
key$order_date <- as.POSIXct(key$order_date)
key$is2015 <- year(key$order_date) == 2015

# Select only pre2015 or 2014 year of transaction
train <- subset(key, !is2015)

## Load Item Features and select relevant features
feat.item.global <- read.csv('features/features.item.global.csv')
item.features = feat.item.global[,colnames(feat.item.global)[c(1,131:139)]]

purchase <- join(train, item.features, by=c('product_id'))


### Create 9 different data sets for processing

########################## DATA PRE-PROCESSING ########################## 

## 1. Product_id
purchase_prod = purchase[,c('customer_id', 'product_id')]
purchase_prod = purchase_prod[!duplicated(purchase_prod),]
purchase_prod <- purchase_prod[with(purchase_prod, order(customer_id)), ]

# create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_prod, data.frame(model.matrix(~product_id-1, purchase_prod))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^..........", "", 
                                                                    colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_prod <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_prod) # convert back to dataframe;

# drop columns with zero transactions
purchase_prod_final <- noZV(purchase_prod)
colDiff_prod <- setdiff(colnames(purchase_prod),colnames(purchase_prod_final))

# store the final variable
save(purchase_prod_final, file = 'features/matrix_2014_item_product.Rdata')
rm(col,purchase_prod)

## 2. Category Type
purchase_cat = purchase[,c('customer_id', 'category_type')]
purchase_cat = purchase_cat[!duplicated(purchase_cat),]
purchase_cat <- purchase_cat[with(purchase_cat, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_cat, data.frame(model.matrix(~category_type-1, purchase_cat))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.............", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_cat <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_cat) # convert back to dataframe;

# drop columns with zero transactions
purchase_cat_final <- noZV(purchase_cat)
colDiff_cat <- setdiff(colnames(purchase_cat),colnames(purchase_cat_final))

# store the final variable
save(purchase_cat_final, file = 'features/matrix_2014_item_category_type.Rdata')
rm(col,purchase_cat)

## 3. Category Brand
purchase_brand = purchase[,c('customer_id', 'category_brand')]
purchase_brand = purchase_brand[!duplicated(purchase_brand),]
purchase_brand <- purchase_brand[with(purchase_brand, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_brand, data.frame(model.matrix(~category_brand-1, purchase_brand))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^..............", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_brand <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_brand) # convert back to dataframe;

# drop columns with zero transactions
purchase_brand_final <- noZV(purchase_brand)
colDiff_brand <- setdiff(colnames(purchase_brand),colnames(purchase_brand_final))

# store the final variable
save(purchase_brand_final, file = 'features/matrix_2014_item_category_brand.Rdata')
rm(col,purchase_brand)

## 4. Category Manufacturer
purchase_manu = purchase[,c('customer_id', 'category_manufacturer')]
purchase_manu = purchase_manu[!duplicated(purchase_manu),]
purchase_manu <- purchase_manu[with(purchase_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_manu, data.frame(model.matrix(~category_manufacturer-1, purchase_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.....................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_manu) # convert back to dataframe;

# drop columns with zero transactions
purchase_manu_final <- noZV(purchase_manu)
colDiff_manu <- setdiff(colnames(purchase_manu),colnames(purchase_manu_final))

# store the final variable
save(purchase_manu_final, file = 'features/matrix_2014_item_category_manufacturer.Rdata')
rm(col,purchase_manu)

## 5. Category Type Manufacturer
purchase_cat_manu = purchase[,c('customer_id', 'category_type_manufaturer')]
purchase_cat_manu = purchase_cat_manu[!duplicated(purchase_manu),]
purchase_cat_manu <- purchase_cat_manu[with(purchase_cat_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_cat_manu, data.frame(model.matrix(~category_type_manufaturer-1, purchase_cat_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.........................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_cat_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_cat_manu) # convert back to dataframe;

# drop columns with zero transactions
purchase_cat_manu_final <- noZV(purchase_cat_manu)
colDiff_cat_manu <- setdiff(colnames(purchase_cat_manu),colnames(purchase_cat_manu_final))

# store the final variable
save(purchase_cat_manu_final, file = 'features/matrix_2014_item_category_type_manufacturer.Rdata')
rm(col,purchase_cat_manu)

## 6. Sub Category Type
purchase_sub_cat = purchase[,c('customer_id', 'subcategory_type')]
purchase_sub_cat = purchase_sub_cat[!duplicated(purchase_sub_cat),]
purchase_sub_cat <- purchase_sub_cat[with(purchase_sub_cat, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_cat, data.frame(model.matrix(~subcategory_type-1, purchase_sub_cat))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_cat <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_cat) # convert back to dataframe;

# drop columns with zero transactions
purchase_sub_cat_final <- noZV(purchase_sub_cat)
colDiff_sub_cat <- setdiff(colnames(purchase_sub_cat),colnames(purchase_sub_cat_final))

# store the purchase_sub_cat_final variable
save(purchase_sub_cat_final, file = 'features/matrix_2014_item_subcategory_type.Rdata')
rm(col,purchase_sub_cat)

## 7. Sub Category Brand
purchase_sub_brand = purchase[,c('customer_id', 'subcategory_brand')]
purchase_sub_brand = purchase_sub_brand[!duplicated(purchase_sub_brand),]
purchase_sub_brand <- purchase_sub_brand[with(purchase_sub_brand, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_brand, data.frame(model.matrix(~subcategory_brand-1, purchase_sub_brand))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^.................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_brand <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_brand) # convert back to dataframe;

# drop columns with zero transactions
purchase_sub_brand_final <- noZV(purchase_sub_brand)
colDiff_sub_brand <- setdiff(colnames(purchase_sub_brand),colnames(purchase_sub_brand_final))

# store the purchase_sub_cat_final variable
save(purchase_sub_brand_final, file = 'features/matrix_2014_item_subcategory_brand.Rdata')
rm(col,purchase_sub_brand)

## 8. Sub Category Manufacturer
purchase_sub_manu = purchase[,c('customer_id', 'subcategory_manufacturer')]
purchase_sub_manu = purchase_sub_manu[!duplicated(purchase_sub_manu),]
purchase_sub_manu <- purchase_sub_manu[with(purchase_sub_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_manu, data.frame(model.matrix(~subcategory_manufacturer-1, purchase_sub_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^........................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_manu) # convert back to dataframe;

# drop columns with zero transactions
purchase_sub_manu_final <- noZV(purchase_sub_manu)
colDiff_sub_cat <- setdiff(colnames(purchase_sub_manu),colnames(purchase_sub_manu_final))

# store the purchase_sub_cat_final variable
save(purchase_sub_manu_final, file = 'features/matrix_2014_item_subcategory_manufacturer.Rdata')
rm(col,purchase_sub_manu)

## 9. Category Type Manufacturer
purchase_sub_cat_manu = purchase[,c('customer_id', 'subcategory_type_manufaturer')]
purchase_sub_cat_manu = purchase_sub_cat_manu[!duplicated(purchase_sub_cat_manu),]
purchase_sub_cat_manu <- purchase_sub_cat_manu[with(purchase_sub_cat_manu, order(customer_id)), ]

# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub_cat_manu, data.frame(model.matrix(~subcategory_type_manufaturer-1, purchase_sub_cat_manu))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^............................", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub_cat_manu <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub_cat_manu) # convert back to dataframe;

# drop columns with zero transactions
purchase_sub_cat_manu_final <- noZV(purchase_sub_cat_manu)
colDiff_sub_cat_manu <- setdiff(colnames(purchase_sub_cat_manu),colnames(purchase_sub_cat_manu_final))

# store the purchase_sub_cat_final variable
save(purchase_sub_cat_manu_final, file = 'features/matrix_2014_item_subcategory_type_manufacturer.Rdata')
rm(col,purchase_sub_cat_manu)

# save vectors related to zero transactions
save(colDiff_sub_cat_manu, colDiff_sub_cat, colDiff_sub_brand, colDiff_cat_manu,colDiff_manu,
     colDiff_brand, colDiff_cat, colDiff_prod, file = 'features/vector_2014_vars_missingtrans.Rdata')

## 10. Sub Category
purchase_sub = purchase[,c('customer_id', 'subcategory')]
purchase_sub = purchase_sub[!duplicated(purchase_sub),]
purchase_sub <- purchase_sub[with(purchase_sub, order(customer_id)), ]


# Create Product Dummy Variables
purchase_matrix <- data.frame(cbind(purchase_sub, data.frame(model.matrix(~subcategory-1, purchase_sub))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(purchase_matrix)[3:ncol(purchase_matrix)] <- sub("^...........", "", colnames(purchase_matrix)[3:ncol(purchase_matrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(purchase_matrix)[3:ncol(purchase_matrix)] 
purchase_sub <- setDT(purchase_matrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(purchase_sub) # convert back to dataframe;

# drop columns with zero transactions
purchase_sub_final <- noZV(purchase_sub)
colDiff_sub <- setdiff(colnames(purchase_sub),colnames(purchase_sub_final))

# store the purchase_sub_cat_final variable
save(purchase_sub_final, file = 'features/matrix_2014_item_subcategory.Rdata')
rm(col,purchase_sub)

# save vectors related to zero transactions
save(colDiff_sub_cat_manu, colDiff_sub_cat, colDiff_sub_brand, colDiff_cat_manu,colDiff_manu,
     colDiff_brand, colDiff_cat, colDiff_prod, colDiff_sub, file = 'features/vector_2014_vars_missingtrans.Rdata')

########################## BEGIN CALCULATING ITEM SIMILARITY ########################## 


## 1. Product_id
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix <- getItemBasedSimMatrix(data.purchase = purchase_prod_final)
save(item.matrix, file = 'features/user_scores_prod.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 2. Category Type
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.cat.lenstype <- getAffinityMatrix(data.purchase = purchase_cat_final)
save(item.matrix.cat.lenstype, file = 'features/user_scores_cat_lenstype.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 3. Category Brand
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.cat.brand <- getAffinityMatrix(data.purchase = purchase_brand_final)
save(item.matrix.cat.brand, file = 'features/user_scores_cat_brand.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 4. Category Manufacturer
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.cat_manu <- getAffinityMatrix(data.purchase = purchase_manu_final)
save(item.matrix.cat_manu, file = 'features/user_scores_cat_manu.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 5. Category Type Manufacturer
# Get affinity user-item matrix
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.cat.lenstype.manu <- getAffinityMatrix(data.purchase = purchase_cat_manu_final)
save(item.matrix.cat.lenstype.manu , file = 'features/user_scores_cat_lenstypemanu.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 6. Sub Category Type
# Get affinity user-item matrix
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.subcat.lenstype <- getAffinityMatrix(data.purchase = purchase_sub_cat_final)
save(item.matrix.subcat.lenstype, file = 'features/user_scores_subcat_lenstype.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 7. Sub Category Brand
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.subcat.brand <- getAffinityMatrix(data.purchase = purchase_sub_brand_final)
save(item.matrix.subcat.brand , file = 'features/user_scores_subcat_brand.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 8. Sub Category Manufacturer
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.subcat.manu <- getAffinityMatrix(data.purchase = purchase_sub_manu_final)
save(item.matrix.subcat.manu , file = 'features/user_scores_subcat_manu.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 9. Category Type Manufacturer
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.subcat.lenstype.manu <- getAffinityMatrix(data.purchase = purchase_sub_cat_manu_final)
save(item.matrix.subcat.lenstype.manu , file = 'features/user_scores_subcat_lenstypemanu.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

## 10. Sub Category
cl <- makeCluster(12); registerDoParallel(cl) 
strt<-Sys.time() # start time
item.matrix.subcat <- getAffinityMatrix(data.purchase = purchase_sub_final)
save(item.matrix.subcat , file = 'features/user_scores_subcat.Rdata')
print(Sys.time()-strt) # end time
# Stop the cluster
stopCluster(cl)

