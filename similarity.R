### R code for Feature Construction on Items-based collaborative filteringrequire(data.table)
### Author : Adi Budyanto
### Date : 20 August 2016

require(plyr)
require(data.table)
require(doParallel)

# # load pre-defined functions
# source('~/PycharmProjects/dissertation/src/v2/func.R')

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data")  
  source('~/PycharmProjects/dissertation/src/v2/func.R')
} else {
  setwd("~/Dropbox/dissertation/raw_data")
  source('~/Dropbox/dissertation/src/func.R')
}

######################################################################
#### Load the original transaction data
trans <- read.csv('data/trans.csv')
rm(list=setdiff(ls(), "trans"))

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

# create data from  selected columns
tmp <- trans[,c('customer_id','order_no','order_date','product_id')]
tmp = tmp[!duplicated(tmp),]
tmp = tmp[with(tmp, order(customer_id)), ]
                  
# partition data from transaction year 2014 only
tmp$is2015 <- year(tmp$order_date) == 2015
purchaseData <- subset(tmp, !is2015)

######################################################################
#### Load Item Features from the raw data
feat.item.global <- read.csv('features/features.item.global.csv')
names(feat.item.global)
item.features = colnames(feat.item.global)[c(1:7,143:ncol(feat.item.global))]
purchaseData <- join(purchaseData, feat.item.global[,item.features], by=c('product_id'))

######################################################################
#### Construct item-based similarity

# Product ID
prodData <- purchaseData[,c('customer_id', 'product_id')]
prodData <- prodData[!duplicated(prodData),]
prodData <- prodData[with(prodData, order(customer_id)), ]

# create product dummy variables
tmpMatrix <- data.frame(cbind(prodData, data.frame(model.matrix(~product_id-1, prodData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^..........", "",colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
prodData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
setDF(prodData) # convert back to dataframe;

# drop columns with zero transactions
prodData.final <- noZV(prodData)
colDiff.prod <- setdiff(colnames(prodData.final),colnames(prodData))

# store the final variable
# save(prodData.final, file = 'features/matrix_product.Rdata')
rm(col,prodData)

# Sub Category
subData <- purchaseData[,c('customer_id', 'subcategory')]
subData <- subData[!duplicated(subData),]
subData <- subData[with(subData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(subData, data.frame(model.matrix(~subcategory-1, subData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^...........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
subData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(subData) 

# drop columns with zero transactions
subData.final <- noZV(subData)
colDiff.sub <- setdiff(colnames(subData),colnames(subData.final))

# store the purchase_sub_cat_final variable
# save(subData.final, file = 'features/matrix_subcategory.Rdata')
rm(col,subData)

# Category
catData <- purchaseData[,c('customer_id', 'category')]
catData <- catData[!duplicated(catData),]
catData <- catData[with(catData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(catData, data.frame(model.matrix(~category-1, catData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
catData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(catData) 

# drop columns with zero transactions
catData.final <- noZV(catData)
colDiff.cat <- setdiff(colnames(catData),colnames(catData.final))

# store the purchase_sub_cat_final variable
# save(catData.final, file = 'features/matrix_category.Rdata')
rm(col,catData)

# Lens type
lensData <- purchaseData[,c('customer_id', 'lenstype')]
lensData <- lensData[!duplicated(lensData),]
lensData <- lensData[with(lensData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(lensData, data.frame(model.matrix(~lenstype-1, lensData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
lensData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(lensData) 

# drop columns with zero transactions
lensData.final <- noZV(lensData)
colDiff.lens <- setdiff(colnames(lensData),colnames(lensData.final))

# store the purchase_sub_cat_final variable
# save(lensData.final, file = 'features/matrix_lenstype.Rdata')
rm(col,lensData)

# Brand
brandData <- purchaseData[,c('customer_id', 'brand')]
brandData <- brandData[!duplicated(brandData),]
brandData <- brandData[with(brandData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(brandData, data.frame(model.matrix(~brand-1, brandData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^.....", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
brandData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(brandData) 

# drop columns with zero transactions
brandData.final <- noZV(brandData)
colDiff.brand <- setdiff(colnames(brandData),colnames(brandData.final))

# store the purchase_sub_cat_final variable
# save(brandData.final, file = 'features/matrix_brand.Rdata')
rm(col,brandData)

# Manufacturer
manuData <- purchaseData[,c('customer_id', 'manufacturer')]
manuData <- manuData[!duplicated(manuData),]
manuData <- manuData[with(manuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(manuData, data.frame(model.matrix(~manufacturer-1, manuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^............", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
manuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(manuData) 

# drop columns with zero transactions
manuData.final <- noZV(manuData)
colDiff.manu <- setdiff(colnames(manuData),colnames(manuData.final))

# store the purchase_sub_cat_final variable
# save(manuData.final, file = 'features/matrix_manufacturer.Rdata')
rm(col,manuData)

# Category lens
catlensData <- purchaseData[,c('customer_id', 'category.lens')]
catlensData <- catlensData[!duplicated(catlensData),]
catlensData <- catlensData[with(catlensData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(catlensData, data.frame(model.matrix(~category.lens-1, catlensData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^.............", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
catlensData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(catlensData) 

# drop columns with zero transactions
catlensData.final <- noZV(catlensData)
colDiff.catlens <- setdiff(colnames(catlensData),colnames(catlensData.final))

# store the purchase_sub_cat_final variable
# save(catlensData.final, file = 'features/matrix_category_lens.Rdata')
rm(col,catlensData)

# Category brand
catbrandData <- purchaseData[,c('customer_id', 'category.brand')]
catbrandData <- catbrandData[!duplicated(catbrandData),]
catbrandData <- catbrandData[with(catbrandData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(catbrandData, data.frame(model.matrix(~category.brand-1, catbrandData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^..............", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
catbrandData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(catbrandData) 

# drop columns with zero transactions
catbrandData.final <- noZV(catbrandData)
colDiff.catbrand <- setdiff(colnames(catbrandData),colnames(catbrandData.final))

# store the purchase_sub_cat_final variable
# save(catbrandData.final, file = 'features/matrix_category_brand.Rdata')
rm(col,catbrandData)

# Category mamufacturer
catmanuData <- purchaseData[,c('customer_id', 'category.manu')]
catmanuData <- catmanuData[!duplicated(catmanuData),]
catmanuData <- catmanuData[with(catmanuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(catmanuData, data.frame(model.matrix(~category.manu-1, catmanuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^.............", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
catmanuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(catmanuData) 

# drop columns with zero transactions
catmanuData.final <- noZV(catmanuData)
colDiff.catmanu <- setdiff(colnames(catmanuData),colnames(catmanuData.final))

# store the purchase_sub_cat_final variable
# save(catmanuData.final, file = 'features/matrix_category_manufacturer.Rdata')
rm(col,catmanuData)

# lenstype brand
lensbrandData <- purchaseData[,c('customer_id', 'lens.brand')]
lensbrandData <- lensbrandData[!duplicated(lensbrandData),]
lensbrandData <- lensbrandData[with(lensbrandData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(lensbrandData, data.frame(model.matrix(~lens.brand-1, lensbrandData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^..........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
lensbrandData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(lensbrandData) 

# drop columns with zero transactions
lensbrandData.final <- noZV(lensbrandData)
colDiff.catmanu <- setdiff(colnames(lensbrandData),colnames(lensbrandData.final))

# store the purchase_sub_cat_final variable
# save(lensbrandData.final, file = 'features/matrix_lenstype_brand.Rdata')
rm(col,lensbrandData)

# Lenstype manufacturer
lensmanuData <- purchaseData[,c('customer_id', 'lens.manu')]
lensmanuData <- lensmanuData[!duplicated(lensmanuData),]
lensmanuData <- lensmanuData[with(lensmanuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(lensmanuData, data.frame(model.matrix(~lens.manu-1, lensmanuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^.........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
lensmanuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(lensmanuData) 

# drop columns with zero transactions
lensmanuData.final <- noZV(lensmanuData)
colDiff.catmanu <- setdiff(colnames(lensmanuData),colnames(lensmanuData.final))

# store the purchase_sub_cat_final variable
# save(lensmanuData.final, file = 'features/matrix_lenstype_manu.Rdata')
rm(col,lensmanuData)

# Lenstype subcategory
lenssubData <- purchaseData[,c('customer_id', 'lens.subcat')]
lenssubData <- lenssubData[!duplicated(lenssubData),]
lenssubData <- lenssubData[with(lenssubData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(lenssubData, data.frame(model.matrix(~lens.subcat-1, lenssubData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^...........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
lenssubData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(lenssubData) 

# drop columns with zero transactions
lenssubData.final <- noZV(lenssubData)
colDiff.lenssub <- setdiff(colnames(lenssubData),colnames(lenssubData.final))

# store the purchase_sub_cat_final variable
# save(lenssubData.final, file = 'features/matrix_lenstype_subcategory.Rdata')
rm(col,lenssubData)

# subcategory brand
subbrandData <- purchaseData[,c('customer_id', 'subcat.brand')]
subbrandData <- subbrandData[!duplicated(subbrandData),]
subbrandData <- subbrandData[with(subbrandData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(subbrandData, data.frame(model.matrix(~subcat.brand-1, subbrandData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^............", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
subbrandData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(subbrandData) 

# drop columns with zero transactions
subbrandData.final <- noZV(subbrandData)
colDiff.subbrand<- setdiff(colnames(subbrandData),colnames(subbrandData.final))

# store the purchase_sub_cat_final variable
# save(subbrandData.final, file = 'features/matrix_subcategory_brand.Rdata')
rm(col,subbrandData)

# subcategory manufacturer
submanuData <- purchaseData[,c('customer_id', 'subcat.manu')]
submanuData <- submanuData[!duplicated(submanuData),]
submanuData <- submanuData[with(submanuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(submanuData, data.frame(model.matrix(~subcat.manu-1, submanuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^...........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
submanuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(submanuData) 

# drop columns with zero transactions
submanuData.final <- noZV(submanuData)
colDiff.submanu <- setdiff(colnames(submanuData),colnames(submanuData.final))

# store the purchase_sub_cat_final variable
# save(submanuData.final, file = 'features/matrix_subcategory_manufacturer.Rdata')
rm(col,submanuData)

# Category Lens Manufacturer
catlensmanuData <- purchaseData[,c('customer_id', 'cat.lens.manu')]
catlensmanuData <- catlensmanuData[!duplicated(catlensmanuData),]
catlensmanuData <- catlensmanuData[with(catlensmanuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(catlensmanuData, data.frame(model.matrix(~cat.lens.manu-1, catlensmanuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^.............", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
catlensmanuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(catlensmanuData) 

# drop columns with zero transactions
catlensmanuData.final <- noZV(catlensmanuData)
colDiff.catlensmanu <- setdiff(colnames(catlensmanuData),colnames(catlensmanuData.final))

# store the purchase_sub_cat_final variable
# save(catlensmanuData.final, file = 'features/matrix_category_lens_manufacturer.Rdata')
rm(col,catlensmanuData)

# Sub-Category Lens Manufacturer
subcatlensmanuData <- purchaseData[,c('customer_id', 'subcat.lens.manu')]
subcatlensmanuData <- subcatlensmanuData[!duplicated(subcatlensmanuData),]
subcatlensmanuData <- subcatlensmanuData[with(subcatlensmanuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(subcatlensmanuData, data.frame(model.matrix(~subcat.lens.manu-1, subcatlensmanuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^................", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
subcatlensmanuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(subcatlensmanuData) 

# drop columns with zero transactions
subcatlensmanuData.final <- noZV(subcatlensmanuData)
colDiff.subcatlensmanu <- setdiff(colnames(subcatlensmanuData),colnames(subcatlensmanuData.final))

# store the purchase_sub_cat_final variable
# save(subcatlensmanuData.final, file = 'features/matrix_subcategory_lens_manufacturer.Rdata')
rm(col,subcatlensmanuData)

# Brand Manufacturer
brandmanuData <- purchaseData[,c('customer_id', 'brand.manu')]
brandmanuData <- brandmanuData[!duplicated(brandmanuData),]
brandmanuData <- brandmanuData[with(brandmanuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(brandmanuData, data.frame(model.matrix(~brand.manu-1, brandmanuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^..........", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
brandmanuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(brandmanuData) 

# drop columns with zero transactions
brandmanuData.final <- noZV(brandmanuData)
colDiff.brandmnu <- setdiff(colnames(brandmanuData),colnames(brandmanuData.final))

# store the purchase_sub_cat_final variable
# save(brandmanuData.final, file = 'features/matrix_brand_manufacturer.Rdata')
rm(col,brandmanuData)

# Lens Brand Manufacturer
lensbrandmanuData <- purchaseData[,c('customer_id', 'lens.brand.manu')]
lensbrandmanuData <- lensbrandmanuData[!duplicated(lensbrandmanuData),]
lensbrandmanuData <- lensbrandmanuData[with(lensbrandmanuData, order(customer_id)), ]

# Create Product Dummy Variables
tmpMatrix <- data.frame(cbind(lensbrandmanuData, data.frame(model.matrix(~lens.brand.manu-1, lensbrandmanuData))))
# rename the first words of "product_id" in the colnames and just mention the product
colnames(tmpMatrix)[3:ncol(tmpMatrix)] <- sub("^...............", "", colnames(tmpMatrix)[3:ncol(tmpMatrix)])
# aggregate multiple columns of all products listed -> taking max instead sum (dummy variables)
col <- colnames(tmpMatrix)[3:ncol(tmpMatrix)] 
lensbrandmanuData <- setDT(tmpMatrix)[, lapply(.SD, max), by=.(customer_id), .SDcols=c(col)]
# convert back to dataframe;
setDF(lensbrandmanuData) 

# drop columns with zero transactions
lensbrandmanuData.final <- noZV(lensbrandmanuData)
colDiff.subcatlensmanu <- setdiff(colnames(lensbrandmanuData),colnames(lensbrandmanuData.final))

# store the purchase_sub_cat_final variable
# save(lensbrandmanuData.final, file = 'features/matrix_lens_brand_manufacturer.Rdata')
rm(col,lensbrandmanuData)

######################################################################
### Storing data.final dataframes
dir.create('features/features.matrix.cust.vars')

# storing customers product relationship matrix
useritemList = lapply(ls(pattern = "Data.final"), get)
save(useritemList ,file= 'features/features.matrix.cust.vars/useritems_list.Rdata')

# storing data frame names
useritemColnames = ls(pattern = "Data.final")
save(useritemColnames, file = 'features/features.matrix.cust.vars/useritems_columnNames.Rdata' )

######################################################################
#### Begin constructing : item similatiy CF

# Detect Cores that workable in CPU
getDoParWorkers()
# Use maximum number of available
cl <- makeCluster(detectCores()); registerDoParallel(cl) 

strt<-Sys.time() # start time

# 1 Product ID (p1,p2,p3)
itembasedCF.prod <- getItemBasedSimMatrix(data.purchase = prodData.final)
# save(itembasedCF.prod, file = 'features/itembasedCF_prod.Rdata')

# 2 Sub Category (dailies, two-weeklies, etc)
itembasedCF.subcategory <- getItemBasedSimMatrix(data.purchase = subData.final)
# save(itembasedCF.subcategory, file = 'features/itembasedCF_subcategory.Rdata')

# 3 Category (dailies, non-dailies, other)
itembasedCF.category <- getItemBasedSimMatrix(data.purchase = catData.final)
# save(itembasedCF.category, file = 'features/itembasedCF_category.Rdata')

# 4 Lens type (spherical, toric,)
itembasedCF.lenstype <- getItemBasedSimMatrix(data.purchase = lensData.final)
# save(itembasedCF.lenstype, file = 'features/itembasedCF_lenstype.Rdata')

# 5 Brand (acuvue,etc)
itembasedCF.brand <- getItemBasedSimMatrix(data.purchase = brandData.final)
# save(itembasedCF.brand, file = 'features/itembasedCF_brand.Rdata')

# 6 Manufacturer (jnj, etc)
itembasedCF.manufacturer <- getItemBasedSimMatrix(data.purchase = manuData.final)
# save(itembasedCF.manufacturer, file = 'features/itembasedCF_manufacturer.Rdata')

# 7 Category Lens (dailies.toric)
itembasedCF.categoryLens <- getItemBasedSimMatrix(data.purchase = catlensData.final)
# save(itembasedCF.categoryLens, file = 'features/itembasedCF_categoryLens.Rdata')

# 8 Category Brand (dailies.acuvue)
itembasedCF.categoryBrand <- getItemBasedSimMatrix(data.purchase = catbrandData.final)
# save(itembasedCF.categoryBrand, file = 'features/itembasedCF_categoryBrand.Rdata')

# 9 Category Manufacturer (dailies_jnj)
itembasedCF.categoryManu <- getItemBasedSimMatrix(data.purchase = catmanuData.final)
# save(itembasedCF.categoryManu, file = 'features/itembasedCF_categoryManu.Rdata')

# 10 Lenstype Brand (toric_acuvue)
itembasedCF.lensBrand <- getItemBasedSimMatrix(data.purchase = lensbrandData.final)
# save(itembasedCF.lensBrand, file = 'features/itembasedCF_lensBrand.Rdata')

# 11 Lenstype Manufacturer (toric_jnj)
itembasedCF.lensManu <- getItemBasedSimMatrix(data.purchase = lensmanuData.final)
# save(itembasedCF.lensManu, file = 'features/itembasedCF_lensManu.Rdata')

# 12 Lenstype Sub category (toric_dailies)
itembasedCF.lensSub <- getItemBasedSimMatrix(data.purchase = lenssubData.final)
# save(itembasedCF.lensSub, file = 'features/itembasedCF_lensSub.Rdata')

# 13 Subcategory Brand (dailies_acuvue)
itembasedCF.subBrand <- getItemBasedSimMatrix(data.purchase = subbrandData.final)
# save(itembasedCF.subBrand, file = 'features/itembasedCF_subBrand.Rdata')

# 14 Subcategory Manufacturer (dailies_jnj)
itembasedCF.subManu <- getItemBasedSimMatrix(data.purchase = submanuData.final)
# save(itembasedCF.subManu, file = 'features/itembasedCF_subManu.Rdata')

# 15 Category Lenstype Manufacturer (nondailies_toric_jnj)
itembasedCF.catLensManu <- getItemBasedSimMatrix(data.purchase = catlensmanuData.final)
# save(itembasedCF.catLensManu, file = 'features/itembasedCF_catLensManu.Rdata')

# 16 Subcategory Lenstype Manufacturer (two_weeklies_toric_jnj)
itembasedCF.subLensManu <- getItemBasedSimMatrix(data.purchase = subcatlensmanuData.final)
# save(itembasedCF.subLensManu, file = 'features/itembasedCF_subLensManu.Rdata')

# 17 Lenstype Brand Manufacturer (toric_acuvue_jnj)
itembasedCF.lensBrandManu <- getItemBasedSimMatrix(data.purchase = lensbrandmanuData.final)
# save(itembasedCF.lensBrandManu, file = 'features/itembasedCF_lensBrandManu.Rdata')

# 18 Brand Manufacturer (toric_acuvue_jnj)
itembasedCF.brandManu <- getItemBasedSimMatrix(data.purchase = brandmanuData.final)
# save(itembasedCF.lensBrandManu, file = 'features/itembasedCF_lensBrandManu.Rdata')
print(Sys.time()-strt) # end time

stopCluster(cl)

######################################################################
### Storing itembased_CF matrix
dir.create('features/features.itembasedCF')

# storing customers product relationship matrix
CFlist = lapply(ls(pattern = "itembasedCF."), get)
save(CFlist ,file= 'features/features.itembasedCF/itembasedCFs_list.Rdata')

# storing data frame names
CFcolnames= ls(pattern = "itembasedCF.")
save(CFcolnames, file = 'features/features.itembasedCF/itembasedCFs_colNames.Rdata' )


