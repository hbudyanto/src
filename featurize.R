### R code for Feature Construction on Items and Customers
### Author : Adi Budyanto
### Date : 20 August 2016

require(data.table)
require(plyr)

# load pre-defined functions
source('~/PycharmProjects/dissertation/src/v2/func.R')

# set working path in Desktop
if (Sys.info()[1] == 'Darwin') {
  setwd("~/PycharmProjects/dissertation/raw_data")  
} else {
  setwd("~/Dropbox/dissertation/raw_data")
}

#### Read in the product data table in it's raw form 
prod <- read.csv('data/prod.csv')

# character
prod$product_id <- as.character(prod$product_id)
prod$manufacturer <- as.character(prod$manufacturer)
# date
prod$created_at <- as.POSIXct(prod$created_at)
prod$updated_at <- as.POSIXct(prod$updated_at)
# numeric
prod$pack_qty <- as.numeric(as.character(prod$pack_qty))
prod$pack_qty[is.na(prod$pack_qty)] <- 0

# featurize brand
prod$brand <- as.character(tolower(gsub('([[:punct:]])|\\s+','',prod$brand)))
prod$brand[is.na(prod$brand)] <- 'unk'
prod$brand <- factor(paste("brand.",prod$brand, sep=""))

# featurize manufacturer
prod$manufacturer <- as.character(tolower(gsub('([[:punct:]])|\\s+','',prod$manufacturer)))
prod$manufacturer[is.na(prod$manufacturer)] <- 'unk'
prod$manufacturer <- factor(paste("manu.",prod$manufacturer, sep=""))

# featurize sub category
prod$subcategory <- as.character(tolower(gsub('([[:punct:]])|\\s+','',prod$category)))
prod$subcategory[is.na(prod$subcategory)] <- 'unk'
prod$subcategory <- factor(paste("subcat.",prod$subcategory, sep=""))

# featurize contact lens type
prod$type <- as.character(tolower(gsub('([[:punct:]])|\\s+','',prod$type)))
prod$type[prod$type== ""] <- 'nonLens'
prod$type<- factor(paste("lens.",prod$type, sep=""))

# feature category
prod$category <- tolower(gsub('([[:punct:]])|\\s+','',prod$category))
prod$category <- as.factor(prod$category)
prod$mod.category <- prod$category
levels(prod$mod.category) = c('dailies','other','solution','nondailies',
                              'dailies','eye_care','eye_care','eye_care',
                              'solution','solution','eye_care','eye_care',
                              'nondailies','solution','nondailies','eye_care',
                              'other','other','nondailies','solution',
                              'nondailies')
prod$mod.category <- factor(paste("cat.",prod$mod.category, sep=""))

# refactoring days perlens
prod$mod.daysperlens <- prod$daysperlens
levels(prod$mod.daysperlens) <- c('other','1','14','30','other')


# refactoring discountinued product
prod$product_lifecycle <- as.character(prod$product_lifecycle)
prod$product_lifecycle[prod$product_lifecycle %in%  c("NULL","")] <- 'unk'
prod$product_lifecycle <- factor(paste("lifecycle.",prod$product_lifecycle, sep=""))

# store the files
write.csv(prod,'data/mod_prod.csv')

######################################################################
#### Global features for item profiles
feat.item.global = data.frame(cbind('product_id'= prod$product_id,
                                    'manufacturer'= prod$manufacturer,
                                    'category' = prod$mod.category,
                                    'subcategory' = prod$subcategory,
                                    'lenstype'= prod$type,
                                    'product.lifecycle' = prod$product_lifecycle,
                                    'brand' =  prod$brand,
                                    'pack_qty' = prod$pack_qty))

# featurize manufacturer
levels(feat.item.global$manufacturer) <-   levels(prod$manufacturer)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~manufacturer-1, feat.item.global))))

# featurize category
levels(feat.item.global$category) <-   levels(prod$mod.category)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~category-1, feat.item.global))))

# featurize category
levels(feat.item.global$subcategory) <-   levels(prod$subcategory)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~category-1, feat.item.global))))

# featurize lens type
levels(feat.item.global$lenstype) <-   levels(prod$type)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~lenstype-1, feat.item.global))))

# featurize product_lifecycle
levels(feat.item.global$product.lifecycle) <-   levels(prod$product_lifecycle)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~product.lifecycle-1, feat.item.global))))

# featurize brand
levels(feat.item.global$brand) <-   levels(prod$brand)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~brand-1, feat.item.global))))

# featurize telesales and promotional product
feat.item.global$tele.promo <- ifelse(prod$telesales_only == 0 & prod$promotional_product ==1, 'nontele.promo',
              ifelse(prod$telesales_only == 0& prod$promotional_product ==1, 'nontele.promo', 
                     ifelse(prod$telesales_only == 1 & prod$promotional_product ==0, 'tele.nonpromo','tele.promo')))

feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~tele.promo-1, feat.item.global))))

# feature construction for item-based similarity on category
# category 
feat.item.global$category.lens <- paste(prod$mod.category,'_',prod$type, sep='')
feat.item.global$category.brand <- paste(prod$mod.category,'_',prod$brand, sep='')
feat.item.global$category.manu <- paste(prod$mod.category,'_',prod$manufacturer, sep='')
# lens type
feat.item.global$lens.brand <- paste(prod$type,'_',prod$brand, sep='')
feat.item.global$lens.manu <- paste(prod$type,'_',prod$manufacturer, sep='')
feat.item.global$lens.subcat <- paste(prod$type,'_',prod$subcategory, sep='')
# sub category
feat.item.global$subcat.brand <- paste(prod$subcategory,'_',prod$brand, sep='')
feat.item.global$subcat.manu <- paste(prod$subcategory,'_',prod$manufacturer, sep='')
# cat <> lens type <> manufactuer
feat.item.global$cat.lens.manu <- paste(prod$mod.category,'_',prod$type,'_', prod$manufacturer,sep='')
feat.item.global$subcat.lens.manu <- paste(prod$subcategory,'_',prod$type,'_', prod$manufacturer,sep='')
# brand <> manu
feat.item.global$brand.manu <- paste(prod$brand,'_',prod$manufacturer, sep='')
# lens type <> brand <> manu
feat.item.global$lens.brand.manu <- paste(prod$type,'_',prod$brand,'_',prod$manufacturer, sep='')

# drop near zero variance predictors
feat.item.global <- noZV(feat.item.global)
# store item features table
write.table(feat.item.global, 'features/features.item.global.csv', row.names = F, col.names = T, sep=',')

######################################################################
#### Read in the customer data table in it's raw form 
cust <- read.csv('data/cust.csv')
# character
cust$customer_id = as.character(cust$customer_id)
# dates
cust$created_at = as.POSIXct(cust$created_at)
cust$updated_at = as.POSIXct(cust$updated_at)
cust$first_order_date = as.POSIXct(cust$first_order_date)
cust$last_order_date = as.POSIXct(cust$last_order_date)

# arrange by created at
cust = cust[with(cust, order(created_at)), ]

# set new factors for gender
cust$gender <- as.character(cust$gender)
cust$gender[cust$gender %in% c("","Edge","Zeitler","Brocklesby")] <- "Unk"
cust$gender <- factor(paste("gender",cust$gender, sep=""))

# set new factors for days_worn behaviour
cust$days_worn <- as.character(cust$days_worn)
cust$days_worn[ cust$days_worn %in% c("Yes","N/A","NULL")] <- "durationDaysWornZero" # durationDaysWornUnk (grouped upon data analysis)
cust$days_worn[ cust$days_worn %in% c("0","No","I don't wear lenses")] <- "durationDaysWornZero"
cust$days_worn[ cust$days_worn %in% c("1 day in a week","2 days in a week","3 days in a week")] <- "durationDaysWorn1to3"
cust$days_worn[ cust$days_worn %in% c("4 days in a week","5 days in a week","6 days in a week")] <- "durationDaysWorn4to6"
cust$days_worn[ cust$days_worn == "7 days in a week"] <- "durationDaysWorn7"
cust$days_worn <- factor(cust$days_w)

# set new factors for unsubscribe all
cust$unsubscribe_all <- tolower(as.character(cust$unsubscribe_all))
cust$unsubscribe_all[ cust$unsubscribe_all %in% c("yes","yes ","1")] <- "optOutAllMarketingYes"
cust$unsubscribe_all[ cust$unsubscribe_all %in% c("0","no","","null","2")] <- "optOutAllMarketingNo"
cust$unsubscribe_all <- factor(cust$unsubscribe_all)

######################################################################
#### Global features for customers profiles
feat.cust.global <- data.frame(cbind('customer_id'= cust$customer_id,
                                     'gender'=cust$gender,
                                     'unsubscribe.all' =cust$unsubscribe_all,
                                     'days.worn'=cust$days_worn))

# featurize first order date of user_id
feat.cust.global$first_order_date = cust$first_order_date
feat.cust.global$first_order_date_year = as.POSIXlt(feat.cust.global$first_order_date)$year + 1900
feat.cust.global$first_order_date_mon = as.POSIXlt(feat.cust.global$first_order_date)$mon + 1
feat.cust.global$first_order_date_mday = as.POSIXlt(feat.cust.global$first_order_date)$mday 
feat.cust.global$first_order_date_wday = as.POSIXlt(feat.cust.global$first_order_date)$wday + 1
feat.cust.global$first_order_date_yday = as.POSIXlt(feat.cust.global$first_order_date)$yday + 1

# featurize last order date of user_id
feat.cust.global$last_order_date = cust$last_order_date
feat.cust.global$last_order_date_year = as.POSIXlt(feat.cust.global$last_order_date)$year + 1900
feat.cust.global$last_order_date_mon = as.POSIXlt(feat.cust.global$last_order_date)$mon + 1
feat.cust.global$last_order_date_mday = as.POSIXlt(feat.cust.global$last_order_date)$mday 
feat.cust.global$last_order_date_wday = as.POSIXlt(feat.cust.global$last_order_date)$wday + 1
feat.cust.global$last_order_date_yday = as.POSIXlt(feat.cust.global$last_order_date)$yday + 1

# featurize tenure (last_order_date - first_order_date)
feat.cust.global$tenure = (as.numeric(feat.cust.global$last_order_date) - as.numeric(feat.cust.global$first_order_date))/(60*60*24)

# featurize gender
levels(feat.cust.global$gender) <-   levels(cust$gender)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~gender-1, feat.cust.global))))

# featurize business channel
levels(feat.cust.global$unsubscribe.all) <-   levels(cust$unsubscribe_all)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~unsubscribe.all-1, feat.cust.global))))

# featurize days worn
levels(feat.cust.global$days.worn) <-  levels(cust$days_worn)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~days.worn-1, feat.cust.global))))

# drop near zero variance predictors
feat.cust.global <- noZV(feat.cust.global)
# store global customer profiles table
write.table(feat.cust.global, 'features/features.cust.global.csv', row.names = F, col.names = T, sep=',')
