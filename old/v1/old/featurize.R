########################################################
## ========== FEATURE ENGINEERING
########################################################


source('~/PycharmProjects/dissertation/src/v1/func_v1.R') #data cleansing
setwd("~/PycharmProjects/dissertation/raw_data")

########################################################
## ========== LITTLE COSMETIC MODIFICATION OF CUST TABLE
########################################################

cust <- read.csv('data/cust.csv')
cust$customer_id = as.character(cust$customer_id)

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

# Features to consider :
# gender, unsubscribe_all, days_worn, tenure (last_order_date - first_order_date), 

########################################################
## ========== USER FEATURE GLOBAL
########################################################
dir.create('features')
feat.cust.global <- data.frame(cbind('customer_id'=as.character(cust$customer_id),
                                     'gender'=cust$gender,
                                     'unsubscribe_all' =cust$unsubscribe_all,
                                    'days_worn'=as.factor(cust$days_worn)))

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
levels(feat.cust.global$unsubscribe_all) <-   levels(cust$unsubscribe_all)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~unsubscribe_all-1, feat.cust.global))))

# featurize days worn
levels(feat.cust.global$days_worn) <-  levels(cust$days_worn)
feat.cust.global = data.frame(cbind(feat.cust.global, data.frame(model.matrix(~days_worn-1, feat.cust.global))))

# names(feat.cust.global[,c(2:4)]) : -> gender, unsubscribe_all, days_worn
feat.cust.global[,c(2:4)] <- NULL
write.table(feat.cust.global, 'features/features.cust.global.csv', row.names = F, col.names = T, sep=',')

########################################################
## ========== LITTLE COSMETIC MODIFICATION OF PROD TABLE
########################################################
prod <- read.csv('data/prod.csv')
brand <- read.csv('data/keys/name_brand.csv')

prod <- join(prod, brand[,c("name","mod_brand")], by = c('name'))
colnames(prod)[20] <- 'brand'

prod$product_id = as.character(prod$product_id)
prod$brand = as.character(prod$brand)
prod$created_at = as.POSIXct(prod$created_at)
prod$updated_at = as.POSIXct(prod$updated_at)

# featurize brand
prod$brand = as.character(tolower(gsub('([[:punct:]])|\\s+','',prod$brand)))
prod$brand[is.na(prod$brand)] <- 'unknown'
prod$brand <- factor(paste("brand",prod$brand, sep=""))

# featurize type
prod$type = as.character(prod$type)
prod$type[prod$type== ""] <- 'nonLens'
prod$type<- factor(paste("typeLens",prod$type, sep=""))

# refactoring manufacturer
prod$mod.manufacturer = prod$manufacturer
levels(prod$mod.manufacturer) = c('other','other','alcon', 'other', 'altacor',
                                  'amo','bausch_Lomb','ciba_vision', 'other','coopervision',
                                  'eye_care_cosmetics','other','other','other','other',
                                  'other', 'johnson_johnson','other','other','other',
                                  'other','other','other','other','other',
                                  'other','rl_vision','sauflon','other','other',
                                  'other','vision_direct')

# refactoring category
# only acuvue - johnson-johnson : 1 item --> include in nondailie along with monthlies
prod$mod.category = prod$category
levels(prod$mod.category) = c('dailies','other','solution','nondailies',
                              'dailies','eye_care','eye_care','eye_care',
                              'solution','solution','eye_care','eye_care',
                              'nondailies','solution','nondailies','eye_care',
                              'other','other','nondailies','solution',
                              'nondailies')

# x <- levels(prod$mod.category)
# x <- data.frame(x)
# x$var <-  c('dailies','other','solution','colours','dailies',
#           'eye_care','eye_care','eye_care','solution','solution',
#           'eye_care','eye_care','nondailies','solution','dailies',
#           'eye_care','other','other','nondailies','solution',
#           'nondailies')
# 
# write.csv(x,'audit/mod_category.csv')

# refactoring days perlens
prod$mod.daysperlens <- prod$daysperlens
levels(prod$mod.daysperlens) <- c('other','1','14','30','other')

# refactoring discountinued product
prod$mod.product_lifecycle <- prod$product_lifecycle
levels(prod$mod.product_lifecycle) <- c('other','discountinued','other','regular')

# refactoring pack qty
prod$mod.pack_qty <- prod$pack_qty
levels(prod$mod.pack_qty) <- c('1','more_than10','2','more_than10','3_to6',
                               'more_than10','3_to6','3_to6','more_than10','1')


# create new features with sub_category + variables
# prod$lenstype = ifelse((prod$mod.category == 'dailies' & prod$type == 'typeLensSpherical'), 'dailiesSpherical', 
#                                              ifelse(prod$mod.category == 'dailies' & prod$type == 'typeLensToric', 'dailiesToric',
#                                                     ifelse(prod$mod.category == 'dailies' & prod$type== 'typeLensMultifocal','dailiesMultifocal',
#                                                            ifelse(prod$mod.category == 'nondailies' & prod$type == 'typeLensSpherical', 'nondailiesSpherical', 
#                                                                   ifelse(prod$mod.category == 'nondailies' & prod$type == 'typeLensToric', 'nondailiesToric',
#                                                                          ifelse(prod$mod.category == 'nondailies' & prod$type == 'typeLensMultifocal','nondailiesMultifocal','nonlens'))))))


# create new features with Category + variables

prod$category_type = paste(prod$mod.category,'_',prod$type, sep='')
prod$category_brand = paste(prod$mod.category,'_',prod$brand,sep='')
prod$category_manufacturer = paste(prod$mod.category,'_',prod$mod.manufacturer,sep='')
prod$category_type_manufaturer = paste(prod$mod.category,'_',prod$type,prod$mod.manufacturer,sep='')


# Reformatting all character -> tokenize 
prod$subcategory = tolower(gsub('([[:punct:]])|\\s+','',prod$category))

# create new features with Sub Category + variables
prod$subcategory_type = paste(prod$subcategory,'_',prod$type, sep='')
prod$subcategory_brand = paste(prod$subcategory,'_',prod$brand,sep='')
prod$subcategory_manufacturer = paste(prod$subcategory,'_',prod$mod.manufacturer,sep='')
prod$subcategory_type_manufaturer = paste(prod$subcategory,'_',prod$type,prod$mod.manufacturer,sep='')

write.csv(prod,'data/mod_product.csv')

########################################################
## ========== ITEM FEATURE GLOBAL
########################################################
feat.item.global = data.frame(cbind('product_id'=as.character(prod$product_id),
                                    'manufacturer'=as.factor(prod$mod.manufacturer),
                                    'category' =as.factor(prod$mod.category),
                                    'lenstype'=as.factor(prod$type),
                                    'product_lifecycle' = as.factor(prod$mod.product_lifecycle),
                                    'brand' =  as.factor(prod$brand),
                                    'pack_qty' = as.factor(prod$mod.pack_qty)))

# featurize manufacturer
levels(feat.item.global$manufacturer) <-   levels(prod$mod.manufacturer)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~manufacturer-1, feat.item.global))))

# featurize category
levels(feat.item.global$category) <-   levels(prod$mod.category)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~category-1, feat.item.global))))

# featurize lens type
levels(feat.item.global$lenstype) <-   levels(prod$type)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~lenstype-1, feat.item.global))))

# featurize product_lifecycle
levels(feat.item.global$product_lifecycle) <-   levels(prod$mod.product_lifecycle)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~product_lifecycle-1, feat.item.global))))

# featurize brand
levels(feat.item.global$brand) <-   levels(prod$brand)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~brand-1, feat.item.global))))

# featurize brand
levels(feat.item.global$pack_qty) <-   levels(prod$mod.pack_qty)
feat.item.global = data.frame(cbind(feat.item.global, data.frame(model.matrix(~pack_qty-1, feat.item.global))))

feat.item.global$average_cost = as.numeric(as.character(prod$average_cost))
feat.item.global$average_cost[which(is.na(feat.item.global$average_cost))] <- 0

# create bucket of price list for average cost
breaks = c(seq(0,2.5,0.5),seq(5,25,5))
tmp <- data.frame(matrix(ncol = (length(breaks)-1), nrow = nrow(feat.item.global)))
for (i in 1:(length(breaks)-1)){
  tmp[,i] <- as.numeric(feat.item.global$average_cost[i] & feat.item.global$average_cost < breaks[i+1])
}
colnames(tmp) = paste0('avg_cost_', 1:(length(breaks)-1))
feat.item.global <- cbind(feat.item.global, tmp)

# create additional variable
feat.item.global$telesales <- prod$telesales_only
feat.item.global$promotional_product <- prod$promotional_product

# create additional product - category variable
feat.item.global$category_type <- prod$category_type 
feat.item.global$category_brand <- prod$category_brand 
feat.item.global$category_manufacturer <- prod$category_manufacturer 
feat.item.global$category_type_manufaturer <- prod$category_type_manufaturer 

# create additional product - sub category variable
feat.item.global$subcategory_type <- prod$subcategory_type 
feat.item.global$subcategory_brand <- prod$subcategory_brand 
feat.item.global$subcategory_manufacturer <- prod$subcategory_manufacturer 
feat.item.global$subcategory_type_manufaturer <- prod$subcategory_type_manufaturer 

feat.item.global <- noZV(feat.item.global)

# drop unecessary columns
# feat.item.global[,c(2:7)] <- NULL
write.table(feat.item.global, 'features/features.item.global.csv', row.names = F, col.names = T, sep=',')
