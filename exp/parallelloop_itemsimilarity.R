library(doParallel)

# load data
# setwd("~/PycharmProjects/dissertation/raw_data/")
setwd("~/Dropbox/dissertation/raw_data/")

# Detect Cores that workable in CPU
detectCores()
getDoParWorkers()

## A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}

getCosine <- function(x,y)
  # Calculate the cosine similarity between two vectors
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}


# featurize
load('features/features.matrix.cust.vars/matrix_2014_item_product.Rdata')

getItemBasedSimilarity <- function(data) 
{
  # Checker to see the data format
  stopifnot(colnames(data)[1]== 'customer_id')
  stopifnot(length(dim(data)) == 2)
  
  # Drop any column named "customer_id"
  data.ibs <- (data[,!(names(data) %in% c("customer_id"))])
  
  # Create empty matrix for item similarity  
  data.ibs.similarity  <- matrix(NA, nrow=ncol(data.ibs), ncol=ncol(data.ibs),
                                          # naming rows 
                                          dimnames=list(colnames(data.ibs),
                                                        # naming columns              
                                                        colnames(data.ibs)))
  
  # Fill the above matrix with item-to-item similarity (cosine)
  # Loop through the columns
  for(i in 1:ncol(data.ibs)) {
    # Loop through the columns for each column
    for(j in 1:ncol(data.ibs)) {
      # Fill in placeholder with cosine similarities
      data.ibs.similarity[i,j] <- getCosine(as.matrix(data.ibs[i]),as.matrix(data.ibs[j]))
    }
  }
  
  # Formatting back to data frame type of format
  data.ibs.similarity <- as.data.frame(data.ibs.similarity)
  return(data.ibs.similarity)
}

getItemBasedSimMatrix <- function(data) {
  # Checker to see the data format
  stopifnot(colnames(data)[1]== 'customer_id')
  stopifnot(length(dim(data)) == 2)
  ## Create a function to exclude customer_id in column names
  "%w/o%" <- function(x, y) x[!x %in% y] 
  # Define Cosine Similarity function
  getCosine <- function(x,y)
    # Calculate the cosine similarity between two vectors
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this.cosine)
  }
  # Drop any column named "customer_id"
  data.ibs <- data[,colnames(data) %w/o% c("customer_id")]
  # Create empty matrix for item similarity  
  ibs.similarity  <- matrix(NA, nrow=ncol(data.ibs), ncol=ncol(data.ibs),
                                     # naming rows 
                                     dimnames=list(colnames(data.ibs),
                                                   # naming columns              
                                                   colnames(data.ibs)))
  
  # Both loop parallelized:
  # Fill the above matrix with item-to-item similarity (cosine)
  ibs.similarity <- foreach(i=1:ncol(data.ibs), .combine='rbind') %:%
    foreach(j=1:ncol(data.ibs), .combine='c') %dopar% {
      getCosine(as.matrix(data.ibs[i]),as.matrix(data.ibs[j]))
    }
  # Formatting back to data frame type of format
  ibs.similarity <- as.data.frame(ibs.similarity)
  # Formatting back the rownames and the columnnames
  colnames(ibs.similarity) <- colnames(data.ibs)
  rownames(ibs.similarity) <- colnames(data.ibs)
  return(ibs.similarity)
}

data <- noZV(purchase_prod_final[1:10000,])

#start time
strt<-Sys.time()
x <- getItemBasedSimilarity(data = data)
print(Sys.time()-strt)

#setup parallel backend to use 8 processors
cl<-makeCluster(8)
registerDoParallel(cl)
#start time
strt<-Sys.time()
y <- getItemBasedSimMatrix(data = data)
print(Sys.time()-strt)
stopCluster(cl)

max(abs(x - y))
