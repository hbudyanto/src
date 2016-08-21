library(doParallel)

# Detect Cores that workable in CPU
detectCores()
getDoParWorkers()

getDoParRegistered()
getDoParWorkers()

# load data
setwd("~/PycharmProjects/dissertation/raw_data/")
# setwd("~/Dropbox/dissertation/raw_data/")

# featurize
load('features/features.matrix.cust.vars/matrix_2014_item_product.Rdata')
load('features/features.matrix.cust.vars/features.similarity/item.matrix.Rdata')

## A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}

getScore <- function(history, similarities)
  # Get similarity score for user-item based
  ## little modification to avoid NaN if no-similarities found in an item
  { 
  if (sum(similarities)==0) {
    result <- 0
  } 
  if  (sum(similarities) > 0) {
    result <- sum(history*similarities)/sum(similarities)
  } 
  return(result)
}

matrix_similarity <- item.matrix
data <- purchase_prod_final
i <- 1
j <- 1

getUserItemBasedScoreLoop <- function(data , matrix_similarity) {
  # A placeholder matrix (use the original dataset)
  holder <- matrix(NA, nrow=nrow(data),ncol=ncol(data)-1,dimnames=list((data$customer_id),colnames(data[-1])))
    # Loop through the users (rows)
  for(i in 1:nrow(holder)) 
  { # Loops through the products (columns)
    for(j in 1:ncol(holder)) 
    { # Get the user's name and th product's name
      # We do this not to conform with vectors sorted differently 
      user <- rownames(holder)[i]
      product <- colnames(holder)[j]
      # i<- 1; j<-3
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(matrix_similarity[order(matrix_similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # We then get the user's purchase history for those 10 items
      topN.userPurchases <- as.numeric(data[which(data$customer_id == user), topN.names])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # end product for loop   
  } # end user for loop
  return(holder)
}


getUserItemBasedScore <- function (data, matrix_similarity)
{
  registerDoParallel(cores = 8)
  ## Parellization of Loops 
  results <- foreach(i=1:nrow(data), .combine='rbind') %dopar% {
    # The value of the inner foreach loop is returned as
    # the value of the body of the outer foreach loop
    foreach(j=1:(ncol(data)-1), .combine='c') %do% {
      user <- rownames(data)[i]
      product <- colnames(data)[j]
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(matrix_similarity[order(matrix_similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # We then get the user's purchase history for those 10 items
      topN.userPurchases <-  as.numeric(data[which(data$customer_id == user), topN.names])
      
      # We then calculate the score for that product and that user
      sum(topN.similarities*topN.userPurchases) / sum(topN.similarities)
    }     
  }
  colnames(results) <- colnames(data)[-1]
  rownames(results) <- rownames(data)
}


#start time
strt<-Sys.time()
x <- getUserItemBasedScoreLoop(data = purchase_prod_final[1:5000,], matrix_similarity = item.matrix)
print(Sys.time()-strt)

#start time
strt<-Sys.time()
y <- getUserItemBasedScoreLoop(data = purchase_prod_final[1:5000,], matrix_similarity = item.matrix)
print(Sys.time()-strt)

max(abs(x - y))
