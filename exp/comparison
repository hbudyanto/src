
getItemBasedSimilarity.loop <- function(data.purchase) 
  {
  # Checker to see the data format
  stopifnot(colnames(data.purchase)[1]== 'customer_id')
  stopifnot(length(dim(data.purchase)) == 2)
  
  # Drop any column named "customer_id"
  data.purchase.ibs <- (data.purchase[,!(names(data.purchase) %in% c("customer_id"))])
  
  # Create empty matrix for item similarity  
  data.purchase.ibs.similarity  <- matrix(NA, nrow=ncol(data.purchase.ibs), ncol=ncol(data.purchase.ibs),
                                          # naming rows 
                                          dimnames=list(colnames(data.purchase.ibs),
                                                        # naming columns              
                                                        colnames(data.purchase.ibs)))
  
  # Fill the above matrix with item-to-item similarity (cosine)
  # Loop through the columns
  for(i in 1:ncol(data.purchase.ibs)) {
    # Loop through the columns for each column
    for(j in 1:ncol(data.purchase.ibs)) {
      # Fill in placeholder with cosine similarities
      data.purchase.ibs.similarity[i,j] <- getCosine(as.matrix(data.purchase.ibs[i]),as.matrix(data.purchase.ibs[j]))
    }
  }
  
  # Formatting back to data frame type of format
  data.purchase.ibs.similarity <- as.data.frame(data.purchase.ibs.similarity)
}

getItemBasedSimilarity.bothParallel <- function(data.purchase) {
  # Checker to see the data format
  stopifnot(colnames(data.purchase)[1]== 'customer_id')
  stopifnot(length(dim(data.purchase)) == 2)
  
  getCosine <- function(x,y)
    # Write a function to :
    # Calculate the cosine similarity between two vectors
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this.cosine)
  }
  
  # Drop any column named "customer_id"
  data.purchase.ibs <- (data.purchase[,!(names(data.purchase) %in% c("customer_id"))])
  
  # Create empty matrix for item similarity  
  data.purchase.ibs.similarity  <- matrix(NA, nrow=ncol(data.purchase.ibs), ncol=ncol(data.purchase.ibs),
                                          # naming rows 
                                          dimnames=list(colnames(data.purchase.ibs),
                                                        # naming columns              
                                                        colnames(data.purchase.ibs)))
  
  # Fill the above matrix with item-to-item similarity (cosine)
  # Loop through the columns
  data.purchase.ibs.similarity <- foreach(i=1:ncol(data.purchase.ibs), .combine='rbind') %:%
    foreach(j=1:ncol(data.purchase.ibs), .combine='c') %dopar% {
      getCosine(as.matrix(data.purchase.ibs[i]),as.matrix(data.purchase.ibs[j]))
    }
  
  # Formatting back to data frame type of format
  data.purchase.ibs.similarity <- as.data.frame(data.purchase.ibs.similarity)
}

getItemBasedSimilarity.outerParallel <- function(data.purchase) {
  # Checker to see the data format
  stopifnot(colnames(data.purchase)[1]== 'customer_id')
  stopifnot(length(dim(data.purchase)) == 2)
  
  # Define Cosine Similarity
  getCosine <- function(x,y)
    # Write a function to :
    # Calculate the cosine similarity between two vectors
  {
    this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
    return(this.cosine)
  }
  
  # Drop any column named "customer_id"
  data.purchase.ibs <- (data.purchase[,!(names(data.purchase) %in% c("customer_id"))])
  
  # Create empty matrix for item similarity  
  data.purchase.ibs.similarity  <- matrix(NA, nrow=ncol(data.purchase.ibs), ncol=ncol(data.purchase.ibs),
                                          # naming rows 
                                          dimnames=list(colnames(data.purchase.ibs),
                                                        # naming columns              
                                                        colnames(data.purchase.ibs)))
  
  # Fill the above matrix with item-to-item similarity (cosine)
  # Loop through the columns
  # Outer loop parallelized:
    data.purchase.ibs.similarity <- foreach(i=1:ncol(data.purchase.ibs), .combine='rbind') %dopar%
        for (j in 1:ncol(data.purchase.ibs)) 
       {
          getCosine(as.matrix(data.purchase.ibs[i]),as.matrix(data.purchase.ibs[j]))
       }
  
  # Formatting back to data frame type of format
  data.purchase.ibs.similarity <- as.data.frame(data.purchase.ibs.similarity)
}


noZV(purchase_prod_final[1:100,])

system.time(getItemBasedSimilarity(data.purchase = noZV(purchase_prod_final[1:1000,])))

cl <- makeCluster(4)  
registerDoParallel(cl)
system.time(getItemBasedSimilarity.BothParallel(data.purchase = noZV(purchase_prod_final[1:1000,])))
stopCluster(cl)

cl <- makeCluster(4)  
registerDoParallel(cl)
system.time(getItemBasedSimilarity.outerParallel(data.purchase = noZV(purchase_prod_final[1:1000,])))
stopCluster(cl)

cl <- makeCluster(4)  
registerDoParallel(cl)
x <- getItemBasedSimilarity.outerParallel(data.purchase = noZV(purchase_prod_final[1:1000,]))
stopCluster(cl)


# Get scoring based on item similarity and transaction history
getScore <- function(history, similarities)
{
  ## little modification to avoid NaN if no-similarities found in an item
  if (sum(similarities)==0) {
    result <- 0
  }
  if  (sum(similarities) > 0) {
    result <- sum(history*similarities)/sum(similarities)
  }
  return(result)
}

getUserItemBasedScore.Loop <- function(data.purchase , matrix_similarity )
{
  
  # A placeholder matrix (use the original dataset)
  holder <- matrix(NA, nrow=nrow(data.purchase),ncol=ncol(data.purchase)-1,dimnames=list((data.purchase$customer_id),colnames(data.purchase[-1])))
  
  # Loop through the users (rows)
  for(i in 1:nrow(holder)) 
  {
    # Loops through the products (columns)
    for(j in 1:ncol(holder)) 
    {
      # Get the user's name and th product's name
      # We do this not to conform with vectors sorted differently 
      user <- rownames(holder)[i]
      product <- colnames(holder)[j]
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      topN<-((head(n=11,(matrix_similarity[order(matrix_similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- data.purchase[,c("customer_id",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$customer_id==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("customer_id"))])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # end product for loop   
  } # end user for loop
  return(holder)
}

getUserItemBasedScore.outerParallel <- function(data.purchase , matrix_similarity )
  {
      # Get scoring based on item similarity and transaction history
      getScore <- function(history, similarities)
      {
        ## little modification to avoid NaN if no-similarities found in an item
        if (sum(similarities)==0) {
          result <- 0
        }
        if  (sum(similarities) > 0) {
          result <- sum(history*similarities)/sum(similarities)
        }
        return(result)
      }
  
    # A placeholder matrix (use the original dataset)
    holder <- matrix(NA, nrow=nrow(data.purchase),ncol=ncol(data.purchase)-1,dimnames=list((data.purchase$customer_id),colnames(data.purchase[-1])))
    
    # Loop through the users (rows)
    holder <- foreach(i=1:nrow(holder), .combine='rbind') %dopar%
      # Loops through the products (columns)
      for(j in 1:ncol(holder)) 
      {
        # Get the user's name and th product's name
        # We do this not to conform with vectors sorted differently 
        user <- rownames(holder)[i]
        product <- colnames(holder)[j]
        
        # We first have to get a product's top 10 neighbours sorted by similarity
        topN<-((head(n=11,(matrix_similarity[order(matrix_similarity[,product],decreasing=TRUE),][product]))))
        topN.names <- as.character(rownames(topN))
        topN.similarities <- as.numeric(topN[,1])
        
        # We then get the user's purchase history for those 10 items
        topN.purchases<- data.purchase[,c("customer_id",topN.names)]
        topN.userPurchases<-topN.purchases[topN.purchases$customer_id==user,]
        topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("customer_id"))])
        
        # We then calculate the score for that product and that user
        getScore(similarities=topN.similarities,history=topN.userPurchases)
        
      } # end product for loop   
    return(holder)
}

cl <- makeCluster(4)  
registerDoParallel(cl)
#start time
strt<-Sys.time()

data.purchase.ibs.similarity <- getItemBasedSimilarity.OuterParallel(data.purchase = noZV(purchase_prod_final[1:1000,]))


print(Sys.time()-strt)
stopCluster(cl)


holder <- getUserItemBasedScore.Loop(data.purchase = noZV(purchase_prod_final[1:1000,]),
                                     matrix_similarity = data.purchase.ibs.similarity)


