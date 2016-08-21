## A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}

getItemBasedSimMatrix <- function(data.purchase) {
  # Checker to see the data format
  stopifnot(colnames(data.purchase)[1]== 'customer_id')
  stopifnot(length(dim(data.purchase)) == 2)
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
  purchase.ibs <- data.purchase[,colnames(data.purchase) %w/o% c("customer_id")]
  # Create empty matrix for item similarity  
  purchase.ibs.similarity  <- matrix(NA, nrow=ncol(purchase.ibs), ncol=ncol(purchase.ibs),
                                          # naming rows 
                                          dimnames=list(colnames(purchase.ibs),
                                          # naming columns              
                                          colnames(purchase.ibs)))
  
  # Both loop parallelized:
  # Fill the above matrix with item-to-item similarity (cosine)
  purchase.ibs.similarity <- foreach(i=1:ncol(purchase.ibs), .combine='rbind') %:%
    foreach(j=1:ncol(purchase.ibs), .combine='c') %dopar% {
      getCosine(as.matrix(purchase.ibs[i]),as.matrix(purchase.ibs[j]))
    }
  # Formatting back to data frame type of format
  purchase.ibs.similarity <- as.data.frame(purchase.ibs.similarity)
  # Formatting back the rownames and the columnnames
  colnames(purchase.ibs.similarity) <- colnames(purchase.ibs)
  rownames(purchase.ibs.similarity) <- colnames(purchase.ibs)
  return(purchase.ibs.similarity)
}

getScore <- function(history, similarities)
  # Write a function to :
  # Get similarity score for user-item based
{ ## little modification to avoid NaN if no-similarities found in an item
  if (sum(similarities)==0) {
    result <- 0
  } if  (sum(similarities) > 0) {
    result <- sum(history*similarities)/sum(similarities)
  } return(result)
}

getUserItemBasedScore <- function(data.purchase , matrix_similarity) {
    # A placeholder matrix (use the original dataset)
    holder <- matrix(NA, nrow=nrow(data.purchase),ncol=ncol(data.purchase)-1,dimnames=list((data.purchase$customer_id),colnames(data.purchase[-1])))
    
    # Loop through the users (rows)
    for(i in 1:nrow(holder)) 
    { # Loops through the products (columns)
      for(j in 1:ncol(holder)) 
      { # Get the user's name and th product's name
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

getAffinityScore <- function(holder,test) {
  affinity_scores <- vector(mode="numeric", length=nrow(test))
  userlist <- rownames(holder)
  songlist <- colnames(holder)
  for (i in 1:nrow(test))
  { 
    #print(i)
    row <- which(userlist == test[i,1])
    col <- which(songlist == test[i,2])
    affinity_scores[i] <- holder[row,col]
  }
  return(affinity_scores)
}
