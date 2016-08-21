## A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}

## A function to compute item-based similarity matrix
## data -> row : users , colmn : variables - dummy (0,1)
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

## A function to produce user - based similarity matrix
## data -> row : users , colmn : variables - dummy (0,1)
## matrix similarity : matrix produced by getItemBasedSimMatrix
getUserItemBasedScore <- function (data, matrix_similarity)
{
  registerDoParallel(cores = 4)
  ## Parellization of Loops 
  results <- foreach(i=1:nrow(data), .combine='rbind') %dopar% {
    # The value of the inner foreach loop is returned as
    # the value of the body of the outer foreach loop
    foreach(j=1:(ncol(data)-1), .combine='c') %do% {
      user <- rownames(data)[i]
      product <- colnames(data)[j]
      
      topN<-((head(n=11,(matrix_similarity[order(matrix_similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # We then get the user's purchase history for those 10 items
      topN.purchases<- data[,c("customer_id",topN.names)]
      topN.userPurchases<-topN.purchases[topN.purchases$customer_id==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("customer_id"))])
      
      # We then calculate the score for that product and that user
      sum(topN.similarities*topN.userPurchases) / sum(topN.similarities)
    }     
  }
  colnames(results) <- colnames(data)[-1]
  rownames(results) <- rownames(data)
}