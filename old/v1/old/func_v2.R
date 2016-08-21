## A function to find and remove zero-variance ("ZV") predictors
noZV <- function(x) {
  keepers <- unlist(lapply(x, function(x) length(unique(x)) > 1))
  x[,keepers,drop = FALSE]
}

# Create row names function to exlucde order_no, target
"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y


# source : https://davidrroberts.wordpress.com/2015/09/22/quick-auc-function-in-r-with-rocr-package/
# AUC function
fun.auc <- function(pred,obs){
  # Run the ROCR functions for AUC calculation
  ROC_perf <- performance(prediction(pred,obs),"tpr","fpr")
  ROC_sens <- performance(prediction(pred,obs),"sens","spec")
  ROC_err <- performance(prediction(pred, labels=obs),"err")
  ROC_auc <- performance(prediction(pred,obs),"auc")
  # AUC value
  AUC <- ROC_auc@y.values[[1]] # AUC
  # Mean sensitivity across all cutoffs
  x.Sens <- mean(as.data.frame(ROC_sens@y.values)[,1])
  # Mean specificity across all cutoffs
  x.Spec <- mean(as.data.frame(ROC_sens@x.values)[,1])
  # Sens-Spec table to estimate threshold cutoffs
  SS <- data.frame(SENS=as.data.frame(ROC_sens@y.values)[,1],SPEC=as.data.frame(ROC_sens@x.values)[,1])
  # Threshold cutoff with min difference between Sens and Spec
  SS_min_dif <- ROC_perf@alpha.values[[1]][which.min(abs(SS$SENS-SS$SPEC))]
  # Threshold cutoff with max sum of Sens and Spec
  SS_max_sum <- ROC_perf@alpha.values[[1]][which.max(rowSums(SS[c("SENS","SPEC")]))]
  # Min error rate
  Min_Err <- min(ROC_err@y.values[[1]])
  # Threshold cutoff resulting in min error rate
  Min_Err_Cut <- ROC_err@x.values[[1]][which(ROC_err@y.values[[1]]==Min_Err)][1]
  # Kick out the values
  round(cbind(AUC,x.Sens,x.Spec,SS_min_dif,SS_max_sum,Min_Err,Min_Err_Cut),3)
}

# AUC Plot
fun.aucplot <- function(pred, obs, title){
  # require ROCR pacakge
  # install.packages('ROCR')
  library('ROCR')
  # Run the AUC calculations
  ROC_perf <- performance(prediction(pred,obs),"tpr","fpr")
  ROC_sens <- performance(prediction(pred,obs),"sens","spec")
  ROC_auc <- performance(prediction(pred,obs),"auc")
  # Spawn a new plot window (Windows OS)
  graphics.off(); x11(h=6,w=6)
  # Plot the curve
  plot(ROC_perf,colorize=T,print.cutoffs.at=seq(0,1,by=0.1),lwd=3,las=1,main=title)
  # Add some statistics to the plot
  text(1,0.25,labels=paste("Npres = ",sum(obs==1),sep=""),adj=1)
  text(1,0.20,labels=paste("Nabs = ",sum(obs==0),sep=""),adj=1)
  text(1,0.15,labels=paste("AUC = ",round(ROC_auc@y.values[[1]],digits=2),sep=""),adj=1)
  text(1,0.10,labels=paste("Sens = ",round(mean(as.data.frame(ROC_sens@y.values)[,1]),digits=2),sep=""),adj=1)
  text(1,0.05,labels=paste("Spec = ",round(mean(as.data.frame(ROC_sens@x.values)[,1]),digits=2),sep=""),adj=1)
}


getCosine <- function(x,y)
  # Write a function to :
  # Calculate the cosine similarity between two vectors
{
  this.cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

getScore <- function(history, similarities)
  # Write a function to :
  # Get similarity score for user-item based
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

getAffinityMatrix <- function(data.purchase){
  # Write a function to :
  # Calculate similarity score matrix between user and items
  
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
  
  ###############################
  ### User-Based Recommendation
  
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
        topN<-((head(n=11,(data.purchase.ibs.similarity[order(data.purchase.ibs.similarity[,product],decreasing=TRUE),][product]))))
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
