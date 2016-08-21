## A function to find and remove zero-variance ("ZV") predictor
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
