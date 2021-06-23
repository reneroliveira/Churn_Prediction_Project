#Auxiliar functions

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

accuracy <- function(ypred,ytrue){
  return(sum(ytrue==ypred)/length(ytrue))
}

precision <- function(ypred,ytrue){
  TP <- sum("&"(ypred==1,ytrue==1))/length(ytrue)
  FP <- sum("&"(ypred==1,ytrue==0))/length(ytrue)
  return(TP/(TP+FP))
}

recall <- function(ypred,ytrue){
  TP <- sum("&"(ypred==1,ytrue==1))/length(ytrue)
  FN <- sum("&"(ypred==0,ytrue==1))/length(ytrue)
  return(TP/(TP+FN))
}

lift <- function(probs,ytrue,quantile=0.1){
  beta_0 <- sum(ytrue==1)/length(ytrue)
  table <- cbind(probs,ytrue)
  sorted <- table[order(table[,1],decreasing = TRUE),]
  top <- sorted[1:round(quantile*length(probs)),]
  beta_q <- sum(top[,2]==1)/nrow(top)
  return(beta_q/beta_0)
}

plot_var <- function(var,data=train,name=var,nbins=50){
  #Function to plot density histogram of continuous variables
  p <- data %>% 
    mutate(Churn=as.factor(Churn)) %>% 
    ggplot( aes(x=data[,var], fill=Churn)) +
    geom_histogram(aes(y=..density..),color="grey", alpha=0.5, position = 'identity',bins=nbins) +
    # scale_fill_manual(values=c("blue", "red")) +
    labs(title=tools::toTitleCase(paste(name,"distribution")),
         x=var) + theme_minimal()
  return(p)
}

rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, "tpr" , "fpr")
  plot(perf, ...)
}

ROS <- function(data,y,target=1,seed=123){
  #Creates a new dataset by replicating the minority instance (target)
  set.seed(seed)
  minority <- data[data[,y]==target,]
  N <- nrow(data)
  n <- nrow(minority)
  new_mino <- N-2*n
  new_instances_index <- sample(1:nrow(minority), new_mino, replace=TRUE)
  new_instances <- minority[new_instances_index,]
  return(rbind(data,new_instances))
}

RUS <- function(data,y,target = 0,seed=123){
  #Ramdomly eliminates instances from the majority class(target)
  set.seed(seed)
  minority <- data[data[,y]!=target,]
  majority <- data[data[,y]==target,]
  n_elim <- nrow(majority)-nrow(minority)
  to_elim <- sample(1:nrow(majority),n_elim,replace=FALSE)
  majority <- majority[-to_elim,]
  return(rbind(minority,majority))
}

evaluate <- function(ypred,ytrue,scores,name=""){
  acc <- accuracy(ypred,ytrue)
  rec <- recall(ypred,ytrue)
  decile_lift <- lift(scores,ytrue)
  roc_curve <- roc.curve(scores.class0 = scores,weights.class0=ytrue,curve=TRUE)
  auc <- as.numeric(roc_curve['auc'])
  results <- matrix(c(acc,rec,decile_lift,auc),nrow=1)
  row.names(results) <- name
  colnames(results) <- c("Accuracy","Recall","Lift","AUC")
  return(results)
}

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
