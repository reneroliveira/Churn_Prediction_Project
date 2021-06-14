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

