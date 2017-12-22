library(ROCR)
cls = c('P', 'P', 'N', 'P', 'P', 'P', 'N', 'N', 'P', 'N', 'P', 
        'N', 'P', 'N', 'N', 'N', 'P', 'N', 'P', 'N')
score = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.51, 0.49, 0.43, 
          0.42, 0.39, 0.33, 0.31, 0.23, 0.22, 0.19, 
          0.15, 0.12, 0.11, 0.04, 0.01)

pred = prediction(score, cls, label.ordering = c("N","P"))
roc = performance(pred,"tpr","fpr")


plot(roc,lwd=2,colorize=T)
auc = performance(pred,"auc")
unlist(auc@y.values)


getAUC <- function(cls,score,label_order){
  pred = prediction(score, cls, label.ordering = label_order)
  roc = performance(pred,"tpr","fpr")
  
  
  plot(roc,lwd=2,colorize=T)
  auc = performance(pred,"auc")
  return(unlist(auc@y.values))
}

score2 = 1 - score
getAUC(cls,score,c("N","P"))
getAUC(cls,score2,c("P","N"))
