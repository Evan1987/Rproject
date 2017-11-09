maeEval<-function(sub,testADF){
  result = copy(testADF)%>%
    .[sub,on="order_id",pred:=i.delivery_duration]%>%
    .[,diff:=abs(delivery_duration-pred)]
  mae = mean(result$diff)
  print(paste("mae: ", mae))
  return(list("mae" = mae,"result" = result))
}