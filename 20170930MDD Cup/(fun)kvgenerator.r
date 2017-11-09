kfoldGenerator<-function(df,k){
  ids = 1:nrow(df)
  foldNum = round(nrow(df)/k)
  i = 1
  idList = list()
  
  while(i<k){
    trainList =sample(ids,foldNum)
    idList = c(idList,list(trainList))
    ids = ids[!ids%in%trainList]
    i = i+1
  }
  idList = c(idList,list(ids))
  return(idList)
}