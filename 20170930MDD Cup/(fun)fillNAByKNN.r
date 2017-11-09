library(DMwR)
# fill na values in uncompleteVec by k nearest neighbour in features
fillNAByKNN<-function(uncompleteVec,featureVec,k=2){
  nas = which(is.na(uncompleteVec))
  n = length(nas)
  if(n==0){return(uncompleteVec)}
  
  # 完整可用的数据
  xcompleteVec = uncompleteVec[-nas]
  xfeatureVec = featureVec[-nas]
  
  fillVec = rep(0,n)
  for(i in 1:n){
    index = nas[i]
    targetFeature = featureVec[index]
    fillVec[i] = {
      dist = abs(xfeatureVec-targetFeature)
      ks = order(dist)[1:k]
      xcompleteVec[ks]%>%centralValue(.,ws=exp(-dist[ks]))
    }
  }
  completeVec = copy(uncompleteVec)%T>%
  {
    .[nas] = fillVec
  }
  return(completeVec)
}