library(data.table)
library(magrittr)
entCal<-function(charVec,totalN,n){
  n = length(charVec)
  if(missing(totalN)){
    totalN = n
  }
  s = tapply(rep(1,n),charVec,sum)/n
  -n/totalN*sum(s*log2(s))
}
gain<-function(charVec,addVec){
  n = length(charVec)
  ent0 = entCal(charVec)
  ent1 = tapply(charVec,addVec,FUN = entCal,totalN=n)%>%sum(.)
  ent0-ent1
}


iris = data.table(iris)
splitV = unique(iris$Sepal.Length)%>%sort(.)
size = length(splitV)
gainsV = rep(0,size)

for(i in 1:size){
  splitVal = splitV[i]
  iris[,new:=0]
  iris[Sepal.Length>splitVal,new:=1]
  gainsV[i] = gain(iris$Species,iris$new)
}
finalSplitV = splitV[which.max(gainsV)]#5.5
