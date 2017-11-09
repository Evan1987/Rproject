

# 根据已有的L型图像，找出肘点，并选择是否标注
fun.elbow_point<-function(x,y,doplot=F){
  library(magrittr)
  
  distance_function<-function(x,y,formula){
    temp<-c(x,y,1)
    distance=crossprod(temp,formula)%>%drop(.)%>%abs(.)/(crossprod(formula[-3])%>%drop(.)%>%sqrt(.))
    return(distance)
  }
  formula<-{
    A=(y[which(x==max(x))]-y[which(x==min(x))])/(max(x)-min(x))#slope
    B=-1
    C=y[which(x==max(x))]-A*max(x)
    c(A,B,C)
  }
  
  distance<-mapply(FUN=distance_function,x,y,MoreArgs = list(formula=formula))
  index=which(distance==max(distance))
  elbow_point<-c(x[index],y[index])
  
  if(doplot){
    plot(x,y)
    lines(x=c(min(x),max(x)),y=c(y[which(x==min(x))],y[which(x==max(x))]))
    points(x=elbow_point[1],y=elbow_point[2],col="red",pch=18)
    text(x=elbow_point[1],y=elbow_point[2],labels = paste0("(",elbow_point[1],",",elbow_point[2],")"),pos = 3,adj=2)
  }
  return(elbow_point)
}
