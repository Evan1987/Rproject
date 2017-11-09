priceSim<-function(target_issuer,target_time,priceList){
  ref = priceList[issuer==target_issuer,]%>%
    .[,mins:=abs(difftime(create_time-target_time,units = "mins"))]%>%
    setorder(.,mins)%>%head(.,1)
  return(ref$price)
}