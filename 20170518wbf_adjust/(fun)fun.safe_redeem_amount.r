fun.safe_redeem_amount<-function(premium_change){
  
  n = length(premium_change)
  
  result<-rep(0,n)
  
  s0=0
  for(i in 1:n){
    result[i]<- -s0
    s0 = (s0 + premium_change[i])%>%pmin(.,0)
  }
  
  return(result)
}