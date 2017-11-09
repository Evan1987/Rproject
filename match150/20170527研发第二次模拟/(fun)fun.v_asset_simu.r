fun.v_asset_simu<-function(amount,num,mean,sd=50000,unit=1e+5,min_amount=2e+5,tol){
  if(missing(mean)){
    mean = floor(amount/num/unit)*unit
  }
  if(missing(tol)){
    tol = 0.01*amount
  }
  x = rnorm(num,mean = mean,sd = sd)%>%sort(.,decreasing=T)
  x = pmax(x,min_amount)
  x = (x/unit)%>%floor(.)*unit
  
  while(abs(amount-sum(x))>tol){
    deltaN = ((amount-sum(x))/unit)%>%ceiling(.)
    s = sample(1:num,deltaN,replace = T)
    x[s] = x[s]+unit
    x= pmax(x,min_amount)
  }
  return(x)
}