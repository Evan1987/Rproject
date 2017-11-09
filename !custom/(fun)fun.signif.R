
# shorten number x to having n effective numbers
fun.signif2<-function(x,n){
  if(x>=1)
  {
    return(round(x,n))
  }else
  {
    return(signif(x,n))
  }
}