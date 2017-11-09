
#计算一个数据向量减去一定额度后的变化
fun.cumminus<-function(minused_vector,minus_amount){
  minused_vector<-as.double(minused_vector)
  
  rest_amount<-max(0,minus_amount-sum(minused_vector))
  n<-length(minused_vector)
  if(rest_amount>0){
    result_vector<-rep(0,n)
  }
  else{
    cum_result_vector<-cumsum(minused_vector)-minus_amount
    index=min(which(cum_result_vector>=0))
    result_vector<-{
        minused_vector[1:index-1]<-0
        minused_vector[index]<-cum_result_vector[index]
        minused_vector
    }
  }
  return(result_vector)
}