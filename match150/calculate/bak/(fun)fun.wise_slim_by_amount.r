# 根据需要预留金额计算需要划定的区间和比例
fun.wise_slim_by_amount<-function(user_list_pre,
                        ref_ratio,
                        target_amount,
                        slim_contribution = c(3,3,4),
                        min_breaks = 100){
  user_list<-copy(user_list_pre)%>%
    .[,exp_match_ratio:=round(1-unmatched_current_premium/premium,2)]%>%
    .[,judge_value:=pmax(ref_ratio-exp_match_ratio,0)*unmatched_current_premium]
  
  total_amount = sum(user_list$unmatched_current_premium)
  slim_avg_ratio = ceiling(target_amount/total_amount*100)/100
  
  adjust_ratio = {
    if(slim_avg_ratio<=0.04){
      step = 1/(length(slim_contribution))*slim_avg_ratio
    }else{
      step = 0.02
    }
    (slim_avg_ratio-0:(length(slim_contribution)-1)*step)%>%c(.,0,1)%>%sort(.)
  }
  
  amount = {
    b = (total_amount-target_amount-sum(user_list[judge_value<min_breaks,]$unmatched_current_premium))%>%
      round(.,2)
    x = 1-(adjust_ratio[-1]%>%.[-length(.)])
    k = slim_contribution*b/sum(slim_contribution)
    k/x
  }
  
  seek_point<-function(start,amount,user_list){
    xx<-user_list[judge_value>=start,]%T>%
      setorder(.,judge_value,unmatched_current_premium)%>%
      .[,cum_amount:=cumsum(unmatched_current_premium)-amount]%>%
      .[cum_amount<=0,]
    max(xx$judge_value)
  }
  
  start = min_breaks
  result<-rep(min_breaks,length(amount))
  
  for(i in 1:length(amount)){
    y<-result[i]<-seek_point(start,amount[i],user_list)
    start<-y
  }
  result<-c(0,min_breaks,result,Inf)
  
  if(uniqueN(result)<length(result)){
    xx<-data.table(start=result[-length(result)],end = result[-1],adjust_ratio=adjust_ratio)%>%
      .[end>start,]
    result<-unique(c(xx$start,xx$end))%>%sort(.)
    adjust_ratio<-xx$adjust_ratio
  }
  return(list(breaks=result,adjust_ratio=adjust_ratio,data=user_list))
}