
fun.wise_break<-function(user_list,
                         asset_data_total,
                         adjust_ratio,
                         min_breaks=100,
                         safe_amount = 1000000,
                         slim_contribution){
  if(length(slim_contribution)!=length(adjust_ratio)-2){
    stop(" the contribution length must be the same as length(adjust_ratio)-2")
  }
  
  slim_amount=sum(user_list$target_current_premium_pre)+
    sum(user_list$premium_regular)+
    sum(user_list$premium_tPlus)-sum(asset_data_total$amount)-safe_amount
  
  amount = {
       
    b = (slim_amount-sum(user_list[premium<min_breaks,]$target_current_premium_pre))%>%round(.,2)
    x = 1-(adjust_ratio[-1]%>%.[-length(.)])
    k = slim_contribution*b/sum(slim_contribution)
    k/x
  }
  
  premium_summary<-function(start,amount,user_list){
    xx<-user_list[premium>=start,c("userid","premium","target_current_premium_pre")]%T>%
      setorder(.,premium,target_current_premium_pre)%>%
      .[,cum_amount:=cumsum(target_current_premium_pre)-amount]%>%
      .[cum_amount<=0,]
    max(xx$premium)
  }
  
  start = min_breaks
  result<-rep(min_breaks,length(amount))
  
  for(i in 1:length(amount)){
    y<-result[i]<-premium_summary(start,amount[i],user_list)
    start<-y
  }
  result<-c(0,min_breaks,result,Inf)
  return(result)
}