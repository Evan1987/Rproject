
# 单资产匹配函数，针对单一资产，结合三个资金序列得出相应匹配结果
# unmatched_amount：资产的剩余未匹配金额
# avail_num：资产的剩余可匹配人数
# regular_sep,tPlus_seq,current_seq：三个当前资金序列
# r_regular：定期序列的最小人数匹配比例
# r_tPlus：匹完定期后，T+N序列的最小人数匹配比例
match_temp5<-function(unmatched_amount,
                     avail_num,
                     first_seq,
                     second_seq,
                     third_seq,
                     a,
                     b,
                     item=c("regular","tPlus","current")){
  # 【FUN】建立匹配输出函数，对每一个资产的每次匹配输出匹配结果
  match_result_details<-function(user_seq,unmatched_amount){
    
    user_seq2<-copy(user_seq)%>%
      .[,cum_sum_item:=cumsum(unmatched_premium)]%>%
      .[,rest:=cum_sum_item-unmatched_amount]%>%
      .[,amount:=round(unmatched_premium-ifelse(rest<0,0,rest),2)]%>%
      .[amount>0,]
    
    temp<-user_seq2[,c("userid","type","amount")]%>%
      .[,id:=.I]
    return(temp)
  }
  
  m_a<-min(floor(a*wait_asset$avail_num),nrow(first_seq))
  m_b<-min(floor((wait_asset$avail_num-m_a)*b),nrow(second_seq))
  m_c<-min(wait_asset$avail_num-m_a-m_b,nrow(third_seq))
  
  
  user_seq<-rbindlist(list(head(first_seq,m_a)%>%.[,type:=item[1]],
                           head(second_seq,m_b)%>%.[,type:=item[2]],
                           head(third_seq,m_c)%>%.[,type:=item[3]]),
                      use.names = T,fill = T)
  
  result<-match_result_details(user_seq = user_seq,unmatched_amount = unmatched_amount)
  return(result)
}