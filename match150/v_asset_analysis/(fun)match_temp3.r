
# 单资产匹配函数，针对单一资产，结合三个资金序列得出相应匹配结果
match_temp3<-function(unmatched_amount,
                     avail_num,
                     A_seq,
                     B_seq,
                     C_seq,
                     r_A,
                     r_B){
  # 【FUN】建立匹配输出函数，对每一个资产的每次匹配输出匹配结果
  match_result_details<-function(user_seq,unmatched_amount,item="A"){
    
    user_seq2<-copy(user_seq)%>%
      .[,cum_sum_item:=cumsum(unmatched_premium)]%>%
      .[,rest:=cum_sum_item-unmatched_amount]%>%
      .[,amount:=unmatched_premium-ifelse(rest<0,0,rest)]%>%
      .[,id:=seq(1 ,nrow(.),by=1)]%>%
      .[amount>0,]
    
    temp<-data.table(id=user_seq2$id,
                     userid=user_seq2$userid,
                     type=item,
                     amount=user_seq2$amount)
    return(temp)
  }
  
  A_half=0
  B_half=0
  temp_A<-data.table()
  temp_B<-data.table()
  temp_C<-data.table()
  
  # 匹配A队列
  if(sum(A_seq$unmatched_premium[1:avail_num],na.rm = T)>=unmatched_amount)
  {
    temp_A<-match_result_details(user_seq = A_seq,
                                 unmatched_amount = unmatched_amount,
                                 item = "A")
  }
  else{
    temp_A<-head(A_seq,floor(r_A*avail_num))%>%
      .[,":="(type="A",
              amount=unmatched_premium,
              id=seq(1,nrow(.)),
              unmatched_premium=NULL)]
    A_half=1
  }
  # 需要继续匹配 B队列
  if(A_half){
    unmatched_amount=unmatched_amount-sum(temp_A$amount)
    avail_num2=avail_num-nrow(temp_A)
    
    if(sum(B_seq$unmatched_premium[1:avail_num2],na.rm = T)>=unmatched_amount)
    {
      temp_B<-match_result_details(user_seq = B_seq,
                                   unmatched_amount = unmatched_amount,
                                   item = "B")
    }
    else{
      temp_B<-head(B_seq,floor(r_B*avail_num2))%>%
        .[,":="(type="B",
                amount=unmatched_premium,
                id=seq(1,nrow(.)),
                unmatched_premium=NULL)]
      tPlus_half=1
    }
  }
  # 需要继续匹配C队列
  if(B_half){
    unmatched_amount=unmatched_amount-sum(temp_B$amount)
    avail_num2=avail_num-uniqueN(c(temp_A$userid,temp_B$userid))
    temp_C<-match_result_details(user_seq = C_seq,
                                       unmatched_amount = unmatched_amount,
                                       item = "C")
  }
  
  temp<-rbind(temp_A,temp_B,temp_C)%>%
    .[,id:=seq(1,nrow(.))]
  return(temp)
}