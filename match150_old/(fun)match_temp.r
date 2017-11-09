
# 单资产匹配函数，针对单一资产，结合三个资金序列得出相应匹配结果
# unmatched_amount：资产的剩余未匹配金额
# avail_num：资产的剩余可匹配人数
# regular_sep,tPlus_seq,current_seq：三个当前资金序列
# r_regular：定期序列的最小人数匹配比例
# r_tPlus：匹完定期后，T+N序列的最小人数匹配比例
match_temp<-function(unmatched_amount,
                     avail_num,
                     regular_seq,
                     tPlus_seq,
                     current_seq,
                     r_regular,
                     r_tPlus){
  # 【FUN】建立匹配输出函数，对每一个资产的每次匹配输出匹配结果
  match_result_details<-function(user_seq,unmatched_amount,item="regular"){
    
    colname<-switch(item,
                    "tPlus" = "unmatched_tPlus_premium",
                    "regular" = "unmatched_regular_premium",
                    "current" = "unmatched_current_premium")
    
    user_seq2<-copy(user_seq)%>%
      .[,cum_sum_item:=eval(parse(text=paste0("cumsum(",colname,")")))]%>%
      .[,rest:=cum_sum_item-unmatched_amount]%>%
      .[,amount:=eval(parse(text=paste0(colname)))-ifelse(rest<0,0,rest)]%>%
      .[,id:=seq(1 ,nrow(.),by=1)]%>%
      .[amount>0,]
    
    temp<-data.table(id=user_seq2$id,
                     userid=user_seq2$userid,
                     type=item,
                     amount=user_seq2$amount)
    return(temp)
  }
  
  regular_half=0
  tPlus_half=0
  temp_regular<-data.table()
  temp_tPlus<-data.table()
  temp_current<-data.table()
  
  # 匹配定期
  if(sum(regular_seq$unmatched_regular_premium[1:avail_num],na.rm = T)>=unmatched_amount)
  {
    temp_regular<-match_result_details(user_seq = regular_seq,
                                       unmatched_amount = unmatched_amount,
                                       item = "regular")
  }
  else{
    temp_regular<-head(regular_seq,floor(r_regular*avail_num))%>%
      .[,":="(type="regular",
              amount=unmatched_regular_premium,
              id=seq(1,nrow(.)),
              unmatched_regular_premium=NULL)]
    regular_half=1
  }
  # 需要继续匹配 T+N
  if(regular_half){
    unmatched_amount=unmatched_amount-sum(temp_regular$amount)
    avail_num2=avail_num-nrow(temp_regular)
    
    if(sum(tPlus_seq$unmatched_tPlus_premium[1:avail_num2],na.rm = T)>=unmatched_amount)
    {
      temp_tPlus<-match_result_details(user_seq = tPlus_seq,
                                       unmatched_amount = unmatched_amount,
                                       item = "tPlus")
    }
    else{
      temp_tPlus<-head(tPlus_seq,floor(r_tPlus*avail_num2))%>%
        .[,":="(type="tPlus",
                amount=unmatched_tPlus_premium,
                id=seq(1,nrow(.)),
                unmatched_tPlus_premium=NULL)]
      tPlus_half=1
    }
  }
  # 需要继续匹配活期
  if(tPlus_half){
    unmatched_amount=unmatched_amount-sum(temp_tPlus$amount)
    avail_num2=avail_num-uniqueN(c(temp_regular$userid,temp_tPlus$userid))
    temp_current<-match_result_details(user_seq = current_seq,
                                       unmatched_amount = unmatched_amount,
                                       item = "current")
  }
  
  temp<-rbind(temp_regular,temp_tPlus,temp_current)%>%
    .[,id:=seq(1,nrow(.))]
  return(temp)
}