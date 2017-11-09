


# 这是根据时间流模拟，在原match_temp基础上做的更新
# 单资产匹配函数，针对单一资产，结合三个资金序列得出相应匹配结果
# unmatched_amount：资产的剩余未匹配金额
# avail_num：资产的剩余可匹配人数
# regular_sep,tPlus_seq,current_seq：三个当前资金序列
# r_regular：定期序列的最小人数匹配比例
# r_tPlus：匹完定期后，T+N序列的最小人数匹配比例
match_temp2<-function(unmatched_amount,
                     avail_num,
                     regular_seq,
                     tPlus_seq,
                     current_seq,
                     r_regular,
                     r_tPlus){
  # 【FUN】建立匹配输出函数，对每一个资产的每次匹配输出匹配结果
  match_result_details<-function(user_seq,unmatched_amount,item="regular"){
    
    user_seq2<-copy(user_seq)%>%
      mutate(.,cum_sum_item=cumsum(unmatched_premium))%>%
      mutate(.,rest=cum_sum_item-unmatched_amount)%>%
      mutate(.,amount=unmatched_premium-ifelse(rest<0,0,rest))%>%
      rownames_to_column(.,"id")%>%
      mutate(.,id=as.integer(id))%>%
      filter(.,amount>0)
    
    temp<-data.table(id=user_seq2$id,
                     userid=user_seq2$userid,
                     log_time=user_seq2$log_time,
                     type=item,
                     amount=user_seq2$amount)%>%tbl_df(.)
    return(temp)
  }
  
  regular_half=0
  tPlus_half=0
  temp_regular<-data.table()%>%tbl_df(.)
  temp_tPlus<-data.table()%>%tbl_df(.)
  temp_current<-data.table()%>%tbl_df(.)
  
  # 匹配定期
  # 如果只靠定期可以满足
  if(sum(regular_seq$unmatched_premium[1:avail_num],na.rm = T)>=unmatched_amount)
  {
    temp_regular<-match_result_details(user_seq = regular_seq,
                                       unmatched_amount = unmatched_amount,
                                       item = "regular")
  }
  else{#否则，只取定期序列的前floor(r_regular*avail_num)行
    temp_regular<-head(regular_seq,floor(r_regular*avail_num))%>%
      rownames_to_column(.,"id")%>%
      mutate(.,type=2,amount=unmatched_premium,id=as.integer(id))%>%
      select(.,-remark,-unmatched_premium,-weight)
    regular_half=1
  }
  # 需要继续匹配 T+N
  # 如果靠T+N可以满足
  if(regular_half){
    unmatched_amount=unmatched_amount-sum(temp_regular$amount)
    avail_num2=avail_num-nrow(temp_regular)
    
    if(sum(tPlus_seq$unmatched_premium[1:avail_num2],na.rm = T)>=unmatched_amount)
    {
      temp_tPlus<-match_result_details(user_seq = tPlus_seq,
                                       unmatched_amount = unmatched_amount,
                                       item = "tPlus")
    }
    else{#否则，只取T+N序列的前floor(r_tPlus*avail_num2)行
      temp_tPlus<-head(tPlus_seq,floor(r_tPlus*avail_num2))%>%
        rownames_to_column(.,"id")%>%
        mutate(.,type=1,amount=unmatched_premium,id=as.integer(id))%>%
        select(.,-remark,-unmatched_premium,-weight)
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
    rownames_to_column(.,"id")%>%
    mutate(.,id=as.integer(id))
  return(temp)
}