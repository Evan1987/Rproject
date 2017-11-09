#  根据待匹资产计算需要划定的区间和比例
fun.wise_slim_by_asset<-function(user_list_pre,
                        ref_ratio,
                        remainTotalNum,
                        target_asset = list(amount=4e+6,num=200),
                        min_breaks = 0,
                        slim_contribution = c(3,3,4)){
  source('~/rstudio/match150/calculate/(fun)fun.wise_slim_by_amount.r', encoding = 'UTF-8',echo=TRUE)
  ################# 计算可匹能力 #################
  fun.judge_match_ability<-function(total_avail_num,avail_num,user_list,breaks,adjust_ratio){
    
    user_list<-copy(user_list)%>%
      .[,adjust_ratio:=cut(judge_value,
                           breaks=breaks,
                           labels=adjust_ratio,
                           right = F)%>%as.character(.)%>%as.numeric(.)]%>%
      .[,unmatched_current_premium:=round(unmatched_current_premium*adjust_ratio,2)]
    
    third_seq<-user_list[unmatched_current_premium>0,]$unmatched_current_premium%>%sort(.,decreasing=T)
    second_seq<-user_list[unmatched_tPlus_premium>0,]$unmatched_tPlus_premium%>%sort(.,decreasing=T)
    first_seq<-user_list[unmatched_regular_premium>0,]$unmatched_regular_premium%>%sort(.,decreasing=T)
    
    a = round(length(first_seq)/total_avail_num,4)%>%pmin(.,1)
    b = round(length(second_seq)/(total_avail_num-length(first_seq)),4)%>%pmin(.,1)
    
    m_a<-min(floor(a*avail_num),length(first_seq))
    m_b<-min(floor((avail_num-m_a)*b),length(second_seq))
    m_c<-min(avail_num-m_a-m_b,length(third_seq))
    
    judge_const<-
      (
        head(first_seq,m_a)%>%
          sum(.)
      )+
      (
        head(second_seq,m_b)%>%
          sum(.)
      )+
      (
        head(third_seq,m_c)%>%
          sum(.)
      )
    return(list(judge_const=judge_const,remain_amount = sum(user_list$unmatched_current_premium)))
  }

  amount = target_asset$amount
  avail_num = target_asset$num
  total_avail_num = remainTotalNum+avail_num
  
  user_list<-copy(user_list_pre)%>%
    .[,exp_match_ratio:=round(1-unmatched_current_premium/premium,2)]%>%
    .[,judge_value:=pmax(ref_ratio-exp_match_ratio,0)*unmatched_current_premium]
  
  # 最差条件
  breaks = c(0,Inf)
  adjust_ratio = 1
  target_amount = sum(user_list$unmatched_current_premium)
  now_ability = fun.judge_match_ability(total_avail_num = total_avail_num,
                                        avail_num = avail_num,
                                        user_list = user_list,
                                        breaks = breaks,
                                        adjust_ratio = adjust_ratio)
  
  # 二分法计算区间和比例
  if(now_ability$judge_const>=amount){
    final_result<-list(breaks=breaks,adjust_ratio=adjust_ratio,data=user_list)
    up= target_amount
    down = amount
    step = abs(1/2*(up+down)-target_amount)
    target_amount = 1/2*(up+down)
    least_remain_cash = now_ability$remain_amount
    stepnum=0
    while(stepnum<=4 & step>=1e+7){
      result<-fun.wise_slim(user_list_pre = user_list_pre,
                            ref_ratio = ref_ratio,
                            target_amount = target_amount,
                            min_breaks = min_breaks,
                            slim_contribution = slim_contribution
                            )
      
      
      now_ability = fun.judge_match_ability(total_avail_num = total_avail_num,
                                            avail_num = avail_num,
                                            user_list = result$data,
                                            breaks = result$breaks,
                                            adjust_ratio = result$adjust_ratio)
      if(now_ability$judge_const>=amount){
        up = target_amount
        step = abs(1/2*(up+down)-target_amount)
        target_amount = 1/2*(down+up)
        final_result = result
        least_remain_cash = now_ability$remain_amount
      }else{
        down = target_amount
        step = abs(1/2*(up+down)-target_amount)
        target_amount = 1/2*(up+down)
      }
      stepnum = stepnum + 1
    }
    print(paste("stepnum :",stepnum,"!"))
    print(paste("final remain target_amount:",least_remain_cash,"!"))
    return(final_result)
  }else{
    print("there is no solution for such asset!")
    return(list(breaks=c(),adjust_ratio=c(),data=data.table()))
  }
}  
  
  
  
  
  