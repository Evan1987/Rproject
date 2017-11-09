fun.wise_slim_by_asset<-function(user_list,
                                 remainTotalNum,
                                 first_seq,
                                 second_seq,
                                 first_seq_length,
                                 second_seq_length,
                                 target_asset = list(amount=4e+6,num=200)){
  
  fun.judge_match_ability<-function(total_avail_num,
                                    avail_num,
                                    first_seq,
                                    second_seq,
                                    third_seq,
                                    first_seq_length,
                                    second_seq_length){
    third_seq<-sort(third_seq,decreasing=T)
    second_seq<-sort(second_seq,decreasing=T)
    first_seq<-sort(first_seq,decreasing=T)
    
    a = round(first_seq_length/total_avail_num,4)%>%pmin(.,1)
    b = round(second_seq_length/(pmax(total_avail_num-first_seq_length,0)+1),4)%>%pmin(.,1)
    
    m_a<-min(floor(a*avail_num),first_seq_length)
    m_b<-min(floor((avail_num-m_a)*b),second_seq_length)
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
    return(judge_const)
  }
  # 最差条件
  target_ratio = up = 1
  down = 0
  now_ability = fun.judge_match_ability(total_avail_num = remainTotalNum,
                                        avail_num = target_asset$num,
                                        first_seq = first_seq,
                                        second_seq = second_seq,
                                        first_seq_length = first_seq_length,
                                        second_seq_length = second_seq_length,
                                        third_seq = user_list$unmatched_amount%>%sort(.,decreasing=T))
  if(now_ability>=target_asset$amount){
    final_ratio = up = target_ratio
    user_list_pre<-copy(user_list)%T>%
      setorder(.,-target_amount)%>%
      .[unmatched_amount>=target_asset$amount/target_asset$num | userid%in%head(.,200)$userid,]
    
    target_ratio = 1/2*(up+down)
    stepnum = 0
    step = abs(target_ratio-up)
    
    while(stepnum<=5 & step>=0.0005){
      third_seq<-user_list_pre[,target_amount:=(premium*target_ratio-exp_match_amount)%>%pmax(.,0)%>%floor(.)]%>%
        .[target_amount>0,]%>%
        .$target_amount%>%
        sort(.,decreasing=T)
      now_ability<-fun.judge_match_ability(total_avail_num = remainTotalNum,
                                           avail_num = target_asset$num,
                                           first_seq = first_seq,
                                           second_seq = second_seq,
                                           first_seq_length = first_seq_length,
                                           second_seq_length = second_seq_length,
                                           third_seq = third_seq)
      if(now_ability>=target_asset$amount){
        final_ratio = up = target_ratio
        step = abs(1/2*(up+down)-target_ratio)
        target_ratio = 1/2*(up+down)
      }else{
        down = target_ratio
        step = abs(1/2*(up+down)-target_ratio)
        target_ratio = 1/2*(up+down)
      }
      stepnum = stepnum+1
    }
    print(paste("stepnum :",stepnum,"!"))
    return(list(final_ratio = final_ratio,
                target_user_list = user_list_pre,
                remainCash = sum(user_list_pre$target_amount)))
  }else{
    print("there is no solution for such asset!")
    return(list(final_ratio = c(),
                target_user_list = data.table(),
                remainCash = c()))
  }
}