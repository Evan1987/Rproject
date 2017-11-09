
# 根据资产、资金队列情况，用二分法求最优产品调和系数。
r_opt<-function(unmatched_amount,
                avail_num,
                regular_seq,
                tPlus_seq,
                current_seq,
                r_regular_init=0.6,
                r_tPlus_init=0.75)
{
  library(data.table)
  library(magrittr)
  # 【FUN】计算判定值的函数
  judge_const_calculation<-function(r_regular,r_tPlus)
  {
    m_regular<-min(floor(r_regular*avail_num),nrow(regular_seq))
    m_tPlus<-min(floor((avail_num-m_regular)*r_tPlus),nrow(tPlus_seq))
    m_current<-min(avail_num-m_regular-m_tPlus,nrow(current_seq))
    
    judge_const<-(
      regular_seq$unmatched_regular_premium%>%
        head(.,m_regular)%>%
        sum(.))+
      (
        tPlus_seq$unmatched_tPlus_premium%>%
          head(.,m_tPlus)%>%
          sum(.))+
      (
        current_seq$unmatched_current_premium%>%
          head(.,m_current)%>%
          sum(.)
      )
    return(judge_const)
  }
  
  up=1
  down=r_regular_init
  # 下界计算
  judge_const<-judge_const_calculation(r_regular = r_regular_init,r_tPlus = r_tPlus_init)
  # 若下界不符合要求，则输出为0值
  if(judge_const<unmatched_amount){
    r_opt_result<-c(0,0,0)
  }
  # 若上界满足要求，则输出（1,0,0）
  else if(judge_const_calculation(r_regular = up,r_tPlus = 0)>=unmatched_amount&
          nrow(regular_seq)>=up*avail_num){
    r_opt_result<-c(up,0,0)
  }
  # 若最优值在上下界之间
  else{
    ######################### 迭代求最优r_regular ###################
    #初始值应取上下界的中点
    r=1/2*(up+down)
    step=1
    while(step>=0.01){
      r=1/2*(up+down)
      judge_const<-judge_const_calculation(r,r_tPlus_init)
      #若满足，则下界提升为当前r值，r值取提升后下界与上界中点
      if(judge_const>=unmatched_amount){
        down=r
        step=1/2*(up+down)-r
      }
      #若不满足，上界下降为当前r值，r值取降低后上界与下界中点
      else{
        up=r
      }
      m_regular=floor(r_regular*avail_num)
      if(m_regular>=nrow(regular_seq)){
        #向上取，保留两位小数
        r=ceiling(nrow(regular_seq)/avail_num*100)/100
        break
      }
    }
    #输出最终迭代结果
    r_regular=r
    m_regular=min(floor(r_regular*avail_num),nrow(regular_seq))
    ######################### 在最优r_regular基础上迭代求最优r_tPlus ######################
    #初始化
    up=1
    down=r_tPlus_init
    avail_num2=avail_num-m_regular
    if(judge_const_calculation(r_regular = r_regular,r_tPlus = up)>=unmatched_amount&
       nrow(tPlus_seq)>=avail_num2){
      r_opt_result<-c(r_regular,up,1-up)
    }
    else{
      r=1/2*(up+down)
      step=1
      while(step>=0.01){
        r=1/2*(up+down)
        judge_const<-judge_const_calculation(r_regular,r)
        #若满足，则下界提升为当前r值，r值取提升后下界与上界中点
        if(judge_const>=unmatched_amount){
          down=r
          step=1/2*(up+down)-r
        }
        #若不满足，上界下降为当前r值，r值取降低后上界与下界中点
        else{
          up=r
        }
        m_tPlus=floor(avail_num2*r_tPlus)
        if(m_tPlus>=nrow(tPlus_seq)){
          #向上取，保留两位小数
          r=ceiling(nrow(tPlus_seq)/avail_num2*100)/100
          break
        }
      }
      r_tPlus=r
      r_opt_result<-c(r_regular,r_tPlus,1-r_tPlus)
    }
  }
  return(r_opt_result)
}
  