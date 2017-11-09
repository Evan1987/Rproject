

#####进行匹配过程的投资处理效率分析
##--time_seq:时间序列向量，用以翻译相关记录label的时间
##--cash_flow_log:匹配过程输出的资金流历史记录
##--invest_simu_data:整个过程的投资记录

invest_match_eff<-function(time_seq,cash_flow_log,invest_simu_data,origin_invest,item="regular"){
  library(data.table)
  library(magrittr)
  library(tcltk)
  source('~/rstudio/match150/(fun)fun.time_seq.r', encoding = 'UTF-8')
  fun.index<-function(amount,matched_premium_vec,label_vec){
    if(amount>max(matched_premium_vec)){
      return(as.numeric(NA))
    }else{
      index=min(which(matched_premium_vec>=amount))
      return(label_vec[index])
    }
  }
  vec.fun.index<-Vectorize(fun.index)
  index<-switch(item,
                "regular"=2,"current"=0,"tPlus"=1)
  
  invest<-invest_simu_data[type==index,]%>%
    .[,create_time:=as.POSIXct(create_time)]%>%
    rbind(.,origin_invest)%>%
    setorder(.,create_time)%>%
    .[,investid:=.I]
  
  time_seq_df<-data.table(log_time=time_seq)%>%.[,":="(label=.I-1,log_time=as.character(log_time))]
  end_for<-length(time_seq)-1
  
  users<-unique(invest$userid)
  num<-length(users)
  result<-data.table()
  pb <- tkProgressBar("进度","已完成 %", 0, 100)
  for(i in 1:num){
    user_invest<-invest[userid==users[i],]%T>%
      setorder(.,investid)%>%
      .[,label:=cut(create_time,time_seq,labels = 1:end_for)%>%as.integer(.)]%>%
      .[,cum_invest_premium:=cumsum(amount)]%>%
      .[cash_flow_log[remark==0,],
        on=c("userid"="userid","label"="label"),
        actual_premium:=i.unmatched_premium]%>%
      .[,actual_premium:=ifelse(is.na(actual_premium),0,actual_premium)]%>%
      .[,matched_premium:=cum_invest_premium-actual_premium]%>%
      .[,finished_label:=vec.fun.index(cum_invest_premium,matched_premium,label)]%>%
      .[nrow(.),finished_label:=ifelse(label<end_for&is.na(finished_label),label+1,finished_label)]%>%
      .[time_seq_df,on=c("label"="label"),end_time:=as.POSIXct(i.log_time)]%>%
      .[,difftime:=difftime(end_time,create_time,units = "hours")%>%as.numeric(.)%>%round(.,2)]
    result<-rbind(result,user_invest)
    info<- sprintf("已完成 %.2f%%", round(i*100/num,2))  
    setTkProgressBar(pb, i*100/num, sprintf("进度 (%s)", info),info)
  }
  return(result)
}

























