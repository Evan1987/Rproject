#根据用户不同生命周期的阶段来筛选原始数据
#df：原始数据
#period：需要筛选的用户阶段
#time_col：原始数据的时间列列名
#life_cycle_info：给定了用户生命周期各阶段的
fun.log_filter_with_period<-function(df,period="freshman",time_col="part_log_day",life_cycle_info,last_date){
  library(data.table)
  library(magrittr)
  period_start_day<-switch(period,
                           "freshman"="invest1st_day",
                           "growth"="freshman_end_day",
                           "maturity"="growth_end_day")
  
  period_end_day<-switch(period,
                        "freshman"="freshman_end_day",
                        "growth"="growth_end_day",
                        "maturity"=as.Date(NA))

  expr<-parse(text=time_col)
  cols = c("userid",period_start_day,period_end_day)%>%.[!is.na(.)]
  tmp<-life_cycle_info[,.SD,.SDcols=cols]%T>%
    { if(length(cols)<3){.[,end_day:=period_end_day]}
      names(.)<-c("userid","start_day","end_day")}%>%
    .[,end_day:=replace(end_day,which(is.na(end_day)),last_date)]
  
    
  result<-df[tmp,on="userid"]%>%
    .[!is.na(start_day),]%>%{
      expr2<-parse(text=ifelse(period=="freshman","start_day","start_day+1"))
      .[between(eval(expr),eval(expr2),end_day),]
    }%>%
    .[,-c("start_day","end_day")]
}