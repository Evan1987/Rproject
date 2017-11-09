
#user_log输入条件，必须包含两个字段：userid、weekday（行为发生的dayOfWeek）
user_app_weekday_prefer<-function(user_log,P_value_threshold=0.99){
  library(data.table)
  setkey(user_log,userid)
  users<-unique(user_log$userid)
  result<-data.table()
  for(i in 1:length(users)){
    user_id<-users[i]
    open_aa<-user_log[user_id,nomatch=NA,mult="all"]
    weekday_prefer_zone<-open_aa[,.(freq=.N),by=weekday]
    x<-open_aa$weekday
    N<-length(x)
    p<-1/7
    q<-1-p  
    mean<-N*p
    sd<-sqrt(N*p*q)
    threshold<-P_value_threshold
    # 做单侧检验
    z_threshold<-abs(qnorm(1-threshold))
    x_threshold<-mean+z_threshold*sd
    weekday_prefer_zone<-weekday_prefer_zone[freq>=x_threshold,]
    result<-rbind(result,weekday_prefer_zone)
  }
  return(result)
}
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}