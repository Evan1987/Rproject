cash_flow_main_forecast<-function(forecastDaySpan=c("2017-02-28","2017-03-10"),
                               S_trainTimeSpan=c("2016-02-01","2017-02-27"),
                               A_trainTimeSpan=c("2016-02-01","2017-02-27"),
                               B_trainTimeSpan=c("2016-02-01","2017-02-27"),
                               C_trainTimeSpan=c("2016-02-01","2017-02-27"),
                               D_trainTimeSpan=c("2016-02-01","2017-02-27"),
                               newcomer_trainTimeSpan=c("2017-01-01","2017-02-27"),
                               brush_trainTimeSpan=c("2017-01-01","2017-02-27"),
                               freshman_trainTimeSpan=c("2017-01-01","2017-02-27"),
                               trainSetPath,calendarPath)
{
  library(data.table)
  library(magrittr)
  library(tcltk)
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_train.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_num_train.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_forecast.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_num_forecast.R', encoding = 'UTF-8')
  
  types<-c("A","B","C","D","S","brush","freshman","newcomer")
  items<-c("type_num_ratio","amount_per_num")
  result<-data.table()
  for(type in types){
    for(item in items){
      # type_train
      eval(parse(text=paste0(type,"_",item,"_train",
                             "<-cash_flow_type_train(item='",item,
                             "',user_type='",type,
                             "',trainTimeSpan=",type,"_trainTimeSpan",
                             ",trainSetPath='",trainSetPath,
                             "',calendarPath='",calendarPath,"')")))
      print(paste0(type,"-",item,"training succeeded!"))
      # type_forecast
      eval(parse(text=paste0(type,"_",item,"_forecast",
                             "<-cash_flow_type_forecast(",type,"_",item,"_train",
                             ",forecastDaySpan=forecastDaySpan",
                             ",calendarPath='",calendarPath,"')")))
      print(paste0(type,"-",item,"forecasting succeeded!"))
      
    }
    # type_num_train
    eval(parse(text=paste0(type,"_type_num_train",
                           "<-cash_flow_type_num_train(user_type='",type,
                           "',trainTimeSpan=",type,"_trainTimeSpan",
                           ",trainSetPath='",trainSetPath,
                           "',calendarPath='",calendarPath,"')")))
    # type_num_forecast
    eval(parse(text=paste0(type,"_type_num_forecast",
                           "<-cash_flow_type_num_forecast(",type,"_type_num_train",
                           ",forecastDaySpan=forecastDaySpan)")))
    
    # 拼接结果至输出
    eval(parse(text=paste0("temp<-",
                           type,"_type_num_ratio_forecast[",type,"_amount_per_num_forecast,",
                           "on=c('date'='date'),nomatch=NA,mult='all'][,c('date','total','i.total')]")))
    eval(parse(text=paste0("temp<-temp[",type,"_type_num_forecast,",
                           "on=c('date'='date'),nomatch=NA,mult='all'][,c('date','type','type_num','total','i.total')]")))
    
    colnames(temp)<-c("date","type","type_num","type_num_ratio","amount_per_num")
    temp[,amount:=type_num*type_num_ratio*amount_per_num]
    result<-rbind(result,temp)
  }
  
  return(result)
}
