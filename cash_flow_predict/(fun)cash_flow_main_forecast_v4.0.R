
# 对v3.0版本的修正，允许以列表形式（允许了用户组合）输入计算参数。
# 对v2.0的版本，brush和newcomer用户只进行amount预估，而不估计其具体计算参数。
# 对v1.0的详细版本，支持今后简便的参数细化。需要仔细考量各个参数训练的详细时间跨度。
cash_flow_main_forecast<-function(user_type_list=list(c("S","A","B"),
                                                      c("C","D"),
                                                      c("brush","newcomer"),
                                                      c("freshman")),
                                  type_train_Timelist=list(c("2016-02-01","2017-02-27"),
                                                           c("2016-02-01","2017-02-27"),
                                                           c("2017-01-01","2017-02-27"),
                                                           c("2017-01-01","2017-02-27")),
                                  type_num_train_Timelist=list(c("2017-01-01","2017-02-27"),
                                                               c("2017-01-01","2017-02-27"),
                                                               c("2017-02-01","2017-02-27"),
                                                               c("2017-01-01","2017-02-27")),
                                  item_list=c("amount","amount","amount","amount"),
                                  forecastDaySpan=c("2017-03-31","2017-04-05"),
                                  trainSetPath,
                                  calendarPath,
                                  detail=F)
{
  n<-length(user_type_list)
  if(any(
     c(length(user_type_list),
       length(type_train_Timelist),
       length(type_num_train_Timelist),
       length(item_list))!=rep(n,4)))
  {
    stop("Error! user_type_list, type_train_list, type_num_train_list and item_list must have same length!")
  }
  
  library(data.table)
  library(magrittr)
  library(tcltk)
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_train.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_num_train.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_forecast.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_num_forecast.R', encoding = 'UTF-8')
  
  result<-data.table()
  for(i in 1:n){
    user_type<-user_type_list[[i]]
    type_trainTimeSpan<-type_train_Timelist[[i]]
    type_num_trainTimeSpan<-type_num_train_Timelist[[i]]
    item<-item_list[i]
    
    if(item=="all"){
      type_num_ratio_train<-cash_flow_type_train(item="type_num_ratio",
                                                 user_type = user_type,
                                                 trainTimeSpan = type_trainTimeSpan,
                                                 trainSetPath = trainSetPath,
                                                 calendarPath = calendarPath)
      amount_per_num_train<-cash_flow_type_train(item="amount_per_num",
                                                 user_type = user_type,
                                                 trainTimeSpan = type_trainTimeSpan,
                                                 trainSetPath = trainSetPath,
                                                 calendarPath = calendarPath)
      type_num_train<-cash_flow_type_num_train(user_type = user_type,
                                               trainTimeSpan = type_num_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
      type_num_ratio_forecast<-cash_flow_type_forecast(type_num_ratio_train,
                                                       forecastDaySpan = forecastDaySpan,
                                                       calendarPath = calendarPath)
      amount_per_num_forecast<-cash_flow_type_forecast(amount_per_num_train,
                                                       forecastDaySpan = forecastDaySpan,
                                                       calendarPath = calendarPath)
      type_num_forecast<-cash_flow_type_num_forecast(type_num_train,
                                                     forecastDaySpan = forecastDaySpan)
      
      
      user_result<-type_num_ratio_forecast[amount_per_num_forecast,
                                           on=c("date"="date"),nomatch=NA,mult="all"]%>%
        .[,c("date","total","i.total","non_res_total","i.non_res_total")]%>%
        .[type_num_forecast,on=c("date"="date"),nomatch=NA,mult="all"]%>%
        .[,c("date","type","type_num","total","i.total","non_res_total","i.non_res_total")]
      
      colnames(user_result)<-c("date",
                               "type",
                               "type_num",
                               "type_num_ratio",
                               "amount_per_num",
                               "pure_type_num_ratio",
                               "pure_amount_per_num")
      
      user_result[,":="(amount=type_num*type_num_ratio*amount_per_num,
                        pure_amount=type_num*pure_type_num_ratio*pure_amount_per_num)]
    }
    if(item=="amount"){
      amount_train<-cash_flow_type_train(item = "amount",
                                         user_type = user_type,
                                         trainTimeSpan = type_trainTimeSpan,
                                         trainSetPath = trainSetPath,
                                         calendarPath = calendarPath)
      type_num_train<-cash_flow_type_num_train(user_type = user_type,
                                               trainTimeSpan = type_num_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
      
      amount_forecast<-cash_flow_type_forecast(amount_train,
                                               forecastDaySpan = forecastDaySpan,
                                               calendarPath = calendarPath)
      
      type_num_forecast<-cash_flow_type_num_forecast(type_num_train,
                                                     forecastDaySpan = forecastDaySpan)
      
      user_result<-type_num_forecast[amount_forecast,
                                     on=c("date"="date"),nomatch=NA,mult="all"]%>%
        .[,":="(type_num_ratio=NA,
                amount_per_num=NA,
                pure_type_num_ratio=NA,
                pure_amount_per_num=NA,
                amount=total,
                pure_amount=non_res_total)]%>%
        .[,c("date","type","type_num","type_num_ratio","amount_per_num",
             "pure_type_num_ratio","pure_amount_per_num","amount","pure_amount")]
    }
    result<-rbind(result,user_result)
    print(paste(i,"-finished!"))
  }
  
  if(detail){
    return(result)
  }
  else{
    result_summary<-result[,.(amount=sum(amount),pure_amount=sum(pure_amount)),by="date"]
    return(result_summary)
  }
}
