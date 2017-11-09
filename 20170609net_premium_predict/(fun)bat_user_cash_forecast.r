bat_user_cash_forecast<-function(trainSetTotal,
                                   calendar,
                                   user_type_list=list(c("S","A","B"),
                                                       c("C","D","E"),
                                                       c("brush"),
                                                       c("freshman"),
                                                       c("newcomer")),
                                   trainTimeSpan_list = list(c("2016-02-01","2017-05-31"),
                                                             c("2016-02-01","2017-05-31"),
                                                             c("2017-04-01","2017-05-31"),
                                                             c("2017-04-01","2017-05-31"),
                                                             c("2017-04-01","2017-05-31")),
                                   forecastTimeSpan=c("2017-06-02","2017-06-10"),
                                   value.var = "net_premium",
                                   detail=F){
  library(data.table)
  library(magrittr)
  library(stringr)
  source('~/rstudio/20170609net_premium_predict/(fun)user_cash_train.r', encoding = 'UTF-8', echo=TRUE)
  source('~/rstudio/20170609net_premium_predict/(fun)user_cash_forecast.r', encoding = 'UTF-8', echo=TRUE)
  
  n = length(user_type_list)
  forecastResult<-data.table()
  for(i in 1:n){
    user_types = user_type_list[[i]]
    trainTimeSpan = trainTimeSpan_list[[i]]
    trainResult <- user_cash_train(trainSetTotal = trainSetTotal,
                                     calendar = calendar,
                                     user_types = user_types,
                                     trainTimeSpan = trainTimeSpan,
                                   value.var = value.var)
    forecastTemp <- user_cash_forecast(trainResult = trainResult,
                                         calendar = calendar,
                                         forecastTimeSpan = forecastTimeSpan)%>%
      .[,type:=str_c(user_types,collapse = "")]
    forecastResult<-rbind(forecastResult,forecastTemp)
  }
  if(detail){
    setorder(forecastResult,log_day,type)
    return(forecastResult)
  }else{
    forecastResult[,total_value:=replace(total_value,is.nan(total_value),0)]
    forecastResult<-forecastResult[,.(total_amount=sum(total_value)),by=log_day]%T>%setorder(.,log_day)
  }
  return(forecastResult)
}