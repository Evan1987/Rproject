# 仅针对type_num进行预测，输入数据为相对应的cash_flow_type_num_train的输出

cash_flow_type_num_forecast<-function(trainResult,
                                      forecastDaySpan=c("2017-02-28","2017-03-10"))
{
  library(data.table)
  library(magrittr)
  
  if(class(trainResult)!="cash_flow_type_num_train"){
    stop("Error! The trainResult must be the result of function cash_flow_type_num_train.")
  }
  
  user_type<-trainResult$user_type
  remark<-trainResult$remark
  
  # *******************************以下为针对不同训练结果（linear、random、no forecast）的预测********
  # linear 对应的是 S-D的人群，基于预测始点按训练斜率进行递增
  if(remark=="linear"){
    coef<-trainResult$coef
    slope<-coef$value[2]
    
    forecast_initial<-trainResult$forecast_initial
    initial_date<-forecast_initial$date[1]
    initial_value<-forecast_initial$type_num[1]
    
    forecast_result<-data.table(type=user_type,
                                date=seq.Date(from=as.Date(forecastDaySpan[1]),
                                              to=as.Date(forecastDaySpan[2]),
                                              by="day")
                                )%>%
      .[,type_num:=round(as.numeric(difftime(date,as.Date(initial_date),units = "days"))*slope+initial_value)]
  }
  # random对应的是newcomer和brush人群，采用的是根据训练分布律在区间内进行加权有放回抽样
  else if(remark=="random"){
    pdl<-trainResult$pdl
    forecast_result<-data.table(type=user_type,
                                date=seq.Date(from=as.Date(forecastDaySpan[1]),
                                              to=as.Date(forecastDaySpan[2]),
                                              by="day"),
                                type_num=round(sample(pdl$x,
                                                      as.numeric(
                                                        difftime(as.Date(forecastDaySpan[2]),
                                                                 as.Date(forecastDaySpan[1]),
                                                                 units = "days"))+1,
                                                      replace = T
                                                      )
                                               )
    )
  }
  # no forecast对应的是freshman人群，这个人群数目可根据当前预测始点和一定日期的newcomer进行推断
  # 理论上 freshman人数等于距预测日期[1,29]天newcomer人数和。
  # 计算上采用预测日期前一天人数+预测日期前一天newcomer人数-预测日期30天的newcomer人数
  else if(remark=="no forecast"){
    forecast_initial<-trainResult$forecast_initial
    freshman_initial<-forecast_initial[type=="freshman",]
    newcomer_initial<-forecast_initial[type=="newcomer",]
    pdl<-trainResult$pdl
    
    newcomer_result<-data.table(date=seq.Date(from=as.Date(forecastDaySpan[1]),
                                              to=as.Date(forecastDaySpan[2]),
                                              by="day"),
                                newcomer_type_num=round(sample(pdl$x,
                                                               as.numeric(
                                                                 difftime(as.Date(forecastDaySpan[2]),
                                                                          as.Date(forecastDaySpan[1]),
                                                                          units = "days")
                                                               ) + 1,
                                                               replace = T))
    )%>%
      rbind(.,data.table(date=newcomer_initial$date,
                         newcomer_type_num=newcomer_initial$type_num))%T>%
      setkey(.,date)%>%
      .[,":="(start_date=date+1,end_date=date+30)]
    
    
    
    
    forecast_result<-data.table(type=user_type,
                                date=seq.Date(from=as.Date(forecastDaySpan[1]),
                                              to=as.Date(forecastDaySpan[2]),
                                              by="day"))%T>%
      setkey(.,date)%>%
      .[newcomer_result[,c("start_date","newcomer_type_num")],
        on=c("date"="start_date"),
        nomatch=0,
        mult="all"]%>%
      .[newcomer_result[,c("end_date","newcomer_type_num")],
        on=c("date"="end_date"),
        nomatch=0,
        mult="all"]%>%
      .[,type_num:=freshman_initial$type_num[1]+newcomer_type_num-i.newcomer_type_num]%>%
      .[,c("type","date","type_num")]
  }
  return(forecast_result)
}