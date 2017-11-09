

#对每一个cash_flow_main_train结果进行预测，而不是整体预测.
#trainResult 必须是由cash_flow_main_train计算得出的。
cash_flow_type_forecast<-function(trainResult,
                                  forecastDaySpan=c("2017-02-28","2017-03-10"),
                                  calendarPath){
  library(data.table)
  library(magrittr)
  library(sqldf)
  if(class(trainResult)!="cash_flow_type_train"){
    stop("Error! The trainResult must be the result of function cash_flow_main_train.")
  }
  
  # *************************拆解训练结果，并对残差结果进行带权重有放回抽样********************
  week_seasonal<-trainResult$weekly_seasonal
  month_seasonal<-trainResult$monthly_seasonal  
  linear_trend<-trainResult$trend
  festivalTrain<-trainResult$festivalInfluence
  
  residuals<-trainResult$residuals
  residuals[is.nan(residuals)]<-0
  high_value<-quantile(residuals,0.95,na.rm=T)
  low_value<-quantile(residuals,0.05,na.rm=T)
  residuals_trimmed<-residuals[residuals>=low_value&residuals<=high_value]
  
  residuals_density<-density(residuals_trimmed,n=1024)
  unit<-mean(diff(residuals_density$x,1))
  residuals_pdl<-data.frame(x=residuals_density$x,y=residuals_density$y*unit)
  
  
  festivalInfluenceFun<-function(inputdate,festivalLabel){
    if(is.na(festivalLabel)|length(festivalLabel)==0){
      result=0
    }
    else{
      if(str_sub(festivalLabel,1,1)==2){
        result<-festivalTrain[label==festivalLabel&date<=inputdate,]$value%>%
          tail(.,10)%>%
          mean(.,trim=0.05)
        if(is.nan(result)){
          result<-festivalTrain[label==festivalLabel,]$value%>%
            tail(.,10)%>%
            mean(.,trim=0.05)
        }
      }
      else{
        result<-festivalTrain[label==festivalLabel&date<=inputdate,]$value%>%
          mean(.)
        if(is.nan(result)){
          result<-festivalTrain[label==festivalLabel,]$value%>%
            mean(.)
        }
      }
    }
    return(result)
  }
  
  calendar<-fread(calendarPath)%>%
    .[,date:=as.Date(date)]%>%
    .[between(date,as.Date(forecastDaySpan[1]),as.Date(forecastDaySpan[2])),]
  
  forecast_result<-copy(calendar)%>%
    .[week_seasonal,on=c("dayOfWeek"="dayOfWeek"),nomatch=0,mult="all"]%>%
    .[month_seasonal,on=c("dayOfMonth"="dayOfMonth"),nomatch=0,mult="all"]%>%
    .[,linear_trend:=linear_trend$value[1]+id*linear_trend$value[2]]
  
  
  Before_influenceResult<-mapply(festivalInfluenceFun,
                                 forecast_result$date,
                                 forecast_result$Before_Label
                                )
  Back_InfluenceResult<-mapply(festivalInfluenceFun,
                               forecast_result$date,
                               forecast_result$Back_Label
                              )
  In_influenceResult<-mapply(festivalInfluenceFun,
                             forecast_result$date,
                             forecast_result$In_Label
                            )
  
  festivalInfluence<-ifelse(is.nan(Before_influenceResult),0,Before_influenceResult)+
    ifelse(is.nan(Back_InfluenceResult),0,Back_InfluenceResult)+
    ifelse(is.nan(In_influenceResult),0,In_influenceResult)
  
  
  # 通过带权重有放回抽样得到random_residuals 
  forecast_result<-data.table(forecast_result,festivalInfluence)%>%
    .[,random_residuals:=mean(sample(residuals_pdl$x,nrow(calendar),prob = residuals_pdl$y,replace = F))]%>%
    .[,":="(total=week_seasonal+month_seasonal+linear_trend+festivalInfluence+random_residuals,
            non_res_total=week_seasonal+month_seasonal+linear_trend+festivalInfluence)]
  
  return(forecast_result)
}