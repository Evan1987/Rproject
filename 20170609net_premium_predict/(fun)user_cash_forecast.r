user_cash_forecast<-function(trainResult,
                             calendar,
                             festival_label=c('7f1','7f2','7f3','7f4','7f5',
                                              '7b1','7b2','7b3','7b4','7b5','7b6','7b7',
                                              '3f1','3b1','3b2','3b3',
                                              '2b1','2b2'),
                             forecastTimeSpan = c("2017-06-02","2017-06-10")){
  library(data.table)
  library(magrittr)
  library(stringr)
  # ************************ 节日影响函数 ********************
  festivalInfluenceFun<-function(inputdate,festivalLabel){
    if(is.na(festivalLabel)|length(festivalLabel)==0){
      return(0)
    }
    # inputdate的节日标签在可处理范围中，且其前2日的真实值均已知
    if(festivalLabel%in%festival_label&nrow(festivalTrain[between(date,inputdate-2,inputdate-1),])==2)
    {
      # 提取对应节日标签 以前的真实值，及其前两日的数值，wide形式,且只取最近的x个数据
      # x = 假日天数*2
      result0<-festivalTrain[date<inputdate&label==festivalLabel,]%T>%
        setnames(.,'value','value_0')%>%
        .[,":="(former1day=date-1,former2day=date-2)]%>%
        .[festivalTrain[,c("date","value")],
          on=c("former1day"="date"),
          value_1:=i.value]%>%
        .[festivalTrain[,c("date","value")],
          on=c("former2day"="date"),
          value_2:=i.value]%>%
        .[,-c("former1day","former2day")]%>%
        .[!(is.na(value_1)|is.na(value_2)),]%>%
        tail(.,as.numeric(str_sub(festivalLabel,1,1))*2)
      
      #如果不存在以前的数据
      if(nrow(result0)==0){
        result<-festivalTrain[label==festivalLabel,]$value%>%
          mean(.)
        result = ifelse(is.nan(result),0,result)
      }else
      {
        # 加线性权值，日期越近越高，计算每日当日与前两日数值之间的加权差距
        result0[,wt:=.I]
        diff1 = weighted.mean(x = result0$value_1-result0$value_0,w = result0$wt)
        diff2 = weighted.mean(x = result0$value_2-result0$value_0,w = result0$wt)
        result = ((festivalTrain[date==(inputdate-1),]$value - diff1)+
                    (festivalTrain[date==(inputdate-2),]$value - diff2))/2
      }
    }else
    {
      result0<-tail(festivalTrain[label==festivalLabel & date<inputdate,],
                    as.numeric(str_sub(festivalLabel,1,1))*2)#2__4;3__6
      if(nrow(result0)==0){
        result<-festivalTrain[label==festivalLabel,]$value%>%
          mean(.)%>%
          ifelse(is.nan(.),0,.)
      }else{
        result0[,rank:=.I]
        result = weighted.mean(x = result0$value,wt = result0$rank)
      }
    }
    return(result)
  }
  
  forecastTimeSpan<-as.Date(forecastTimeSpan)
  calendar<-calendar[between(log_day,forecastTimeSpan[1],forecastTimeSpan[2]),]
  # ************************* 拆解训练结果 ********************
  week_seasonal<-trainResult$weekly_seasonal%>%setnames(.,names(.),c("dayOfWeek","week_seasonal"))
  month_seasonal<-trainResult$monthly_seasonal%>%setnames(.,names(.),c("dayOfMonth","month_seasonal"))  
  linear_trend<-trainResult$trend
  festivalTrain<-trainResult$festivalInfluence%>%setnames(.,names(.),c("label","date","value"))
  
  forecast_result<-copy(calendar)%>%
    .[week_seasonal,on=c("dayOfWeek"="dayOfWeek"),week_seasonal_part:=i.week_seasonal]%>%
    .[month_seasonal,on=c("dayOfMonth"="dayOfMonth"),month_seasonal_part:=i.month_seasonal]%>%
    .[,linear_trend_part:=linear_trend$value[1]+id*linear_trend$value[2]]%>%
    {
      festivalInfluence<-{
        Before_influenceResult<-mapply(festivalInfluenceFun,
                                       .$log_day,
                                       .$Before_Label)
        Back_InfluenceResult<-mapply(festivalInfluenceFun,
                                     .$log_day,
                                     .$Back_Label)
        In_influenceResult<-mapply(festivalInfluenceFun,
                                   .$log_day,
                                   .$In_Label)
        x<-as.numeric(!is.na(.$Before_Label))+
          as.numeric(!is.na(.$Back_Label))+
          as.numeric(!is.na(.$In_Label))+1e-7
        (((Before_influenceResult)+(Back_InfluenceResult)+(In_influenceResult))/x)%>%
          replace(.,is.nan(.),0)
      }
      .[,festival_part:=festivalInfluence]
    }%>%
    {
      vars = names(.)%>%.[which(str_detect(.,"_part"))]
      .[,total_value:=rowSums(.SD,dims = 1),.SDcols=vars]
    }
  
  return(forecast_result)
} 