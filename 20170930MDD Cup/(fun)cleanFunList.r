
orderFun<-function(waybill_info){
  library(data.table)
  library(magrittr)
  library(stringr)
  library(lubridate)
  bill_info = {
    x = sapply(waybill_info,class)
    vars = names(x)[x=="character"]
    if(length(vars)>0){
      copy(waybill_info)%>%
        .[,(vars):=lapply(.SD,as.numeric),.SDcols = vars]
    }else{
      waybill_info
    }
  }%>%
    # matrixDF = as.matrix(bill_info)
    # validEle = matrixDF[is.na(matrixDF)] ### empty  
    
    ## 1.2 basic feature and filter
    .[,":="(order_log_time=as.POSIXct(order_unix_time,origin=as.POSIXct("1970-01-01 0:00:00",tz="UTC")))]%>%
    .[,":="(log_day=lubridate::date(order_log_time),hour=hour(order_log_time),minute=minute(order_log_time))]%>%
    .[,time:=hour+minute/60]%>%
    .[,-c("order_log_time")]
  return(bill_info)
}


areaFun<-function(area_realtime){
  library(data.table)
  library(magrittr)
  library(stringr)
  library(lubridate)
  source('~/rstudio/20170930MDD Cup/(fun)fillNAByKNN.r', encoding = 'UTF-8', echo=TRUE)
  area_status_log = area_realtime[complete.cases(area_realtime),]%>%
    .[,log_day:=as.Date(as.character(date),"%Y%m%d")]%>%
    .[,hour:=floor(time/100)]%>%
    .[,minute:=time%%100]%>%
    .[,time:=hour+minute/60]%>%
    {
      vars = names(.)%>%.[str_detect(.,"order_num")]
      .[,(vars):=lapply(.SD,function(x) replace(x,x<0,NA)),.SDcols = vars]%T>%
        setorder(.,area_id,log_day,time)%>%
        .[,(vars):=lapply(.SD,function(x) as.integer(fillNAByKNN(x,time,2))),by=.(log_day,area_id),.SDcols = vars]
    }%>%
    .[,-c("date","log_unix_time")]
  return(area_status_log)
}


weatherFun<-function(weather_realtime,method=c("total","partial")){
  library(data.table)
  library(magrittr)
  library(stringr)
  library(lubridate)
  source('~/rstudio/20170930MDD Cup/(fun)fillNAByKNN.r', encoding = 'UTF-8', echo=TRUE)
  method = match.arg(method)
  
  weatherDF = {
    x = sapply(weather_realtime,class)
    vars = names(x)[x=="character"]
    copy(weather_realtime)%>%
      .[,(vars):=lapply(.SD,as.numeric),.SDcols = vars]%>%
      .[,log_day:=as.Date(as.character(date),"%Y%m%d")]%>%
      .[,":="(hour=floor(time/100),minute=time%%100)]%>%
      .[,time:=hour+minute/60]%>%
      .[,temperature:=round(temperature)]%>%
      .[,(vars):=lapply(.SD,function(x) fillNAByKNN(x,time,k=2)),by=.(log_day,area_id),.SDcols = vars]%>%
      .[,-c("date","log_unix_time")]
  }
  result = {
    if(method=="total"){
      df = expand.grid(hour=0:23,minute=0:59)%>%
        as.data.table(.)%>%
        .[,time:=hour+minute/60]%>%
        setorder(.,time)
      weatherDF[,-c("hour","minute")]%>%
      .[,.SD[df[,c("hour","minute","time")],on="time",roll=T],by=.(area_id,log_day)]
    }else if(method=="partial"){
     weatherDF[,.SD[CJ(minute=min(minute):max(minute)),on="minute",roll=T],by=.(area_id,log_day,hour)]%>%
        .[,time:=hour+minute/60]
    }
  }
  
  return(result)
}
