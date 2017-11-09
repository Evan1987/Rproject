library(data.table)
library(magrittr)
library(readr)
source('~/rstudio/20170518wbf_adjust/(fun)fun.premium_cal.r')
rawdata<-fread(file.choose())
rawdata[,dt:=as.Date(dt)]
rawdata[,premium_change:=ifelse(trade_type=="invest",amount,-amount)]


now_data<-read_csv("F:/Project/20170518沃百富数据调整/result/old/newdata_finished.csv",
                   col_types = cols(ord_no="c"))%>%as.data.table(.)
daily_rate = 5/100/365
end_dt = as.Date("2017-06-06")


premium_log<-now_data%T>%
  setorder(.,user_id,dt,trade_type)%>%
  .[,retain_days:=as.numeric(dplyr::lead(dt)-dt),by=user_id]%>%
  .[is.na(retain_days),retain_days:=as.numeric(end_dt-dt)]%>%
  .[,premium:=premiumCal(premium_change,days = retain_days,rate = daily_rate),by=user_id]


premium_log<-now_data%T>%
  setorder(.,user_id,dt,trade_type)%>%
  .[,retain_days:=as.numeric(dplyr::lead(dt)-dt),by=user_id]%>%
  .[is.na(retain_days),retain_days:=as.numeric(end_dt-dt)]%>%
  .[,premium:=fun.premium_cal(premium_change,days = retain_days,rate = daily_rate),by=user_id]


premium_now<-premium_log[,.(premium=last(premium)),by=user_id]%>%.[,premium:=pmax(round(premium,2),0)]
write.csv(premium_now,"F:/premium_now.csv",row.names = F)




system.time(premium_log[,premium:=premiumCal(premium_change,days = retain_days,rate = daily_rate),by=user_id])

system.time(premium_log[,premium:=fun.premium_cal(premium_change,days = retain_days,rate = daily_rate),by=user_id])








