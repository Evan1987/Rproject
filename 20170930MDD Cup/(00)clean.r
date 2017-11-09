library(data.table)
library(magrittr)
library(stringr)
library(lubridate)
source('~/rstudio/20170930MDD Cup/(fun)cleanFunList.r', echo=TRUE)
path = "F:/meituan_Project/20170930 MDD Cup/data/"
#########################################################
# 1. clean for waybill_info 
waybill_info = fread(paste0(path,"train/waybill_info.csv"))

# ManhattanDist<-function(ja,wa,jb,wb,R=6371){
#   R*abs(wa-wb)*pi/180 + R*cos(0.5*(wa+wb))*abs(ja-jb)*pi/180
# }

bill_info = orderFun(waybill_info)%>%
  .[complete.cases(.),]%>%
{
  durationQ = quantile(.$delivery_duration,probs = seq(0,1,0.01))
  lo = durationQ[which(names(durationQ)=="1%")]
  up = durationQ[which(names(durationQ)=="99%")]
  .[delivery_duration>lo & delivery_duration<up,]
}
write.csv(bill_info,paste0(path,"clean/bill_info.csv"),row.names = F)

#########################################################


#########################################################
# 2. clean for area_realtime
area_realtime = fread(paste0(path,"train/area_realtime.csv"))
area_status_log = areaFun(area_realtime)
write.csv(area_status_log,paste0(path,"clean/area_status_log.csv"),row.names = F)
#########################################################


#########################################################
# 3. clean for weather_realtime
weather_realtime = fread(paste0(path,"train/weather_realtime.csv"))
weather_status_log = weatherFun(weather_realtime)
write.csv(weather_status_log,paste0(path,"clean/weather_status_log.csv"),row.names = F)


