library(data.table)
library(magrittr)
library(readr)
library(xgboost)
source('~/rstudio/20170930MDD Cup/(fun)cleanFunList.r', echo=TRUE)
path = "F:/meituan_Project/20170930 MDD Cup/data/clean/"

###############################
bill_info = read_csv(paste0(path,"bill_info.csv"))%>%as.data.table(.)
area_status_log = read_csv(paste0(path,"area_status_log.csv"))%>%as.data.table(.)
weather_status_log = read_csv(paste0(path,"weather_status_log.csv"))%>%as.data.table(.)

testPath = "F:/meituan_Project/20170930 MDD Cup/data/test/"
test_bill = fread(paste0(testPath,"waybill_info_test_b.csv"))%>%
  orderFun(.)
test_a_bill = fread(paste0(testPath,"waybill_info_test_a.csv"))%>%
  orderFun(.)%>%
  .[complete.cases(.)]
test_area_status = fread(paste0(testPath,"area_realtime_test.csv"))%>%
  areaFun(.)
test_weather_status = fread(paste0(testPath,"weather_realtime_test.csv"))%>%
  weatherFun(.,"partial")

## 合并全部已知数据
total_weather_status_log = rbind(weather_status_log,test_weather_status)%>%
{
  df = .[,CJ(hour=unique(hour),minute=0:59),by=.(area_id,log_day)]%>%
    .[,time:=hour+minute/60]%>%
    .[,-c("hour","minute")]
  .[,-c("hour","minute")]%>%
  .[df,on=c("area_id","log_day","time"),roll=T]
}%>%
{
  vars = c("temperature","rain","wind")
  .[,(vars):=lapply(.SD,function(x) fillNAByKNN(x,time,2)),.SDcols=vars,by=.(area_id,log_day)]
}%>%
  .[,hour:=floor(time)]%>%
  .[,minute:=round((time%%1)*60)]

total_area_status_log = rbind(area_status_log,test_area_status)

trainDF = copy(bill_info)%>%
  .[area_status_log,on=c("area_id","log_day","hour","minute"),
    ":="(working_rider_num=i.working_rider_num,
         notbusy_working_rider_num=i.notbusy_working_rider_num,
         not_fetched_order_num=i.not_fetched_order_num,
         deliverying_order_num=i.deliverying_order_num)]%>%
  .[weather_status_log,on=c("area_id","log_day","hour","minute"),
    ":="(temperature=i.temperature,wind=i.wind,rain=i.rain)]%>%
  .[complete.cases(.)]  ##缺失的都是0点数据

## 删除无用的数据
rm(list=c("test_area_status","test_weather_status","area_status_log","weather_status_log","bill_info"))
gc()
###############################

testDF = copy(test_bill)%>%
  .[total_area_status_log,on=c("area_id","log_day","hour","minute"),
    ":="(working_rider_num=i.working_rider_num,
         notbusy_working_rider_num=i.notbusy_working_rider_num,
         not_fetched_order_num=i.not_fetched_order_num,
         deliverying_order_num=i.deliverying_order_num)]%>%
  .[total_weather_status_log,on=c("area_id","log_day","hour","minute"),
    ":="(temperature=i.temperature,wind=i.wind,rain=i.rain)]


testADF = copy(test_a_bill)%>%
  .[total_area_status_log,on=c("area_id","log_day","hour","minute"),
    ":="(working_rider_num=i.working_rider_num,
         notbusy_working_rider_num=i.notbusy_working_rider_num,
         not_fetched_order_num=i.not_fetched_order_num,
         deliverying_order_num=i.deliverying_order_num)]%>%
  .[total_weather_status_log,on=c("area_id","log_day","hour","minute"),
    ":="(temperature=i.temperature,wind=i.wind,rain=i.rain)]



## 生成集合干净的数据
write.csv(trainDF,paste0(path,"trainDF.csv"),row.names = F)
write.csv(total_weather_status_log,paste0(path,"total_weather_status_log.csv"),row.names = F)
write.csv(total_area_status_log,paste0(path,"total_area_status_log.csv"),row.names = F)
write.csv(testADF,paste0(path,"testADF.csv"),row.names = F)
write.csv(testDF,paste0(path,"testDF.csv"),row.names = F)

# ManhattanDist<-function(ja,wa,jb,wb,R=6371){
#   R*abs(wa-wb)*pi/180 + R*cos(0.5*(wa+wb))*abs(ja-jb)*pi/180
# }
# xxsub = fread("F:/meituan_Project/20170930 MDD Cup/data/total/sub_xgb_starter.csv")
# combined = sub[xxsub,on="order_id",formerResult:=i.delivery_duration]

