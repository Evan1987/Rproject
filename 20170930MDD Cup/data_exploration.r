
library(data.table)
library(magrittr)
path = "F:/meituan_Project/20170930 MDD Cup/data/clean/"

totalTrainDF = fread(paste0(path,"trainDF.csv"))
weatherInfo = fread(paste0(path,"total_weather_status_log.csv"))
areaInfo = fread(paste0(path,"total_area_status_log.csv"))
testDF = fread(paste0(path,"testDF.csv"))
testADF = fread(paste0(path,"testADF.csv"))

orderSummary <-copy(areaInfo)%>%
{
  vars = c("working_rider_num","notbusy_working_rider_num","not_fetched_order_num","deliverying_order_num")
  .[,lapply(.SD,mean),by=.(area_id,time),.SDcols = vars]
}

trainDF = totalTrainDF[hour%in%c(11,17)]
areas = unique(orderSummary$area_id)



for(area in areas){
  dat = orderSummary[area_id==area]
  png(filename = paste0(path,"area_summary/rider/daily_",area,".png"),width = 1200,height = 600)
  plot(dat$time,dat$working_rider_num,col = "red",type="l",lwd= 7, main = paste(area,"-rider status"),xlim=c(9,21))
  lines(dat$time,dat$notbusy_working_rider_num,col="blue",lwd=7)
  dev.off()
  
  png(filename = paste0(path,"area_summary/order/daily_",area,".png"),width = 1200,height = 600)
  plot(dat$time,dat$not_fetched_order_num,col = "black",type="l",lwd = 7, main = paste(area,"-order status"),xlim=c(9,21))
  lines(dat$time,dat$deliverying_order_num,col = "red",lwd=7)
  dev.off()
}


poi_summary = totalTrainDF[,.(duration_mean=mean(delivery_duration),
                              duration_sd = sd(delivery_duration),
                              orderNum=.N,
                              distance_mean=mean(delivery_distance),
                              distance_sd = sd(delivery_distance)),by=.(poi_id,hour)]%>%
  setorder(.,poi_id,hour)

poi_summary_slim = poi_summary[hour%in%c(10,11,16,17)]


avail_features = names(trainDF)%>%{
  drop_cols = c("order_unix_time",
                "arriveshop_unix_time",
                "fetch_unix_time",
                "finish_unix_time",
                "id",
                "log_day",
                "hour",
                "minute",
                "delivery_duration",
                "temperature",
                "wind",
                "rain",
                "poi_lat",
                "poi_lng",
                "order_id")
  
  .[which(!.%in%drop_cols)]
}
labels = "delivery_duration"

library(corrplot)
corr = cor(trainDF[,.SD,.SDcols =c(avail_features,labels)])
corrplot(corr,method = "number")



####################### area_info explore ####################
## 11.10
sampleAreaInfo = areaInfo[hour%in%c(10,11,12,13,16,17,18)]%>%setorder(.,area_id,log_day,time)

sampleAreaSummary = sampleAreaInfo[,.(max_rider_num = max(working_rider_num),
                                      min_rider_num = min(working_rider_num),
                                      max_deliver_order_num = max(deliverying_order_num),
                                      min_deliver_order_num = min(deliverying_order_num)),by=.(area_id,log_day,hour)]


















