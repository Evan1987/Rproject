
library(data.table)
library(magrittr)
path = "F:/meituan_Project/20170930 MDD Cup/data/clean/"

trainDF = fread(paste0(path,"trainDF.csv"))
weatherInfo = fread(paste0(path,"total_weather_status_log.csv"))
areaInfo = fread(paste0(path,"total_area_status_log.csv"))
testDF = fread(paste0(path,"testDF.csv"))


orderSummary <-copy(areaInfo)%>%
{
  vars = c("working_rider_num","notbusy_working_rider_num","not_fetched_order_num","deliverying_order_num")
  .[,lapply(.SD,mean),by=.(area_id,time),.SDcols = vars]
}

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













































drop_cols = c("order_unix_time",
              "arriveshop_unix_time",
              "fetch_unix_time",
              "finish_unix_time",
              "id",
              "log_day",
              "temperature",
              "order_id",
              "customer_longitude",
              "customer_latitude",
              "delivery_duration")

features = names(trainDF)%>%.[which(!.%in%drop_cols)]
labels = "delivery_duration"
x_train = trainDF[,.SD,.SDcols=features]%>%as.matrix(.)
y_train = trainDF$delivery_duration%>%as.matrix(.)

x_test = testDF[,.SD,.SDcols=features]%>%as.matrix(.)

dtrain = xgb.DMatrix(x_train,label=y_train)
dtest = xgb.DMatrix(x_test)

watchlist = list("train"=dtrain)

params = list("boost"="gbtree",
              "silent"=1,
              "eta"=0.15,
              "max_depth"=5,
              "objective"="reg:linear",
              "eval_metric"="mae",
              "colsample_bytree"=0.65,
              "subsample"=0.8,
              "nthread"=-1,
              "seed"=20171001,
              "num_round"=1000)

xgbModel = xgb.train(params = params,
                     nrounds = params$num_round,
                     data = dtrain,
                     watchlist = watchlist,
                     verbose = 1)

pred = predict(xgbModel,x_test)


sub = data.table(order_id = testDF$order_id,
                 delivery_duration = pred)
write.csv(sub,paste0(testPath,"sub_result1106_03.csv"),row.names = F)
