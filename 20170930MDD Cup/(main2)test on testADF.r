library(data.table)
library(magrittr)
library(lubridate)
source('~/rstudio/20170930MDD Cup/(fun)batchTest.r', echo=TRUE)


path = "F:/meituan_Project/20170930 MDD Cup/data/clean/"
totalTrainDF = fread(paste0(path,"trainDF.csv"))
weatherInfo = fread(paste0(path,"total_weather_status_log.csv"))
areaInfo = fread(paste0(path,"total_area_status_log.csv"))
testADF = fread(paste0(path,"testADF.csv"))

# 特征工程函数，只增加特征，不删除特征
featurePro1<-function(df){
  result = copy(df)%>%
    .[,dayIndex:=wday(as.Date(log_day))-1]%>%
    .[,dayIndex:=replace(dayIndex,dayIndex==0,7)]%>%
    .[,isWorkDay:=ifelse(dayIndex%in%c(6,7),0,1)]%>%
    .[,totalValue:=food_total_value + box_total_value]
  return(result)
}

###############################   test on testADF  ####################################
trainDF = totalTrainDF[hour%in%c(10,16)]%>%featurePro1(.)
testADF = featurePro1(testADF)
## XgBoost
avail_features = names(trainDF)%>%{
  drop_cols = c("order_unix_time",
                "arriveshop_unix_time",
                "fetch_unix_time",
                "finish_unix_time",
                "food_total_value",
                "box_total_value",
                "id",
                "log_day",
                "hour",
                "minute",
                "time",
                "delivery_duration",
                "temperature",
                "wind",
                "rain",
                "poi_id",
                "poi_lat",
                "poi_lng",
                "customer_longitude",
                "customer_latitude",
                "order_id")
  
  .[which(!.%in%drop_cols)]
}

labels = "delivery_duration"

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
              "num_round"=500)

# xgModel = getXgModel(avail_features,labels,trainDF,params)
# importance_matrix = xgb.importance(model=xgModel,feature_names = avail_features)
# xgb.plot.importance(importance_matrix)

result = batchTest(trainDF,testADF,features = avail_features,labels = labels,params = params,workType = "test")

xgModel = result$model
importance_matrix = xgb.importance(model=xgModel,feature_names = avail_features)
xgb.plot.importance(importance_matrix)
###############################   test on testDF  ####################################
finalTrainDF = totalTrainDF[hour%in%c(11,17)]%>%featurePro1(.)
testDF = featurePro1(testDF)

result = batchTest(finalTrainDF,testDF,avail_features,labels,params,workType = "output")
sub = result$result
write.csv(sub,"F:/meituan_Project/20170930 MDD Cup/data/sub/sub_result1110_01.csv",row.names = F)



