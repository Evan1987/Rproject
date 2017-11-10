library(data.table)
library(magrittr)
library(lubridate)
source('~/rstudio/20170930MDD Cup/(fun)getXgModel.r', echo=TRUE)
source('~/rstudio/20170930MDD Cup/(fun)getXgSub.r', echo=TRUE)
source('~/rstudio/20170930MDD Cup/(fun)maeEval.r', echo=TRUE)
source('~/rstudio/20170930MDD Cup/(fun)kvgenerator.r', echo=TRUE)
path = "F:/meituan_Project/20170930 MDD Cup/data/clean/"
totalTrainDF = fread(paste0(path,"trainDF.csv"))
weatherInfo = fread(paste0(path,"total_weather_status_log.csv"))
areaInfo = fread(paste0(path,"total_area_status_log.csv"))
testADF = fread(paste0(path,"testADF.csv"))

featurePro1<-function(df){
  result = copy(df)%>%
    .[,dayIndex:=wday(as.Date(log_day))-1]%>%
    .[,dayIndex:=replace(dayIndex,dayIndex==0,7)]%>%
    .[,isWorkDay:=ifelse(dayIndex%in%c(6,7),0,1)]
  return(result)
}

trainDF = totalTrainDF[hour%in%c(11,17)]#%>%
# {
#   distance = quantile(.$delivery_distance,probs=seq(0,1,0.01))
#   lo = distance[which(names(distance)=="1%")]
#   up = distance[which(names(distance)=="99%")]
#   .[between(delivery_distance,lo,up),]
# }
  
testDF = fread(paste0(path,"testDF.csv"))%>%featurePro1(.)
trainDF = featurePro1(trainDF)

##################### Single Model ##########################
## XgBoost
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

xgModel = getXgModel(avail_features,labels,trainDF,params)
importance_matrix = xgb.importance(model=xgModel,feature_names = avail_features)
xgb.plot.importance(importance_matrix)

sub = getXgSub(xgModel,testDF,avail_features,labels)
# result = maeEval(sub,test)
# resultDF = result$result
write.csv(sub,paste0("F:/meituan_Project/20170930 MDD Cup/data/sub/sub_result1109_03.csv"),row.names = F)

## GBDT
library(gbm)
generateFormula<-function(features,labels){
  library(stringr)
  rhs = str_c(features,collapse = "+")
  formulaStr = paste0(labels,"~",rhs)
  return(eval(parse(text=formulaStr)))
}
formula = generateFormula(avail_features,labels)
gbm1 = gbm(formula, 
           data = trainDF, 
           var.monotone = rep(0,length(avail_features)),
           distribution = "gaussian",
           n.trees = 1000,
           shrinkage = 0.15,
           interaction.depth = 3,
           bag.fraction = 0.5,
           train.fraction = 0.65,
           n.minobsinnode = 10,
           cv.folds = 5,
           keep.data = FALSE,
           verbose = TRUE,
           n.cores = 8)

best.iter = gbm.perf(gbm1,method = "cv")
summary(gbm1,n.trees = best.iter)

pred = predict.gbm(gbm1,testDF,n.trees = best.iter)
sub = data.table(testDF$order_id,pred)%T>%
{
  names(.)<-c("order_id",labels)
}
write.csv(sub,paste0("F:/meituan_Project/20170930 MDD Cup/data/sub/sub_result1109_04.csv"),row.names = F)


####################### k fold 3 model stack ########################
# 模型数量和K折数
foldNum=5
modelNum = 3
kvList = kfoldGenerator(trainDF,foldNum)
# 增加辅助列帮助使用K折法训练
trainDF[,id:=.I]
# 记录每个模型选择特征的容器
featureList = list()
# 存放每个模型的容器
modelList = list()
# 训练集和最终测试集的大小
n_train = nrow(trainDF)
n_test = nrow(testDF)
# 记录每个模型在训练集和最终测试集的预测结果
finalTrainResultMatrix = matrix(0,nrow = n_train,ncol = modelNum)
finalTestResultMatrix = matrix(0,nrow = n_test,ncol = modelNum)

for(j in 1:modelNum){
  # 对此模型筛选feature，只从可选feature集合里挑出10个
  features = sample(avail_features,10)
  # 如果当前随机生成的feature组合与过去重复，则重新生成
  if(j>1){
    # 与以往模型选用的feature进行比较，如果此次选择与以往某次完全相同则重新选择
    while(max(sapply(featureList, function(old) sum(features%in%old)))==10) {
      features = sample(avail_features,10)
    }
  }
  # 保存当前模型选用的feature
  featureList = c(featureList,list(features))
  # 记录每折训练在最终预测集上的预测结果，因为有K折，每次都是对最终测试集全体进行预测，因此会有K个独立预测结果
  tempTestResultMatrix = matrix(0,nrow = nrow(testDF),ncol = foldNum)
  # 记录每折训练在训练集上的预测结果，因为每次都是对1/k数量训练集进行预测，互不重复，因此全体累加之后才能成为完整预测结果。
  predResult = data.table()
  for (i in 1:foldNum) {
    # 根据事先分好的K折划分，选择当前的训练集拆分成的训练集和测试集
    dfList = split.data.frame(trainDF,trainDF$id%in%kvList[[i]])
    test = dfList$`TRUE`
    train = dfList$`FALSE`
    # k折训练
    xgModel = getXgModel(features,labels,train,params)
    # k折预测
    pred = getXgSub(xgModel,test,features,labels)
    # 对k次预测结果整合，最终整合长度与整体训练集相当，
    predResult = rbind(predResult,pred)
    # 对最终测试集的预测，每次长度与最终测试集相当
    finalTestResult = getXgSub(xgModel,testDF,features,labels)
    tempTestResultMatrix[,i] = finalTestResult[,.SD,.SDcols = labels]%>%as.matrix(.)
    print(paste("model ",j,"-fold",i,"finished!"))
  }
  finalTestResultMatrix[,j] = apply(tempTestResultMatrix,MARGIN = 1,mean)
  finalTrainResultMatrix[,j] = copy(predResult)%T>%setorder(.,order_id)%>%.[,.SD,.SDcols = labels]%>%as.matrix(.)
  modelList = c(modelList,list(xgModel))
  print("-----------------------------")
  print(paste("model",j,"finished!"))
}


###################  final predict #########################

append_names = c("m1","m2","m3")
finalTrainDF = copy(trainDF)%T>%
  setorder(.,order_id)%>%
  {
    df = data.table(finalTrainResultMatrix)%T>%
    {
      names(.)<-append_names  
    }
    cbind(.,df)
  }

finalFeatures = c(avail_features,append_names)
finalParams = list("boost"="gbtree",
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

finalTestDF = copy(testDF)%>%
  {
    df = data.table(finalTestResultMatrix)%T>%
    {
      names(.)<-append_names
    }
    cbind(.,df)
  }

finalXgModel = getXgModel(finalFeatures,labels,finalTrainDF,finalParams)
importance_matrix = xgb.importance(model=finalXgModel,feature_names = finalFeatures)
xgb.plot.importance(importance_matrix)
sub = getXgSub(finalXgModel,finalTestDF,finalFeatures,labels)

write.csv(sub,paste0("F:/meituan_Project/20170930 MDD Cup/data/sub/sub_result1109_01.csv"),row.names = F)




