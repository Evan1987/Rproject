library(data.table)
library(magrittr)
source('~/rstudio/20170928Coupon_kingkong/(funList)funlist.r', encoding = 'UTF-8', echo=TRUE)
path = "F:/meituan_Project/20170928金刚位复购优惠券推荐/"

# 0.生成用于衡量推荐性能的ActualCondition数据
sample_data = fread(paste0(path,"sample_data.csv"))
trainData = fread(paste0(path,"train.txt"))

validTrainData = trainData[mobile%in%sample_data$mobile,]
train_users = unique(validTrainData$mobile)

validTrainDataSummary = validTrainData[,.(cnt=sum(total_order_cnt)),by=.(mobile,tenant_id)]
sampleDataSummary = sample_data[mobile%in%train_users,]%>%
  .[,.(cnt=sum(total_order_cnt)),by=.(mobile,tenant_id)]

actualCondition = sampleDataSummary[validTrainDataSummary,on=c("mobile","tenant_id"),formerCnt:=i.cnt]%>%
  .[,formerCnt:=replace(formerCnt,is.na(formerCnt),0)]%>%
  .[,changeCnt:=cnt-formerCnt]%>%
  .[changeCnt>0,]

write.csv(validTrainData,paste0(path,"validTrainData.csv"),row.names = F)
write.csv(actualCondition,paste0(path,"actualCondition.csv"),row.names = F)

rm(list=ls()%>%.[!.%in%c("path","validTrainData","train_users","actualCondition")])
gc()

# 1. 生成UserProfile和ItemProfile

sample_prefer = copy(validTrainData)%>%
  .[,score:=preferEval(m_0_1_order_cnt,
                       m_0_1_order_amt,
                       m_1_3_order_cnt,
                       m_1_3_order_amt,
                       m_3_6_order_cnt,
                       m_3_6_order_amt,
                       m_6_12_order_cnt,
                       m_6_12_order_amt,
                       total_order_cnt,
                       total_order_amt)]%>%
  .[,c("mobile","tenant_id","poi_id","score")]%>%
  setorder(.,mobile,-score)

poi_info = fread(paste0(path,"poi_info.txt"))%>%
{
  vars = names(.)
  .[,(vars):=lapply(.SD,function(x) as.integer(x)),.SDcols=vars]
}%>%
  .[,cityid:=replace(cityid,cityid<0,NA)]


user_birthday = fread(paste0(path,"user_birthday.txt"))%>%
  .[,birthday:=as.Date(birthday)]%>%
  .[!is.na(birthday),":="(thisYearBirthday=replaceYear(birthday,year(Sys.time())),nextYearBirthday=replaceYear(birthday,year(Sys.time())+1))]

userProfile = userProfileGenerator(sample_prefer,poi_info)%>%c(.,list("user_birthday"=user_birthday))

itemProfile = fread(paste0(path,"couponListSim.csv"))%>%
  .[poi_info[,-c("tenant_id")],on="poi_id",":="(lat=i.latitude/1e+6,lng=i.longitude/1e+6,barea_id=i.barea_id,city_id=i.cityid)]%>%
  .[,city_id:=as.integer(city_id)]%>%
  {
    poi_summary = poi_info[,.(num=uniqueN(poi_id)),by=tenant_id]
    .[poi_summary,on="tenant_id",num:=i.num]
  }%>%
  setnames(.,"id","coupon_id")

## 2. Do Recommend

### some identification:
#### coupon_type = 0:7
#### 0: birthday
#### 1: waimai
#### 2: highF
#### 3: mediumF
#### 4: lowF
#### 5: active
#### 6: silent
#### 7: drained
#### priority when conflict: 0>5>2>1>6>7>3>4

priorFun<-mapGenerator(order=c(0,5,2,1,6,7,3,4))
itemProfile[,priorLevel:=sapply(coupon_type,priorFun)]

# ///main
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)
conn = MySQL_conn_select("local",db_name = "meituan")
users = unique(sample_prefer$mobile)
user_num = length(users)
pb <- tkProgressBar("进度","已完成 %", 0, 100)
for(i in 1:user_num){
  user = users[i]
  user_recommend = recommendFun(user,
                                userProfile = userProfile,
                                itemProfile = itemProfile,
                                max_dist=100,
                                num=50)%>%
    .[,mobile:=user]
  
  tryCatch({dbWriteTable(conn,"coupon_recommend",user_recommend,append=T,row.names=F)},
           error=function(e){
             cat("error occured", conditionMessage(e),"\n\n")
             conn = MySQL_conn_select("local",db_name = "meituan")
             dbWriteTable(conn,"coupon_recommend",user_recommend,append=T,row.names=F)
             return("error")
           },finally = {
             info<- sprintf("已完成 %.4f%%", round(i*100/user_num,4))  
             setTkProgressBar(pb, i*100/user_num, sprintf("进度 (%s)", info),info)
           })
}

z0<-"select mobile,tenant_id,rank from coupon_recommend"
res<-dbSendQuery(conn,z0)
recommendResult <- dbFetch(res, n = -1) %>% as.data.table(.)
dbClearResult(res)
dbDisconnect(conn)

validActualCondition = actualCondition[tenant_id%in%itemProfile$tenant_id,]
recallResult<-copy(validActualCondition)%>%
  .[recommendResult,on=c("mobile","tenant_id"),rank:=i.rank]%>%
  .[unique(itemProfile[,c("tenant_id","num")]),on="tenant_id",num:=i.num]

# 整体Recall
nrow(recallResult[!is.na(rank)])/nrow(recallResult)
# 复购Recall
nrow(recallResult[formerCnt>0&(!is.na(rank)),])/nrow(recallResult[formerCnt>0])
# 在规则范围内（补充推荐只能是num>=5的tenant）的Recall
nrow(recallResult[!is.na(rank) & (formerCnt>0|(num>=5 & formerCnt==0))])/nrow(recallResult[formerCnt>0|(num>=5 & formerCnt==0)])



# distEval<-function(user,target_tenant_id,itemProfile,userProfile){
#   coord_prefer = userProfile$coord_prefer[mobile%in%user,]
#   distStatus = itemProfile[tenant_id==target_tenant_id,]%>%
#     .[,dist:=ManhattanDist(lng,lat,coord_prefer$lng,coord_prefer$lat)]
#   return(min(distStatus$dist))
# }
# 
# 
# missResult = recallResult[is.na(rank),]%>%
#   .[,dist:=distEval(mobile,tenant_id,itemProfile,userProfile),by=.(mobile,tenant_id)]






