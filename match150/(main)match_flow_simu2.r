
library(data.table)
library(magrittr)
library(lubridate)
source('~/rstudio/match150/(fun)fun.asset_deadline.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)fun.time_seq.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)fun.redeem_select_asset.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)match_temp2.r', encoding = 'UTF-8', echo=TRUE)
source('~/rstudio/match150/(fun)match_time_flow_evaluation.r', encoding = 'UTF-8', echo=TRUE)
path<-"F:/Project/20170315资产匹配穿透150人调研/时间流模拟/"
n=as.integer(150)

#正常投资、赎回的处理权重
fun.invest<-function(unmatched_premium,log_time,now_time,remark){
  if(is.null(unmatched_premium)){
    weight=0
  }else{
    remark_weight<-ifelse(is.na(remark),0,ifelse(remark=="out_asset",0.1,ifelse(remark=="extra",0.2,0)))
    
    time_weight=difftime(as.POSIXct(now_time),as.POSIXct(log_time),units = "days")%>%as.numeric(.)
    weight=log10(unmatched_premium+1)*10+time_weight+remark_weight
  }
  return(weight)
}

#进行资产匹配时，影响资产匹配队列顺序的权重计算
fun.asset_weight<-function(unmatched_amount,redeem_time,now_time,deadline,issettled){
  now_time<-as.POSIXct(now_time)
  deadline<-as.POSIXct(deadline)
  time_weight=ifelse(is.na(redeem_time),0,
                     difftime(now_time,as.POSIXct(redeem_time),units = "days")%>%as.numeric(.))
  deadline_weight=ifelse(issettled,0,difftime(deadline,now_time,units = "days")%>%as.numeric(.)%>%{1/exp(.)})
  weight=log10(unmatched_amount+1)*10+time_weight+deadline_weight
  return(weight)
}

#重要参数对照
fun.type_name<-function(type){
  name<-switch(type+1,
               "current",
               "tPlus",
               "regular")
  return(name)
}

############################## 0.模拟数据源加载 #################################
# 0.0时间流序列时间轴
Dseq<-seq.Date(from = as.Date("2017-03-15"),to = as.Date("2017-03-22"),by = "day")
HMSseq<-7:22
time_seq<-fun.time_seq(Dseq = Dseq,HMSseq = HMSseq)%>%c(.,as.POSIXct("2017-03-15 00:00:00"))%>%sort(.)

# 0.1匹配起点资产状态
asset_initial<-fread(paste0(path,"asset_list.csv"))
# 0.2匹配起点资产匹配信息
match_record_initial<-fread(paste0(path,"match_record.csv"))%>%
  .[,log_time:=as.POSIXct("2017-03-14 23:59:59")]

#资产整体数据
asset<-fread(paste0(path,"asset_simu_data.csv"))%>%
  .[,":="(create_time=as.POSIXct(create_time),
          end_time=as.POSIXct(end_time))]%T>%
  setnames(.,"corpusamount","amount")


# 0.3匹配起点用户资金序列状态
user_list<-fread(paste0(path,"user_list.csv"))%>%
  .[,log_time:=as.POSIXct("2017-03-14 23:59:59")]

regular<-user_list[,c("userid","unmatched_regular_premium","log_time")][
  unmatched_regular_premium>0,][
    ,":="(unmatched_premium=unmatched_regular_premium,
          unmatched_regular_premium=NULL)]

tPlus<-user_list[,c("userid","unmatched_tPlus_premium","log_time")][
  unmatched_tPlus_premium>0,][
    ,":="(unmatched_premium=unmatched_tPlus_premium,
          unmatched_tPlus_premium=NULL)]

current<-user_list[,c("userid","unmatched_current_premium","log_time")][
  unmatched_current_premium>0,][
    ,":="(unmatched_premium=unmatched_current_premium,
          unmatched_current_premium=NULL)]

# 0.3资产入库时间流整体
in_asset<-copy(asset)%>%
  .[create_time>=as.POSIXct("2017-03-15")&amount>0,]%>%
  .[,deadline:=sapply(create_time,fun.asset_deadline)%>%as.POSIXct(.,origin="1970-01-01 00:00:00")]%>%
  .[,":="(unmatched_amount=amount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]%>%
  .[,":="(isredeemed=0,redeem_time=as.POSIXct(NA),issettled=0,settled_time=as.POSIXct(NA))]
# 0.4资产出库时间流整体
out_asset<-copy(asset)%>%
  .[end_time<=as.POSIXct("2017-03-22")&amount>0,]
# 0.5投资时间流整体
invest<-fread(paste0(path,"invest_simu_data.csv"))%>%
  .[,":="(create_time=as.POSIXct(create_time))]
# 0.6赎回时间流整体
redeem<-fread(paste0(path,"redeem_simu_data.csv"))%>%
  .[,":="(create_time=as.POSIXct(create_time))]

# 0.7循环初始值
#----当前匹配信息表
match_status_now<-copy(match_record_initial)
#----当前资产信息表
asset_now<-asset[asset_initial,on=c("id"="id"),nomatch=0,mult="all"][,c("i.amount"):=NULL]%>%
  .[,deadline:=sapply(create_time,FUN = fun.asset_deadline)%>%as.POSIXct(.,origin="1970-01-01 00:00:00")]%>%
  .[end_time>=as.POSIXct("2017-03-14 23:59:59"),]%>%
  .[,":="(isredeemed=0,
          redeem_time=as.POSIXct(NA),
          issettled=1,
          settled_time=create_time,
          avg_avail_amount=as.double(avg_avail_amount),
          unmatched_amount=as.double(unmatched_amount),
          amount=as.double(amount))]
#----当前用户信息表
user_list_now<-copy(user_list)

#----当前定期序列
regular_now<-copy(regular)%>%.[,":="(remark=NA)]
#----当前T+N序列
tPlus_now<-copy(tPlus)%>%.[,":="(remark=NA)]
#----当前活期序列
current_now<-copy(current)%>%.[,":="(remark=NA)]
#----赎回记录总表，先创建空的
redeem_record<-data.table(id=0,
                          userid="0",
                          create_time=as.POSIXct("2017-03-23 0:00:00"),
                          amount=1000,
                          type=1,
                          status=1,
                          update_time=as.POSIXct("2017-03-24 0:00:00"))%>%.[-1,]
#----赎回日志总表，先创建空的
redeem_log<-data.table(redeemID=0,
                       free_redeem_amount=5.00,
                       matched_redeem_amount=1000)%>%.[-1,]

redeem_asset<-data.table()

############################## 1.时间流验证 #################################
asset_status_log<-data.table()
redeem_asset_log<-data.table()
redeem_asset_snap_log<-data.table()
match_status_log<-data.table()
redeem_record_log<-data.table()
redeem_status_log<-data.table()
asset_now_log<-data.table()
end_for=length(time_seq)-1
for(index in 1:end_for){
  result<-match_time_flow_evaluation(time_seq = time_seq,
                                     index = index,
                                     in_asset = in_asset,
                                     out_asset = out_asset,
                                     invest = invest,
                                     redeem = redeem,
                                     redeem_record = redeem_record,
                                     redeem_log = redeem_log,
                                     redeem_asset=redeem_asset,
                                     asset_now = asset_now,
                                     match_status_now = match_status_now,
                                     regular_now = regular_now,
                                     tPlus_now = tPlus_now,
                                     current_now = current_now,
                                     n = n)
  
  redeem_record<-result$redeem_record
  redeem_log<-result$redeem_log
  redeem_asset<-result$redeem_asset
  asset_now<-result$asset_now
  match_status_now<-result$match_status_now
  regular_now<-result$regular_now
  tPlus_now<-result$tPlus_now
  current_now<-result$current_now
  redeem_status<-result$redeem_status
  redeem_asset_snap<-result$redeem_asset_snap
  
  asset_status_log<-rbind(asset_status_log,copy(result$asset_status_summary)[,label:=index])
  redeem_asset_log<-rbind(redeem_asset_log,copy(redeem_asset)[,label:=index])
  redeem_record_log<-rbind(redeem_record_log,copy(redeem_record)[,label:=index])
  asset_now_log<-rbind(asset_now_log,copy(asset_now)[,label:=index])
  redeem_status_log<-rbind(redeem_status_log,copy(redeem_status)[,label:=index])
  redeem_asset_snap_log<-rbind(redeem_asset_snap_log,copy(redeem_asset_snap)[,label:=index],fill=T)
}




