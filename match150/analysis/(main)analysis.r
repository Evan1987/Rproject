library(data.table)
library(magrittr)
library(lubridate)
library(readr)
source('~/rstudio/match150/analysis/(fun)invest_match_eff.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)fun.time_seq.r')

path<-"F:/Project/20170315资产匹配穿透150人调研/时间流模拟0410/"
time_seq<-fun.time_seq(Dseq = seq.Date(from = as.Date("2017-03-15"),to = as.Date("2017-03-22"),by = "day"),
                       HMSseq = 7:22)%>%c(.,as.POSIXct("2017-03-15 00:00:00"))%>%sort(.)

redeem_record<-read_csv(paste0(path,"result/redeem_record.csv"))%>%as.data.table(.)%>%
  .[,":="(create_time=create_time+8*3600,update_time=update_time+8*3600)]

asset_now<-read_csv(paste0(path,"result/asset_now_log.csv"))%>%as.data.table(.)%>%.[label==128,]%>%
  .[,":="(create_time=create_time+8*3600,end_time=end_time+8*3600,deadline=deadline+8*3600,settled_time=settled_time+8*3600)]
in_asset<-asset_now[date(create_time)>=as.Date("2017-03-15")&date(create_time)<=as.Date("2017-03-22"),]%>%
  .[,difftime:=difftime(settled_time,create_time,units = "hours")%>%as.numeric(.)%>%round(.,2)]

user_list<-fread(paste0(path,"user_list.csv"))%>%
  .[,-c("premium","regular_premium","tPlus_premium")]

regular_now_log<-fread(paste0(path,"result/regular_now_log.csv"))%>%
  .[unmatched_premium>0,]

tPlus_now_log<-readr(paste0(path,"result/tPlus_now_log.csv"))%>%
  as.data.table(.)%>%
  .[unmatched_premium>0,]%>%
  .[,create_time:=create_time+8*3600]

current_now_log<-fread(paste0(path,"result/current_now_log.csv"))%>%.[unmatched_premium>0,]

invest_simu_data<-fread(paste0(path,"invest_simu_data.csv"))

regular_extra_invest<-user_list[,c("userid","unmatched_regular_premium")]%>%
  setnames(.,"unmatched_regular_premium","amount")%>%
  .[amount>0,]%>%
  .[,":="(type=2,create_time=time_seq[1],amount=as.double(amount))]

tPlus_extra_invest<-user_list[,c("userid","unmatched_tPlus_premium")]%>%
  setnames(.,"unmatched_tPlus_premium","amount")%>%
  .[amount>0,]%>%
  .[,":="(type=2,create_time=time_seq[1],amount=as.double(amount))]

regular_invest_result<-invest_match_eff(time_seq = time_seq,
                                        cash_flow_log = regular_now_log,
                                        invest_simu_data = invest_simu_data,
                                        origin_invest = regular_extra_invest,
                                        item = "regular")

tPlus_invest_result<-invest_match_eff(time_seq = time_seq,
                                      cash_flow_log = tPlus_now_log,
                                      origin_invest = tPlus_extra_invest,
                                      invest_simu_data = invest_simu_data,
                                      item = "tPlus")

# current_invest_result<-invest_match_eff(time_seq = time_seq,
#                                       cash_flow_log = current_now_log,
#                                       invest_simu_data = invest_simu_data,
#                                       item = "current")


hist(regular_invest_result$difftime,main="定期投资匹配时长分布",xlab="时长(h)",labels=T,breaks = 0:10)
hist(regular_invest_result[difftime>1,]$difftime,main="",xlab="",labels = T,breaks =0:10)
hist(regular_invest_result[hour(create_time)>=22|hour(create_time)<7,]$difftime,labels = T,breaks=0:10,col="dark blue",
     main="夜间22:00-7:00定期投资的匹配时长分布",
     xlab="时长(h)")
hist(regular_invest_result[hour(create_time)>=7&hour(create_time)<22,]$difftime,labels = T,breaks=0:10,col="light blue",
     main="日间7:00-22:00定期投资的匹配时长分布",
     xlab="时长(h)")

hist(tPlus_invest_result$difftime,main="T+N投资匹配时长分布",xlab="时长(h)",labels=T,breaks = 0:10)
hist(tPlus_invest_result[difftime>1,]$difftime,main="",xlab="",labels = T,breaks = 0:10)
hist(tPlus_invest_result[hour(create_time)>=22|hour(create_time)<7,]$difftime,labels = T,breaks=0:10,col="dark blue",
     main="夜间22:00-7:00T+N投资的匹配时长分布",
     xlab="时长(h)")
hist(tPlus_invest_result[hour(create_time)>=7&hour(create_time)<22,]$difftime,labels = T,breaks=0:10,col="light blue",
     main="日间7:00-22:00T+N投资的匹配时长分布",
     xlab="时长(h)")


hist(redeem_record$difftime,main="赎回处理时长分布",xlab = "时长(h)",labels = T,breaks = 0:12)
hist(redeem_record[difftime>1,]$difftime,main="",xlab = "",labels = T,breaks = 0:12)
hist(redeem_record[hour(create_time)>=22|hour(create_time)<7,]$difftime,labels = T,breaks=0:12,col="dark blue",
     main="夜间22:00-7:00赎回的处理时长分布",
     xlab="时长(h)")
hist(redeem_record[hour(create_time)>=7&hour(create_time)<22,]$difftime,labels = T,breaks=0:12,col="light blue",
     main="日间7:00-22:00赎回的处理时长分布",
     xlab="时长(h)")


hist(in_asset$difftime,labels = T,main="新资产完全匹配时长分布",xlab="时长(h)")









