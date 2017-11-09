library(data.table)
library(magrittr)
library(readr)
library(dplyr)
library(lubridate)
source('~/rstudio/!custom/(fun)colname_treat.R')

path<-"F:/Project/20170419用户流失风险预警模型/invest1st_to_leave/rawdata/"

# user_premium_log treatment
user_premium_log<-fread(paste0(path,"user_premium_log.csv"))%>%
  # .[,log_day:=as.Date(as.character(log_day),"%Y%m%d")]%T>%
  .[,log_day:=as.Date(log_day)]%T>%
  setkey(.,userid,log_day)%>%
  # 为缺失的日期padding with 0
  .[,.SD[CJ(log_day=seq.Date(min(log_day),max(log_day),by="days")),on=c("log_day"="log_day")],by=userid]%>%
  {.[,3:ncol(.)][is.na(.[,3:ncol(.)])]<-0}

user_premium_log[,":="(regular_ratio=premium_regular/(premium_final+0.01))]%>%
  .[,":="(premium_change=c(0,diff(premium_final)),regular_change=c(0,diff(premium_regular))),by=userid]%>%
  .[,":="(premium_change_ratio=(premium_change/(premium_final-premium_change+0.01))%>%round(.,2),
          regular_change_ratio=(regular_change/(premium_regular-regular_change+0.01))%>%round(.,2)),by=userid]

fwrite(user_premium_log,file=paste0(path,"user_premium_log2.csv"),dateTimeAs = "write.csv")

# user_app_log treatment
user_app_log<-fread(paste0(path,"user_app_log.csv"))%>%
  .[,part_log_day:=as.Date(part_log_day)]%T>%
  setorder(.,userid,part_log_day)%>%
  .[,diffdate:=c(0,diff.Date(part_log_day)),by=userid]
user_summary<-user_app_log[,.(last_visit_day=max(part_log_day)),by=userid]

write.csv(user_summary,paste0(path,"user_app_summary.csv"),row.names = F)
fwrite(user_app_log,file=paste0(path,"user_app_log.csv"),dateTimeAs = "write.csv")

# user_info treatment
user_info<-fread(paste0(path,"user_info.csv"))%>%
{colname_treat(.)}%>%
  .[,lapply(.SD,function(x) replace(x,which(x=="null"|x=="NULL"),NA))]
write.csv(user_info,paste0(path,"user_info.csv"),row.names = F)

# invest_info treatment
invest_info<-read_csv(paste0(path,"invest_info.csv"))%>%
  mutate(.,create_time=as.POSIXct(create_time))%>%
  mutate(.,log_day=date(create_time))

write.csv(invest_info,paste0(path,"invest_info.csv"),row.names = F)


# redeem_info treatment
redeem_info<-read_csv(paste0(path,"redeem_info.csv"))%>%
  mutate(.,create_time=as.POSIXct(create_time))%>%
  mutate(.,log_day=date(create_time))

write.csv(redeem_info,paste0(path,"redeem_info.csv"),row.names = F)


# regular_info treatment
regular_info<-read_csv(paste0(path,"regular_info.csv"))%>%
  mutate(.,create_time=as.POSIXct(create_time))%>%
  filter(.,!is.na(amount)&amount>=1000)%>%
  mutate(.,log_day=date(create_time))

write.csv(regular_info,paste0(path,"regular_info.csv"),row.names = F)


# user_life_cycle treatment
user_life_cycle<-fread(paste0(path,"user_life_cycle.csv"))%>%
  {colname_treat(.)}%>%
  .[,lapply(.SD,function(x){replace(x,which(x=="NULL"|x=="null"),NA)})]%>%
  .[,names(.)[-1]:=lapply(.SD,as.Date),.SDcols=-names(.)[1]]

write.csv(user_life_cycle,paste0(path,"user_life_cycle.csv"),row.names = F)

