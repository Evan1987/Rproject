library(data.table)
library(magrittr)
library(readr)
library(dplyr)
source('~/rstudio/!custom/(fun)fun.elbow_point.r', encoding = 'UTF-8')

path<-"F:/Project/20170419用户流失风险预警模型/invest1st_to_leave/"

user_info<-read_csv(paste0(path,"rawdata/user_info.csv"))%>%as.data.table(.)
user_app_log<-read_csv(paste0(path,"rawdata/user_app_log.csv"))%>%as.data.table(.)

user_leave_days_summary<-user_app_log[diffdate<=30,][,.(max_retain_days=max(diffdate)),by=userid]%>%
  .[user_info[,c("userid","isleave")],on="userid"]

N_0=nrow(user_leave_days_summary[isleave==0,])
result<-data.table()
for(i in 1:30){
  fpr = nrow(user_leave_days_summary[isleave==0,][max_retain_days>i,])/N_0
  temp<-data.table(wait_time=i,fpr=fpr)
  result<-rbind(result,temp)
}
elbow_point=fun.elbow_point(result$wait_time,result$fpr,doplot = T)

write.csv(result,paste0(path,"wait_days_threhold.csv"),row.names = F)


#超过20天未访问就判定为疑似流失的话，有10%概率会将未流失用户判断错。