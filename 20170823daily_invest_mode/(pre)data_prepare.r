library(data.table)
library(magrittr)
library(readr)

path = "F:/Project/20170823日投资金额组成模式/"

invest_record = read_csv(paste0(path,"invest_record.csv"))%>%
  as.data.table(.)%T>%
  setorder(.,create_time)%>%
  .[,":="(log_day=as.Date(create_time),hms = hour(create_time)+round(minute(create_time)/60,2))]


redeem_record = read_csv(paste0(path,"redeem_record.csv"))%>%
  as.data.table(.)%T>%
  setorder(.,create_time)%>%
  .[,":="(log_day=as.Date(create_time),hms = hour(create_time)+round(minute(create_time)/60,2))]

write.csv(invest_record,paste0(path,"invest_record.csv"),row.names = F)
write.csv(redeem_record,paste0(path,"redeem_record.csv"),row.names = F)


invest_summary = invest_record[,.(amount=sum(amount)),by=.(log_day,userid)]
redeem_summary = redeem_record[,.(amount= -sum(amount)),by=.(log_day,userid)]
user_premium = fread(paste0(path,"user_premium.csv"))%>%
  .[log_day==20170228,]%>%
  .[,log_day2:=as.Date("2017-02-28")]%>%
  .[,c("log_day2","userid","premium_final")]%T>%
  setnames(.,c("log_day2","premium_final"),c("log_day","amount"))


premium_record<-rbindlist(list(invest_summary,redeem_summary,user_premium),fill = T)%>%
  setorder(.,userid,log_day)%>%
  .[,premium:=cumsum(amount),by=userid]%>%
  .[,premium:=pmax(premium,0)]%>%
  .[,c("log_day","userid","premium")]

write.csv(premium_record,paste0(path,"premium_record.csv"),row.names = F)
