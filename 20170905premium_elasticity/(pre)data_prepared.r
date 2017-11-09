library(data.table)
library(magrittr)
library(readr)
library(lubridate)

path = "F:/Project/20170905用户存量弹性评价/"
brush_users = fread(paste0(path,"brush_users.csv"))
user_info = fread(paste0(path,"user_info.csv"))
invest_log = read_csv(paste0(path,"invest_log.csv"))%>%
  as.data.table(.)%>%
  .[(!userid%in%brush_users$userid)&(userid%in%user_info$userid),]

redeem_log = read_csv(paste0(path,"redeem_log.csv"))%>%
  as.data.table(.)%>%
  .[(!userid%in%brush_users$userid)&(userid%in%user_info$userid),]%>%
  .[,amount:= -amount]

cash_log = rbind(invest_log,redeem_log)%T>%
  setorder(.,userid,date)%>%
  .[date<=as.Date("2017-07-31"),]

write.csv(cash_log,paste0(path,"cash_log.csv"),row.names = F)
