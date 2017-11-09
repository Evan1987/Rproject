library(data.table)
library(magrittr)
library(readr)
path = "F:/Project/20170816用户加息计划潜力/"
elim_users<-fread(paste0(path,"brush_users.csv"))
target_users<-fread(paste0(path,"user_info.csv"))
invest_log<-read_csv(paste0(path,"invest_log.csv"))%>%
  as.data.table(.)%>%
  .[(!userid%in%elim_users$userid)&(userid%in%target_users$userid),]%>%
  setnames(.,"invest_amount","amount")

redeem_log<-read_csv(paste0(path,"redeem_log.csv"))%>%
  as.data.table(.)%>%
  .[(!userid%in%elim_users$userid)&(userid%in%target_users$userid),]%>%
  setnames(.,"redeem_amount","amount")


write.csv(invest_log,paste0(path,"elim_invest_log.csv"),row.names = F)
write.csv(redeem_log,paste0(path,"elim_redeem_log.csv"),row.names = F)
