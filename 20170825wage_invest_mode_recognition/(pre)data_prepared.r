library(data.table)
library(magrittr)
library(readr)
library(lubridate)

path = "F:/Project/20170825疑似工薪用户投资模式/"

user_info = fread(paste0(path,"user_info.csv"))
brush_user = fread(paste0(path,"brush_users.csv"))
invest_log = read_csv(paste0(path,"invest_log.csv"))%>%
  as.data.table(.)%>%
  .[!userid%in%brush_user$userid,]%T>%
  setorder(.,userid,date)%>%
  .[,":="(month = month(date),day = day(date))]%>%
  .[!month%in%c(2,10),]

investMonthNum = invest_log[,.(monthNum = uniqueN(month)),by=userid]

daySpan = 5
p = daySpan/30
# 最少需要的月份数量
findMinN <- function(p,conf.level=0.99){
  n = log(1-conf.level,base=p)%>%ceiling(.)
  return(n)
}
minN = findMinN(p)
validUsers = investMonthNum[monthNum>=minN,]%>%.$userid
validInvestLog = invest_log[userid%in%validUsers,]
write.csv(validInvestLog,paste0(path,"validInvestLog.csv"),row.names = F)