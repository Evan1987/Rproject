library(data.table)
library(sqldf)
library(magrittr)
library(lubridate)
source('~/rstudio/money_retain/(fun)redeem_cycle.R')
source('~/rstudio/(fun)mysqlconn.R')
conn<-mysqlconn("mysql_settings.csv")

##### Part1. Create raw dataset.
z_invest<-"select date,userid,amount as invest_amount from invest_record where partitiontime>=20160901"
z_redeem<-"select date,userid,amount as redeem_amount from cash_record where partitiontime<=20160901"
invest_record<-as.data.table(sqlQuery(conn,z_invest))
redeem_record<-as.data.table(sqlQuery(conn,z_redeem))
setkey(invest_record,userid)
setkey(redeem_record,userid)

z_users<-"select userid,invest1st_time from user_info where invest1st_time>='2016-09-01'"
user_info<-as.data.table(sqlQuery(conn,z_users))
users<-data.table(userid=user_info$userid)

# Get effective data of target users
invest_log<-invest_record[users,nomatch=0]
redeem_log<-redeem_record[users,nomatch=0]

# Delete userless variables to save memory
rm(list = c("invest_record","redeem_record"))
rm(list = c("z_invest","z_redeem","z_users"))

write.csv(user_info,"F:/Project/20170109赎回资金周期/invest_log.csv",row.names = F)
write.csv(invest_log,"F:/Project/20170109赎回资金周期/redeem_log.csv",row.names = F)
write.csv(redeem_log,"F:/Project/20170109赎回资金周期/user_info.csv",row.names = F)


##### Part2. Do the Calculation
invest_log<-fread("F:/Project/20170109赎回资金周期/invest_log.csv")%>%
  .[,.(invest_amount=sum(invest_amount)),by=.(date,userid)]%>%
  .[,date:=as.Date(date)]%T>%
  setkey(.,userid)

redeem_log<-fread("F:/Project/20170109赎回资金周期/redeem_log.csv")%>%
  .[,.(redeem_amount=sum(redeem_amount)),by=.(date,userid)]%>%
  .[,date:=as.Date(date)]%T>%
  setkey(.,userid)

user_info<-fread("F:/Project/20170109赎回资金周期/user_info.csv")%>%
  .[,invest1st_day:=date(invest1st_time)]

user_num<-dim(user_info)[1]

user_cash_log<-data.table()
for(i in 1:user_num){
  i_id<-user_info$userid[i]

  ##### Create individual invest&redeem log for next calc  ####
 
  i_invest<-invest_log[i_id,nomatch=0,mult="all"]
  i_redeem<-redeem_log[i_id,nomatch=0,mult="all"]
  i_result<-redeem_cycle(i_invest,i_redeem)
  i_result<-data.table(userid=i_id,i_result)
  user_cash_log<-rbind(user_cash_log,i_result)
  print(paste(i,"/",user_num,"finished!"))
}