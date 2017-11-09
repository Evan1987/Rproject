library(data.table)
library(bit64)
library(lubridate)

user_log<-fread("F:\\Project\\20161219app登陆数据\\app登陆行为数据（20160901-20161218）.csv")
user_log[,time:=as.POSIXct(round(as.numeric(create_time)/1000,0),origin="1970-01-01")]
user_log<-user_log[userid!='',]

user_log[,":="(hour=hour(time),
               minute=minute(time),
               weekday=weekdays(time),
               day=day(time))]
user_log[,time_dist:=hour+minute/60]
user_log<-user_log[,c(1:3):=NULL]
write.csv(user_log,"F:\\Project\\20161219app登陆数据\\app_open_log(0901-1218).csv",row.names = F)
user_log<- fread("F:\\Project\\20161219app登陆数据\\app_open_log(0901-1218).csv")