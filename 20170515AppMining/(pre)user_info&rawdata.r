library(data.table)
library(magrittr)
library(readr)
library(stringr)
source('~/rstudio/!custom/(fun)iwrite.r')
path = "F:/Project/20170515用户app访问数据/"

# total_user_info 2016-09-01===2017-05-01 reg users
user_info<-fread(paste0(path,"user_info.csv"))%>%
{
  vars<-names(.)%>%.[which(str_detect(.,"_"))]
  .[,(vars):=lapply(.SD,function(x) replace(x,x=="null",NA)),.SDcols=vars]
}%>%
{
  timevars<-names(.)%>%.[which(str_detect(.,"time"))]
  .[,(timevars):=lapply(.SD,as.POSIXct),.SDcols=timevars]
}


iwrite(user_info,path)

# partial users 2017-02-01===2017-05-01
my_user_info<-read_csv(paste0(path,"user_info.csv"))%>%
  as.data.table(.)%>%
  .[reg_time>=as.POSIXct("2017-02-01 0:00:00"),]%>%
  .[,isinvited:=ifelse(is.na(invite_user_id),0,1)]%>%
  .[,-"invite_user_id"]

iwrite(my_user_info,path)


# the app log of partial users above
rawdata<-fread(paste0(path,"rawdata.csv"))%>%
  .[occurrencetime>=1e+12,]%>%
  .[,log_time:=as.POSIXct(occurrencetime/1000,origin="1970-01-01")]%>%
  .[,-"occurrencetime"]%T>%
  setkey(.,userid,log_time)

write.csv(rawdata,paste0(path,"rawdata_treated.csv"))

# the slim data between one's reg_time and invest1st_time
slimdata<-read_csv(paste0(path,"rawdata_treated.csv"))%>%
  as.data.table(.)%T>%
  setkey(.,userid,log_time)%>%
  .[my_user_info[,c("userid","reg_time","invest1st_time")],on="userid",nomatch=0]%>%
  .[log_time>=reg_time&(log_time<=invest1st_time|is.na(invest1st_time)),]%>%
  .[,-c("reg_time","invest1st_time")]%>%
  .[module!="open",]%>%
  .[,stay_time:=c(NA,as.numeric(diff(log_time)))%>%dplyr::lead(.,default = 120),by=userid]%>%
  .[,id:=seq_len(.N),by=userid]%>%
  .[activitytype=="exit002"|stay_time>=120,isEnd:=1]%>%{
    y<-.[,.SD[CJ(id=c(.SD[isEnd==1,]$id+1,1)),on="id",nomatch=0],by=userid]
    .[y,on=c("userid","id"),isStart:=1]
  }%>%
  .[,isStart:=replace(isStart,is.na(isStart),0)]%>%
  .[,sessionID:=cumsum(isStart),by=userid]%>%
  .[!(isStart==1&isEnd==1&module=="exit"),]

# slimdata2[,.(num=.N,exitn=n_distinct(module[which(module=="exit")])),by=.(userid,sessionID)][num==1&exitn==1,]
iwrite(slimdata,path)


# 计算最后访问页的预估停留时间
slimdata<-read_csv(paste0(path,"slimdata2.csv"))%>%as.data.table(.)
tmp<-slimdata[is.na(isEnd)&stay_time>0,]
time_exp<-median(tmp$stay_time)

num=nrow(tmp)
result<-data.table()
for(i in seq(1,60,1)){
  ratio=nrow(tmp[stay_time<=i,])/num
  temp<-data.table(time=i,ratio=ratio)
  result<-rbind(result,temp)
}

fun.elbow_point(result$time,result$ratio,doplot=T)
# quantile(slimdata$stay_time,probs=seq(0,1,0.05))




