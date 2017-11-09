library(data.table)
library(magrittr)
library(readr)
library(lubridate)
library(plotrix)
path = "F:/Project/20170609日存量增量预测/data/"

rawdata<-read_csv(paste0(path,"invest_record.csv"))%>%as.data.table(.)
rawdata[,log_day:=date(create_time)]
invest_data<-rawdata[,.(amount=sum(amount)),by=.(userid,log_day)]%T>%
  setkey(.,userid,log_day)%>%
  .[,index:=seq_along(log_day),by=userid]

invest_summary<-invest_data[,.(total_amount=sum(amount)),by=log_day]
invest_summary_by_index<-invest_data[,.(amount=sum(amount),num=.N),by=.(log_day,index)]%T>%
  .[invest_summary,on="log_day",ratio:=amount/i.total_amount]%>%
  setorder(.,log_day,-ratio)%>%
  .[,cum_ratio:=cumsum(ratio),by=log_day]


index = unique(invest_summary_by_index$index)
library(animation)
for(j in 1:max(index)){
  i=index[which(index<=j)]
  
  temp<-invest_summary_by_index[index%in%i,]%>%
    .[,.(total_ratio=sum(ratio),total_amount=sum(amount)),by=log_day]%>%
    setorder(.,log_day)
  
  png(filename=paste0(path,"index below",j,".png"),width = 1200,height = 800)
  twoord.plot(lx=temp$log_day,
              rx=temp$log_day,
              ly=temp$total_ratio,
              ry=temp$total_amount,
              type="l")
  dev.off()
}

