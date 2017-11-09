library(data.table)
library(timeSeries)
library(lubridate)
library(forecast)
library(magrittr)
library(stringr)
library(sqldf)
library(tcltk)
library(plotrix)
filepath<-"F:\\Project\\20170220投资额度预测\\invest_data.csv"
#去掉异常的影响分布的userid
abnormal_users<-c("3f65d2a472304ce994c7d009160d1362","d3bcc474164a4d11837e6db347b1fa09","bd90572f9e9b43e086f6b5c0866c131c")
investdata<-fread(filepath)%>%
  .[,date:=as.Date(date)]%>%
  .[,weekday:=weekdays(date)]
slimdata<-investdata[!(userid %in% abnormal_users),]%>%
  .[amount>0,]%>%
  .[type=="\\N",type:="newcomer"]%>%
  .[,province:=ifelse(str_count(province)>3|str_count(province)<2,"other",province)]%T>%
  setkey(.,"date")

timeseq = seq.Date(from = as.Date("2016-01-01"),to=as.Date("2016-12-31"),by="day")
num<-length(timeseq)
plotpath<-"F:\\Project\\20170220投资额度预测\\daily_invest\\"
#输出每日的投资金额分布
for(j in 1:num){
  i<-timeseq[j]
  mm<-density(investdata[date==i,]$amount,n=512,to=512*500)
  png(filename=paste0(plotpath,as.character(i),".png"),width =1200,height=468,units ="px")
  plot(mm,type="l",main=i)
  dev.off()
}

#上证指数的分布输出
library(quantmod)
sh <- getSymbols('000001.ss', auto.assign = FALSE)#上证指数的代号是000001.ss
sh<-as.data.table(sh)
colnames(sh)<-c("Date","Open","High","Low","Close","Volume","Adjusted")
sh[,":="(Volume=NULL,Adjusted=NULL,Date=as.Date(Date))]%T>%
  setkey(.,Date)

sh <- getSymbols('000300.ss', auto.assign = FALSE)#沪深300指数的代号是000300.ss
sh<-as.data.table(sh)
colnames(sh)<-c("Date","Open","High","Low","Close","Volume","Adjusted")
sh[,":="(Volume=NULL,Adjusted=NULL,Date=as.Date(Date))]%T>%
  setkey(.,Date)


#分用户类别的输出
type<-unique(slimdata$type)
invest_by_type<-slimdata[,.(num=.N,amount=sum(amount)),by=.(date,type)]%>%
  .[,num:=as.numeric(num)]%T>%
  setkey(.,date)
invest_by_date<-slimdata[,.(num=.N,amount=sum(amount)),by=date]%>%
  .[,num:=as.numeric(num)]%T>%
  setkey(.,date)
invest_by_type<-invest_by_type[invest_by_date,nomatch=NA,mult="all"]%>%
  .[,":="(num_ratio=round(num/i.num,2),amount_ratio=round(amount/i.amount,2))]%>%
  .[,":="(i.num=NULL,i.amount=NULL,amount_per_num=round(amount/num,2))]
for(i in type){
  temp<-invest_by_type[type==i,]
  png(filename=paste0("F:/Project/20170220投资额度预测/invest_by_type/",i,".png"),width =1200,height=468,units ="px")
  twoord.plot(lx=temp$date,rx=temp$date,ly=temp$num,ry=temp$num_ratio,
              type="l",xlab = "Date",ylab = "num",rylab = "num_ratio",
              lwd=2,lcol = "black",rcol = "red",main=paste(i,"-","人数及占总人数比例变化"))
  dev.off()
}

