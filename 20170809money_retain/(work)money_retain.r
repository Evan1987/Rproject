library(data.table)
library(magrittr)
library(tcltk)
source('~/rstudio/20170809money_retain/(fun)redeem_cycle.R')

##### Part1. Create raw dataset
##### Part2. Do the Calculation
invest_log<-fread("F:/Project/20170809赎回周期再次计算/invest_log.csv")%>%
  .[,date:=as.Date(date)]%T>%
  setkey(.,userid,date)

redeem_log<-fread("F:/Project/20170809赎回周期再次计算/redeem_log.csv")%>%
  .[,date:=as.Date(date)]%T>%
  setkey(.,userid,date)

user_info<-fread("F:/Project/20170809赎回周期再次计算/user_info.csv")
user_num<-dim(user_info)[1]
users = user_info$userid
user_cash_log<-data.table()

pb <- tkProgressBar("进度","已完成 %", 0, 100)
for(i in 1:user_num){
  i_id<-users[i]
  ##### Create individual invest&redeem log for next calc  ####
  i_invest<-invest_log[userid==i_id,]
  i_redeem<-redeem_log[userid==i_id,]
  redeem_num = nrow(i_redeem)
  if(redeem_num==0){
    next
  }
  for(j in 1:redeem_num){
    j_redeem = i_redeem[j,]
    redeem_day = j_redeem$date
    redeem_amount = j_redeem$redeem_amount
    i_invest[date<=redeem_day,":="(cycle_days=as.numeric(redeem_day-date),cumamount=cumsum(invest_amount))]%>%
      .[,rest_amount:=round(cumamount-redeem_amount,2)]%>%
      .[,amount:=round(invest_amount-ifelse(rest_amount<0,0,rest_amount),2)]%>%
      .[,invest_amount:=invest_amount-ifelse(amount<0|is.na(amount),0,amount)]
    days = {
      x<-i_invest[amount>0,]
      ifelse(nrow(x)>0,sum(x$cycle_days*x$amount)/sum(x$amount),9999)
    }
    i_redeem[j,cycle_days:=round(days,2)]
    i_invest<-i_invest[invest_amount>0,]
  }
  user_cash_log<-rbind(user_cash_log,i_redeem)
  info<- sprintf("已完成 %.4f%%", round(i*100/user_num,4))  
  setTkProgressBar(pb, i*100/user_num, sprintf("进度 (%s)", info),info)
}

plotCycleDays<-function(result,days_limit = 30){
  hist(result[cycle_days<days_limit,]$cycle_days,breaks=days_limit+1,right=F,labels=T)
}

####### 结果处理
path = "F:/Project/20170809赎回周期再次计算/"
user_cash_log = fread(paste0(path,"user_cash_log.csv"))%>%
  .[,date:=as.Date(date)]
result = user_cash_log[cycle_days!=9999,]%>%
  .[,floor_days:=floor(cycle_days)]
# 1.刷子去除
brush_user<-fread(paste0(path,"brush_users.csv"))
result1<-result[!userid%in%brush_user$userid,]
result1_summary<-result1[,.(amount=sum(redeem_amount),num=.N),by=floor_days]
write.csv(result1_summary,paste0(path,"result_elim_brush.csv"),row.names = F)
#plotCycleDays(result1)

# 2.去掉疑似利息赎回
result2 = result1[redeem_amount>=50,]
result2_summary<-result2[,.(amount=sum(redeem_amount),num=.N),by=floor_days]
#plotCycleDays(result2)

user_summary<-copy(result2)%>%
  .[,.(num=.N,total_amount=sum(redeem_amount),total_value=sum(redeem_amount*cycle_days)),by=userid]%>%
  .[,avg_cycle_days:=round(total_value/total_amount,2)]
write.csv(user_summary,paste0(path,"result2_user_summary.csv"),row.names = F)
influence_user_summary<-user_summary[avg_cycle_days<=5,]


#################################
# 3.去掉新手体验金影响的赎回
result3<-copy(result2)%>%
  .[user_info,on="userid",invest1st_day:=as.Date(i.invest1st_day)]%>%
  .[,diffdays:=as.numeric(date-invest1st_day)]%>%
  .[!between(diffdays,3,7),]
plotCycleDays(result3)





