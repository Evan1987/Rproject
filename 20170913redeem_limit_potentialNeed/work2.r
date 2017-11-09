library(data.table)
library(magrittr)
library(readr)
library(lubridate)
path = "F:/Project/20170913大额用户超额体现需求/"

invest_log = read_csv(paste0(path,"invest_log.csv"))%>%
  as.data.table(.)%>%
  .[,log_day:=lubridate::date(create_time)]

redeem_log = read_csv(paste0(path,"redeem_log.csv"))%>%
  as.data.table(.)%>%
  .[,log_day:=lubridate::date(create_time)]


invest_summary = invest_log[,.(amount=sum(amount)),by=.(userid,log_day)]
redeem_summary = redeem_log[,.(amount=sum(amount)),by=.(userid,log_day)]

write.csv(invest_summary,paste0(path,"invest_summary.csv"),row.names = F)
write.csv(redeem_summary,paste0(path,"redeem_summary.csv"),row.names = F)

##################################################
invest_log = read_csv(paste0(path,"invest_summary.csv"))%>%
  as.data.table(.)

redeem_log = read_csv(paste0(path,"redeem_summary.csv"))%>%
  as.data.table(.)%>%
  .[!is.na(log_day),]%>%
  .[,amount:=-amount]

cash_log = rbind(invest_log,redeem_log)%>%
  .[,.(amount=sum(amount)),by=.(userid,log_day)]%>%
  setorder(.,userid,log_day)%>%
  .[,premium:=cumsum(amount)%>%pmax(.,0),by=userid]


amountLevel = 5e+5

daySpanCal <-function(amountLevel,cash_log,now_day=as.Date("2017-09-13")){
  target_data = cash_log[premium>=amountLevel,]%>%
    .[,.(from = min(log_day),to=max(log_day)),by=userid]%>%
    {
      user_summary = cash_log[userid%in%.$userid,]%>%
        .[,.(last_day=max(log_day)),by=userid]
      .[user_summary,on="userid",last_day:=i.last_day]
    }%>%
    .[,to:=replace(to,to==last_day,now_day)]%>%
    .[,daySpan:=as.numeric(to-from)+1]%>%
    .[,last_day:=NULL]
  
  return(target_data)
}

result50 = daySpanCal(5e+5,cash_log)

result100 = daySpanCal(10e+5,cash_log)

write.csv(result50,paste0(path,"daySpan50.csv"),row.names = F)
write.csv(result100,paste0(path,"daySpan100.csv"),row.names = F)



