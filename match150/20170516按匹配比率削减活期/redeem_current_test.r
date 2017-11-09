library(data.table)
library(magrittr)

path="F:/Project/20170315资产匹配穿透150人调研/20170517活期待匹按比例削减/"

user_list<-fread(paste0(path,"user_list.csv"))
match_record<-fread(paste0(path,"match_record.csv"))
invest_simu_data<-fread(paste0(path,"invest_simu_data.csv"))%>%
  .[,":="(amount=round(amount,2),create_time=as.POSIXct(create_time))]
redeem_simu_data<-fread(paste0(path,"redeem_simu_data.csv"))%>%
  .[,":="(amount=round(amount,2),create_time=as.POSIXct(create_time))]

xx<-
{ 
  a1<-match_record[,.(matched_premium=round(sum(amount),2)),by=.(userid,type)]%>%
    .[type=="current",]%T>%
    setnames(.,"matched_premium","amount")%>%
    .[,-"type"]
  a2<-user_list[,c("userid","unmatched_current_premium")]%T>%
    setnames(.,"unmatched_current_premium","amount")%>%
    .[,amount:=round(amount,2)]
  
  a<-rbind(a1,a2)%>%
    .[,.(amount=sum(amount)),by=userid]%>%
    .[,create_time:=as.POSIXct("2017-03-14 23:59:59")]
  
  b<-invest_simu_data[type==0,-"type"]
  d<-redeem_simu_data[type==0,-"type"]%>%.[,amount:=-amount]
  
  rbindlist(list(a,b,d),fill = T)
}%T>%
  setorder(.,userid,create_time)%>%
  .[,premium:=round(cumsum(amount),2),by=userid]






