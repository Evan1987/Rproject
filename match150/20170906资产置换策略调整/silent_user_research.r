library(data.table)
library(magrittr)
library(readr)

now_date=as.Date("2017-09-05")
path="F:/Project/20170315资产匹配穿透150人调研/20170906资产置换策略调整/"
rawdata<-fread(paste0(path,"silent_users.csv"))%>%
  .[,premium_final:=replace(premium_final,which(premium_final=="\\N"),"0")%>%as.numeric(.)]%>%
  .[,last_visit_day:=replace(last_visit_day,which(last_visit_day=="\\N"),"2017-08-05")%>%as.Date(.)]%>%
  .[,daySpan:=as.numeric(now_date-last_visit_day)]

dayLines=0:31
feval<-function(x,df){
  result <- df[daySpan>=x,]%>%
    .[,.(num=.N,amount=sum(premium_final))]%>%
    .[,minDays:=x]
  return(result)
}

resultSummary = mapply(feval,dayLines,MoreArgs = list(df=rawdata))%>%
  apply(.,2,function(x) sapply(x,unclass))%>%
  t(.)%>%
  as.data.table(.)

write.csv(resultSummary,paste0(path,"silent_user_judge.csv"),row.names = F)
