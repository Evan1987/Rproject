library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(tcltk)
source('~/rstudio/20170816user_regular_potential/(fun)findAvailSec.r', encoding = 'UTF-8', echo=TRUE)
path = "F:/Code projects/Python/Common/20170816user_regular_potential/"

user_premium = read_csv(paste0(path,"user_premium1.csv"))%>%as.data.table(.)
users<-unique(user_premium$userid)
user_num = length(users)

result = data.table()
for(i in 1:user_num){
  user = users[i]
  resultTmp<-data.table()
  user_cash_log<-user_premium[userid==user,]%>%
      .[,c("log_day","premium_cur")]%>%
      .[CJ(log_day=seq.Date(min(log_day),max(log_day),by="days")),on="log_day"]%T>%
        setorder(.,log_day)%>%
    .[,premium_cur:=replace(premium_cur,is.na(premium_cur),0)]
  
  for(j in 1:12){
    tmp<-findAvailSec(user_cash_log,
                      judge.var = "premium_cur",
                      date.var = "log_day",
                      minPremium = 1000,
                      minStep = 1000,
                      minLen = 30*j,
                      maxStepNum = 10)
    if(nrow(tmp)>0){
      resultTmp<-rbind(resultTmp,tmp)
    }else{
      break
    }
  }
  if(nrow(resultTmp)>0){
    resultTmp[,userid:=user]
    result<-rbind(result,resultTmp)
  }
}

write.csv(result,paste0(path,"result.csv"),row.names = F)



