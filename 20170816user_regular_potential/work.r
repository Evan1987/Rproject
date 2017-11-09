library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(tcltk)
source('~/rstudio/20170816user_regular_potential/(fun)findAvailSec.r', encoding = 'UTF-8', echo=TRUE)
path = "F:/Project/20170816用户加息计划潜力/"

invest_log<-read_csv(paste0(path,"elim_invest_log.csv"))%>%
  as.data.table(.)%>%
  setkey(.,userid,date)

redeem_log<-read_csv(paste0(path,"elim_redeem_log.csv"))%>%
  as.data.table(.)

users<-unique(invest_log$userid)
user_num = length(users)

result = data.table()
pb <- tkProgressBar("进度","已完成 %", 0, 100)
for(i in 1:user_num){
  user = users[i]
  resultTmp<-data.table()
  user_cash_log<-{
    invest<-invest_log[userid==user,-"userid"]
    redeem<-redeem_log[userid==user,-"userid"]%>%
      .[,amount:=-amount]
    # 创建模拟存量表
    rbind(invest,redeem)%>%
      setorder(.,date)%>%
      .[,premium:=cumsum(amount)%>%pmax(.,0)]%>%
      .[,c("date","premium")]%>%
      .[CJ(date=seq.Date(min(date),max(date),by="days")),on="date",roll=T]
  }
  for(j in 1:12){
    tmp<-findAvailSec(user_cash_log,
                      judge.var = "premium",
                      date.var = "date",
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
  info<- sprintf("已完成 %.4f%%", round(i*100/user_num,4))  
  setTkProgressBar(pb, i*100/user_num, sprintf("进度 (%s)", info),info)
}

write.csv(result,paste0(path,"result.csv"),row.names = F)



