library(data.table)
library(magrittr)
library(readr)
library(tcltk)
Rcpp::sourceCpp('20170825wage_invest_mode_recognition/findGroup.cpp')
source('~/rstudio/20170825wage_invest_mode_recognition/(fun)findAlpha.r', echo=TRUE)

# 补全月份信息
seq.month<-function(from,to,yearSpan=c(2015,2016,2017),monthExcept=c(1,2,10)){
  m = 1:12%>%.[which(!.%in%monthExcept)]
  monthSeq = do.call(`+`,expand.grid(yearSpan*100,m))%>%.[which(.>=from&.<=to)]%>%sort(.)
  return(monthSeq)
}

daySpan = 5
conf.level = 0.97
p = daySpan/30
# 时间衰减系数（指数衰减）
alpha = findAlpha(p=p,conf.level = 0.95)
trialsInMonth = 30
meanSuccess = trialsInMonth*p
sdSuccess = sqrt(trialsInMonth*p*(1-p))


path = "F:/Project/20170825疑似工薪用户投资模式/"

invest_log = read_csv(paste0(path,"validInvestLog.csv"))%>%
  as.data.table(.)%>%
  .[,month:=year(date)*100+month(date)]%>%
  setorder(.,userid,date)

yearSpan = unique(year(invest_log$date))
invest_month_summary<-invest_log[,.(total_amount=sum(invest_amount)),by=.(userid,month)]%T>%
  setorder(.,userid,month)

users = unique(invest_log$userid)
user_num = length(users)

f1<-function(x,df,months,r,N,prob=p){
  # 加上时间衰减
  y = df[between(day,x,x+4),]$month%>%unique(.)%>%sort(.)
  judge = sum((months%in%y)*r)
  return(pbinom(floor(judge)-1,N,prob = prob,lower.tail = F))
}

f2<-function(x,df,dfMonthSummary,r,N,trialsNum,mean,sd,conf.level){
  focusLog = df[between(day,x,x+4),][,.(amount=sum(invest_amount)),by=month]%>%
    .[dfMonthSummary[,c("month","total_amount")],on="month"]%>%
    .[,amount:=replace(amount,is.na(amount),0)]%>%
    .[,conf:=round(pnorm(amount/(total_amount+0.01)*trialsNum,mean,sd)*10000)/10000]
  judge = sum(focusLog$conf*r)
  # 关于金额的时间集中度的可信度
  concentrateConf = round(pnorm(judge,mean=0.5*N,sd=sqrt(N*1/12))*100,2)
  # 金额稳定的评价指数,标尺为标准正态分布的分位距qnorm(0.9)-qnorm(0.1)=2.563103，定为60分
  selectAmount = focusLog[conf>=conf.level,]$amount
  if(length(selectAmount)<3){
    meanAmount = 0
    stabledConf =0
  }else{
    meanAmount = median(selectAmount)
    stabledConf = {
      y = sd(selectAmount)/mean(selectAmount)
      round(120/(1+exp(8.047*(y-0.2))),2)
    }
  }
  return(data.table(start=x,concentrateConf,meanAmount,stabledConf))
}

result<-data.table()
pb <- tkProgressBar("进度","已完成 %", 0, 100)
for(i in 1:user_num){
  user = users[i]
  user_invest_log = invest_log[userid==user,]
  user_invest_summary = invest_month_summary[userid==user,]%>%
    .[CJ(month=seq.month(from = min(month),to=max(month),yearSpan = yearSpan,monthExcep=c(1,2,10))),on="month"]%>%
    .[,total_amount:=replace(total_amount,is.na(total_amount),0)]%>%
    .[,r:=alpha^rev(.I-1)]
  
  months = user_invest_summary$month
  N = length(months)
  rseq = user_invest_summary$r
  r = rseq*N/sum(rseq)
  minDay = min(user_invest_log$day)
  maxDay = max(user_invest_log$day)
  timeLineStart = minDay:pmax(minDay,maxDay-daySpan+1)
  timeFocusJudge = mapply(FUN = f1,timeLineStart,MoreArgs = list(df=user_invest_log,
                                                                 months,
                                                                 r=r,
                                                                 N=N,
                                                                 prob=p))
  validTimeLineStart = timeLineStart[timeFocusJudge<=(1-conf.level)]
  
  if(length(validTimeLineStart)==0){
    next
  }
  tmp<-mapply(FUN = f2,validTimeLineStart,MoreArgs = list(df=user_invest_log,
                                                          dfMonthSummary = user_invest_summary,
                                                          r=r,
                                                          N=N,
                                                          trialsNum = trialsInMonth,
                                                          mean=meanSuccess,
                                                          sd=sdSuccess,
                                                          conf.level = conf.level))%>%
    
    
    apply(.,2,function(x) sapply(x,unclass))%>%
    t(.)%>%
    as.data.table(.)%>%
    .[,totalScore:=stabledConf+concentrateConf]%>%
    .[stabledConf>=70&concentrateConf>=85,]
  
  if(nrow(tmp)==0){next}
  tmp[,":="(end=start+4,userid=user)]
  result<-rbind(result,tmp)
  info<- sprintf("已完成 %.4f%%", round(i*100/user_num,4))  
  setTkProgressBar(pb, i*100/user_num, sprintf("进度 (%s)", info),info)
}

treatedResult<-copy(result)%T>%
  setorder(.,userid,start)%>%
  .[,index:=findGroup(start),by=userid]%>%
  .[,id:=.I]%T>%
  setorder(.,userid,index,-totalScore)%>%
  {
    selectID<-.[,.(id=first(id)),by=.(userid,index)]%>%.$id
    .[id%in%selectID,]
  }%>%
  .[,-c("id","index")]

write.csv(treatedResult,paste0(path,"treatedResult.csv"),row.names = F)

