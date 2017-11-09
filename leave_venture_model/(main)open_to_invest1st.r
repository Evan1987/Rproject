library(data.table)
library(magrittr)
library(readr)
library(dplyr)
source('~/rstudio/!custom/(fun)colname_treat.R')
source('~/rstudio/!custom/(fun)fun.elbow_point.r', encoding = 'UTF-8')
path="F:/Project/20170419用户流失风险预警模型/"

open_to_invest1st<-read_csv(paste0(path,"reg_to_invest_data.csv"),locale = locale(encoding = "GBK"))%>%
  mutate(.,wait_time=ifelse(is.na(invest1st_time),Inf,difftime(invest1st_time,open_time,units="days")%>%as.numeric(.)))

open_to_invest1st<-as.data.table(open_to_invest1st)
quantile(open_to_invest1st[!is.na(invest1st_time),]$wait_time,probs = seq(0,1,0.1))

valid_data<-open_to_invest1st[(!is.na(invest1st_time))&wait_time>0,]
quantile(valid_data$wait_time,probs = seq(0,1,0.1))
num=nrow(valid_data)
dayspan<-seq(0.1,30,0.1)
result<-data.table()
#对这个问题，无论怎么选天数阈值，tpr都是1，而fpr则随着天数阈值增加而减小，因此需选取肘点
for(i in dayspan){
  fpr = nrow(valid_data[wait_time>i,])/num
  temp=data.table(wait_time=i,fpr=fpr)
  result<-rbind(result,temp)
}

elbow_point=fun.elbow_point(result$wait_time,result$fpr,doplot = T)

