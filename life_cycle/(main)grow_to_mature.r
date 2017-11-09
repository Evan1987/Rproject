library(data.table)
library(magrittr)
library(readr)
path="F:/Project/20170426用户生命周期相关/"
source('~/rstudio/!custom/(fun)fun.elbow_point.r', encoding = 'UTF-8')

user_invest<-read_csv(paste0(path,"user_invest.csv"))%>%as.data.table(.)%>%
  .[,grow_days:=difftime(first_10000_day,invest1st_day,units = "days")]

invalid_data<-user_invest[grow_days<=30,]
valid_data<-user_invest[grow_days>30,]
quantile(invalid_data$invest_days,probs = seq(0,1,0.1))
quantile(valid_data$invest_days,probs = seq(0,1,0.1))

N=nrow(valid_data)
result=data.table()
for(i in 1:100){
  tpr=(nrow(valid_data[invest_days<=i,])/N)%>%round(.,2)
  temp<-data.table(freq=i,tpr=tpr)
  result<-rbind(result,temp)
}
elbow_point=fun.elbow_point(result$freq,result$tpr,doplot = T)

N=nrow(user_invest)
result_total=data.table()
for(i in 1:100){
  tpr=(nrow(user_invest[invest_days<=i,])/N)%>%round(.,2)
  temp<-data.table(freq=i,tpr=tpr)
  result_total<-rbind(result_total,temp)
}
elbow_point2=fun.elbow_point(result_total$freq,result_total$tpr,doplot = T)
write.csv(result_total,paste0(path,"result_total.csv"),row.names = F)
