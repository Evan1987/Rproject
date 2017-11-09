library(data.table)
library(magrittr)
library(readr)
library(MASS)
library(lubridate)
source('~/rstudio/!custom/(fun)fun.elbow_point.r', encoding = 'UTF-8', echo=TRUE)
path = "F:/Project/20170905用户存量弹性评价/"

# 数据准备
user_info = fread(paste0(path,"user_info.csv"))
brush_user = fread(paste0(path,"brush_users.csv"))
invest_log = read_csv(paste0(path,"invest_log.csv"))%>%
  as.data.table(.)%>%
  .[!userid%in%brush_user$userid,]

redeem_log = read_csv(paste0(path,"redeem_log.csv"))%>%
  as.data.table(.)%>%
  .[!userid%in%brush_user$userid,]

cash_log = rbind(invest_log,redeem_log[,amount:=-amount])%>%
  .[,.(amount=sum(amount)),by=.(userid,date)]%T>%
  setorder(.,userid,date)%>%
  .[,days:=c(NA,diff(date)),by=userid]

# 寻找用户存量不发生变化的时间长度水平(存量稳定的判据)
user_days_log = cash_log[!is.na(days),]%>%
{
  summ = .[,.(num=.N,sd = sd(days)),by=userid]
  .[userid%in%summ[!is.na(sd)&sd>0,]$userid,]
}

gammaFit<-function(x){
  a<-fitdistr(x,"gamma")
  return(a$estimate)
}
users = unique(user_days_log$userid)
user_num = length(users)
mat = matrix(nrow=user_num,ncol = 2,dimnames = list(c(),c("shape","rate")))
for(i in 1:user_num){
  x = user_days_log[userid==users[i],]$days
  mat[i,]=try(gammaFit(x),T)
}

user_gamma<-data.table(userid=users,mat)%>%
  .[nchar(shape)<30,]%>%
  .[,":="(shape=round(as.numeric(shape),2),rate=round(as.numeric(rate),2))]

dayLines = 5:10
feval<-function(x,df,conf.level){
  df[,conf:=1-pgamma(x,shape = shape,rate = rate)]
  return(nrow(df[conf>=conf.level,]))
}
confResult = mapply(feval,dayLines,MoreArgs = list(df=user_gamma,conf.level=0.7))
fun.elbow_point(x=dayLines,y=confResult,doplot = T) #  ans: 7


###################################################################################
x = cash_log$days%>%.[!is.na(.)]
boxplot(x)

quantile(x,probs = seq(0,1,0.1))
# 0%  10%  20%  30%  40%  50%  60%  70%  80%  90% 100% 
# 1    1    1    2    3    4    5    7   12   22  337 

