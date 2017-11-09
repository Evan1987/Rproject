library(data.table)
library(magrittr)
library(readr)
library(tcltk)

now_day = as.Date("2017-08-01")
t = 60

getPars<-function(x,y,w){
  m = lm(y~x,weights = w)
  #coefficients = m$coefficients%T>%{names(.)<-c("intercept","slope")}
  return(c(m$coefficients,sd=sd(m$residuals)))
}

path = "F:/Project/20170905用户存量弹性评价/"

rawData = read_csv(paste0(path,"result.csv"))%>%
  as.data.table(.)%>%
  setorder(.,userid,from)

user_summary = rawData[,.(num=.N),by=userid]%>%
  setorder(.,-num)

users = user_summary$userid
user_num = length(users)
result = matrix(ncol = 7,nrow = user_num)%T>%{colnames(.)<-c("userid","intercept","slope","sd","from","to","num")}
pb <- tkProgressBar("进度","已完成 %", 0, 100)
add_data = data.table(userid="",amount=0,from=now_day,daySpan=0)
for(i in 1:user_num){
  user = users[i]
  # 补充数据
  userData = rawData[userid==user,]%>%
    rbind(.,add_data)
  # 回归变量处理
  x0 = userData$amount
  # 保证x0=0时，x也为0
  x = log10(x0+1)
  y = userData$daySpan
  
  # 获取自变量的大致区间
  if(uniqueN(x0[x0>0])==1){
    amountSpan = rep(x0[x0>0][1],2)
  }else{
    amountSpan = c(quantile(x0[x0>0],0.1),quantile(x0[x0>0],0.9))
  }
  # 计算权重
  w = exp(-as.numeric(userData$from-now_day)^2/(2*60^2))
  # 拟合得到参数
  pars = try(getPars(x,y,w),T)
  
  if(length(pars)==0){
    pars = rep(0,3)
  }
  result[i,]<-c(user,round(pars,2),amountSpan,length(x)-1)
  info<- sprintf("已完成 %.4f%%", round(i*100/user_num,4))  
  setTkProgressBar(pb, i*100/user_num, sprintf("进度 (%s)", info),info)
}

result<-data.table(result)%>%{
  vars = c("intercept","slope","sd","from","to","num")
  .[,(vars):=lapply(.SD,as.numeric),.SDcols=vars]
}
  


