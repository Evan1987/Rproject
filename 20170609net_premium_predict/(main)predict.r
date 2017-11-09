library(data.table)
library(magrittr)
library(readr)
library(stringr)
source('~/rstudio/20170609net_premium_predict/(fun)bat_user_cash_forecast.r', echo=TRUE)
source('~/rstudio/20170609net_premium_predict/(fun)user_cash_train.r', encoding = 'UTF-8', echo=TRUE)
source('~/rstudio/20170609net_premium_predict/(fun)user_cash_forecast.r', encoding = 'UTF-8', echo=TRUE)

path="F:/Project/20170609日存量增量预测/"
calendar = fread(paste0(path,"calendar.csv"))%>%.[,log_day:=as.Date(log_day)]
net_premium_summary = read_csv(paste0(path,"trainSet.csv"))%>%as.data.table(.)%>%setkey(.,user_type,log_day)
invest_summary<-read_csv(paste0(path,"invest_summary.csv"))%>%as.data.table(.)%>%setkey(.,user_type,log_day)
redeem_summary<-read_csv(paste0(path,"redeem_summary.csv"))%>%as.data.table(.)%>%setkey(.,user_type,log_day)

total_data<-{
  start = min(net_premium_summary$log_day)
  end = max(net_premium_summary$log_day)
  user_types = unique(net_premium_summary$user_type)%>%.[which(.!="notag")]
  data.table(log_day = seq.Date(start,end,by="days"))%>%
    .[,.SD[CJ(log_day = log_day,user_type = user_types),on="log_day"]]%>%
    .[invest_summary,on=c("log_day","user_type"),invest_amount := i.amount]%>%
    .[redeem_summary,on=c("log_day","user_type"),redeem_amount := i.amount]%>%{
      vars = c("invest_amount","redeem_amount")
      .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols = vars]
    }%>%
    .[,net_premium:=round(invest_amount-redeem_amount)]%>%
    .[,value:=log(invest_amount+.01)-log(redeem_amount+.01)]%>%
    setkey(.,log_day,user_type)
}

library(corrplot)
types = c("S","A","B","freshman")
typelist<-c()
for(i in 1:length(types)){
  mat<-combn(types,i,simplify = F)
  typelist<-c(typelist,mat)
}

for(i in 1:length(typelist)){
  types = typelist[[i]]
  select_data<-total_data[user_type%in%types,]%>%
    .[,.(invest_amount=sum(invest_amount),
         redeem_amount=sum(redeem_amount),
         net_premium=sum(net_premium)),by=log_day]%>%
    .[,remain_ratio:=round(net_premium/(invest_amount+.01),2)]%>%
    .[,value:=log(invest_amount+.01)-log(redeem_amount+.01)]
  
  png(filename = paste0(path,str_c(types,collapse = "-"),".png"))
  corrplot(corr = cor(select_data[,c("invest_amount","redeem_amount","net_premium","remain_ratio")]),
           method = "number",main=paste(str_c(types,collapse = "-"),"-corr"))
  dev.off()
}

result<-data.table()
for(i in user_types){
  trainResult<-user_cash_train(trainSetTotal = total_data,
                               calendar = calendar,
                               user_types = c(i),
                               trainTimeSpan = c("2016-02-01","2017-06-01"),
                               value.var = "value")
  
  trainResultSet<-trainResult$trainingSet%>%
    .[,res_ratio:=round(abs(residuals/(value+1)),4)]
  
  probs = seq(0,1,0.1)
  x1 = quantile(abs(trainResultSet$residuals),probs=probs)
  x2 = quantile(trainResultSet$res_ratio,probs=probs)
  temp = data.table(probs = probs,user_type=i,res=x1,res_ratio=x2)
  result<-rbind(result,temp)
}
result2<-dcast(result,probs~user_type,value.var = c("res","res_ratio"))



types = c("S","A","B")
bindtypes = str_c(types,collapse = "")
select_data<-total_data[user_type%in%types,]%>%
  .[,.(invest_amount=sum(invest_amount),
       redeem_amount=sum(redeem_amount),
       net_premium=sum(net_premium)),by=log_day]%>%
  .[,value:=log(invest_amount+100)-log(redeem_amount+100)]%>%
  .[,user_type:=bindtypes]

trainResult<-user_cash_train(trainSetTotal = select_data,
                             calendar = calendar,
                             user_types = bindtypes,
                             trainTimeSpan = c("2016-02-01","2017-05-31"),
                             value.var = "net_premium")
investResult<-user_cash_train(trainSetTotal = select_data,
                              calendar = calendar,
                              user_types = bindtypes,
                              trainTimeSpan = c("2016-02-01","2017-05-31"),
                              value.var = "invest_amount")
redeemResult<-user_cash_train(trainSetTotal = select_data,
                              calendar = calendar,
                              user_types = bindtypes,
                              trainTimeSpan = c("2016-02-01","2017-05-31"),
                              value.var = "redeem_amount")

trainResultSet<-trainResult$trainingSet%>%
  .[investResult$trainingSet,on="log_day",invest_amount:=round(i.value-i.residuals)]%>%
  .[redeemResult$trainingSet,on="log_day",redeem_amount:=round(i.value-i.residuals)]%>%
  .[,predict1_values:=round(value-residuals)]%>%
  .[,predict2_values:=round(invest_amount-redeem_amount)]%>%
  .[,diff_predict:=round(predict1_values-predict2_values)]

xx<-trainResultSet[,c("log_day","value","predict1_values","predict2_values","residuals","diff_predict")]%>%
  calendar[.,on="log_day"]%T>%
  setnames(.,"residuals","residuals1")%>%
  .[,residuals2:=round(value-predict2_values)]


user_type_list=list(c("S","A","B"),
                    c("C","D","E"),
                    c("brush"),
                    c("freshman"),
                    c("newcomer"))
trainTimeSpan_list = list(c("2016-02-01","2017-05-31"),
                          c("2016-02-01","2017-05-31"),
                          c("2017-04-01","2017-05-31"),
                          c("2017-04-01","2017-05-31"),
                          c("2017-04-01","2017-05-31"))

n = length(user_type_list)

result<-data.table()
for(i in 1:n){
  user_types = user_type_list[[i]]
  trainTimeSpan = trainTimeSpan_list[[i]]
  trainResult <- user_cash_train(trainSetTotal = total_data,
                                 calendar = calendar,
                                 user_types = user_types,
                                 trainTimeSpan = trainTimeSpan,
                                 value.var = "invest_amount")
  x<-trainResult$trainingSet[,c("log_day","residuals","user_type")]
  result<-rbind(result,x)
}
result_summary<-result[between(log_day,as.Date("2017-04-01"),as.Date("2017-05-31")),]%>%
  .[,.(residuals=sum(residuals)),by=.(log_day)]



forecastResult<-bat_user_cash_forecast(trainSetTotal = trainSetTotal,
                                         calendar = calendar,
                                         user_type_list=list(c("S","A","B"),
                                                             c("C","D","E"),
                                                             c("newcomer","freshman"),
                                                             c("brush")),
                                         trainTimeSpan_list = list(c("2016-02-01","2017-05-31"),
                                                                   c("2016-02-01","2017-05-31"),
                                                                   c("2016-02-01","2017-05-31"),
                                                                   c("2017-04-01","2017-05-31")),
                                         forecastTimeSpan=c("2017-06-02","2017-06-10"),
                                         value.var = "net_premium",
                                         detail = F)




forecastResult<-bat_net_premium_forecast(trainSetTotal = trainSetTotal,
                                         calendar = calendar,
                                         user_type_list=list(c("S","A","B","C","D","E"),
                                                             c("newcomer"),
                                                             c("freshman"),
                                                             c("brush")),
                                         trainTimeSpan_list = list(c("2017-02-01","2017-05-31"),
                                                                   c("2017-02-01","2017-05-31"),
                                                                   c("2017-02-01","2017-05-31"),
                                                                   c("2017-04-01","2017-05-31")),
                                         forecastTimeSpan=c("2017-06-02","2017-06-10"),detail = F)

forecastResult<-bat_net_premium_forecast(trainSetTotal = trainSetTotal,
                                         calendar = calendar,
                                         user_type_list=list(unique(trainSetTotal$user_type)),
                                         trainTimeSpan_list = list(c("2017-04-01","2017-05-31")),
                                         forecastTimeSpan=c("2017-06-02","2017-06-10"),detail = F)


result_temp<-net_premium_train(trainSetTotal = trainSetTotal,
                      calendar = calendar,
                      user_types = c("S","A","B","C"),
                      trainTimeSpan = c("2016-02-01","2017-05-31"))

plot(result_temp$residuals)
trainSet_temp<-result_temp$trainingSet







