library(data.table)
library(magrittr)
library(readr)
Rcpp::sourceCpp('20170920miaoka/calCost.cpp')

path= "F:/Project/20170920秒咖赔付/"
rawdata = read_csv(paste0(path,"rawdata.csv"))%>%
  as.data.table(.)%>%
  .[,create_time:=as.POSIXct(create_time)]%>%
  setorder(.,userid,issuer,create_time)

rs = rb = 0.003
initTime = as.POSIXct("2017-03-30 0:00:00")

priceList = rawdata[price>0,c("userid","create_time","issuer","price")]%>%unique(.)
rawdata[deal_type%in%c("yonghuchoujiang","yonghuzhongjiang"),price:=0]
rawdata[,label:=ifelse(type=="buy",1,-1)]
rawdata[,total_num:=cumsum(num*label),by=.(userid,issuer)]

df = copy(rawdata)%>%
  .[,total_cost:=calCost(type,deal_type,num,price,poundage,total_num),by=.(userid,issuer)]%>%
  .[,cost:=total_cost-dplyr::lag(total_cost,default = 0),by=.(userid,issuer)]%>%
  {
    clearLog = .[total_num==0,]%>%.[,.(clearTime = max(create_time)),by=.(userid,issuer)]
    .[clearLog,on=c("userid","issuer"),clearTime:=i.clearTime]%>%
      .[,clearTime:=replace(clearTime,is.na(clearTime),initTime)]
  }

write.csv(df,paste0(path,"detail.csv"),row.names = F)

result<-copy(df)%>%
  .[,.(rest_num = last(total_num),
       clearTime = first(clearTime),
       totalBuyAmount = sum((num*price + poundage)*(type=="buy")),
       totalBuyPoundage = sum(poundage*(type=="buy")),
       totalSellAmount = sum((num*price - poundage)*(type=="sell")*(deal_type!="jiaoge")),
       totalSellPoundage = sum(poundage*(type=="sell")),
       totalExercisedNum = sum(num*(deal_type=="jiaoge")),
       totalExercisedAmount = -1*sum(cost*(deal_type=="jiaoge")),
       validBuyAmount = sum((num*price + poundage)*(type=="buy")*(create_time>clearTime)),
       validBuyPoundage = sum(poundage*(type=="buy")*(create_time>clearTime)),
       validSellAmount = sum((num*price - poundage)*(type=="sell")*(deal_type!="jiaoge")*(create_time>clearTime)),
       validSellPoundage = sum(poundage*(type=="sell")*(create_time>clearTime)),
       validExercisedNum = sum(num*(deal_type=="jiaoge")*(create_time>clearTime)),
       validExercisedAmount = -1*sum(cost*(deal_type=="jiaoge")*(create_time>clearTime)), 
       total_cost = last(total_cost)
      ),by=.(userid,issuer)]

write.csv(result,paste0(path,"result.csv"),row.names = F)






