library(data.table)
library(magrittr)
library(readr)
library(tcltk)
Rcpp::sourceCpp('20170905premium_elasticity/findValley.cpp')
Rcpp::sourceCpp('20170905premium_elasticity/findPeak.cpp')
Rcpp::sourceCpp('20170905premium_elasticity/peakSmooth.cpp')

# 对确定范围的谷点进行相应深度的边缘测量
marginCal<-function(height,labels,start,end,valleyPoint){
  valleyHeight = labels[valleyPoint]
  testHeight = height+valleyHeight
  left<-{
    targetLabels = labels[start:valleyPoint]
    which(targetLabels>=testHeight)%>%max(.)+start-1
  }
  right<-{
    targetLabels = labels[valleyPoint:end]
    which(targetLabels>=testHeight)%>%min(.)+valleyPoint-1
  }
  return(data.table(left,right,height))
}


path = "F:/Project/20170905用户存量弹性评价/"

cash_log = read_csv(paste0(path,"cash_log.csv"))%>%
  as.data.table(.)%>%
  .[,.(amount=sum(amount)),by=.(userid,date)]%>%
  .[abs(amount)>0,]

# 初筛
cash_log_summary = cash_log[,.(investNum=sum(amount>0),
                               redeemNum=sum(amount<0),
                               maxRedeem=min(amount)),by=userid]
validUsers = cash_log_summary[investNum>=2&redeemNum>=2&maxRedeem< -1000,]$userid
dayLimit = 7
now_day = as.Date("2017-07-31")
users = validUsers
user_num = length(users)
result = data.table()
pb <- tkProgressBar("进度","已完成 %", 0, 100)

for(i in 1:user_num){
  user = users[i]
  user_cash_log = cash_log[userid==user,-"userid"]%>%
    .[CJ(date = seq.Date(from=min(date),to=now_day,by="days")),on="date"]%>%
    .[,amount:=replace(amount,is.na(amount),0)]%>%
    .[,premium:=cumsum(amount)%>%pmax(.,0)]
  premium = user_cash_log$premium
  
  if(max(premium)<1000|nrow(user_cash_log)<30|mean(premium)<1000){
    next
  }
  
  unit = (floor((max(premium)-min(premium))*0.1/1000)*1000)
  unit = ifelse(unit>=50000,50000,ifelse(unit<=500,500,unit))
  num=floor(max(premium)/unit)
  breaks = (0:(num+1))*unit
  user_cash_log[,label:=cut(premium,breaks = breaks,include.lowest = T,labels = 0:(length(breaks)-2))%>%
                  as.character(.)%>%as.numeric(.)]
  
  labels = user_cash_log$label%>%peakSmooth(.,dayLimit)
  peakPositions = findPeak(labels)%>%c(.,1,length(labels))%>%unique(.)
  valleyPositions = findValley(labels)%>%.[which(.<length(labels))]
  valleyPointNum = length(valleyPositions)
  
  if(valleyPointNum==0){
    next
  }
  user_result = data.table()
  for(j in 1:valleyPointNum){
    
    valleyPoint = valleyPositions[j]
    start = max(peakPositions[peakPositions<valleyPoint])
    end = min(peakPositions[peakPositions>valleyPoint])
    
    # 确定最大测试深度
    maxDeep = {
      forwardHeight = max(cumsum(diff(labels[valleyPoint:end])))
      backwardHeight = max(cumsum(diff(rev(labels[start:valleyPoint]))))
      min(backwardHeight,forwardHeight)
    }
    
    # 对该谷点进行深度测试
    marginResult = mapply(marginCal,1:maxDeep,MoreArgs = list(labels=labels,
                                                              start=start,
                                                              end=end,
                                                              valleyPoint=valleyPoint))%>%
      apply(.,2,function(x) sapply(x,unlist))%>%
      t(.)%>%
      data.table(.)
    # 去重，当两端边缘一致时，保留最大高度
    if(maxDeep>1){
      marginResult = marginResult[,.(height=max(height)),by=.(left,right)]
    }
    user_result = rbind(user_result,marginResult)
  }
  
  result_tmp = data.table(userid=user,
                          amount=unit*user_result$height,
                          from=user_cash_log$date[user_result$left],
                          daySpan=user_result$right-user_result$left)
  result<-rbind(result,result_tmp)
  info<- sprintf("已完成 %.4f%%", round(i*100/user_num,4))  
  setTkProgressBar(pb, i*100/user_num, sprintf("进度 (%s)", info),info)
}

write.csv(result,paste0(path,"result.csv"),row.names = F)

