

# invest_simu_data必须是经过【fun】invest_simu处理过的
redeem_simu<-function(redeem_rawdata,invest_simu_data,premium_initial){
  library(data.table)
  library(magrittr)
  library(lubridate)
  library(tcltk)
  
  invest_simu_data[,create_time:=as.POSIXct(create_time)]
  premium_data<-copy(premium_initial)%>%
    .[,c("unmatched_regular_premium",
         "unmatched_tPlus_premium",
         "unmatched_current_premium"):=NULL]%>%
    .[,":="(current_premium=premium-regular_premium-tPlus_premium,
            log_day=as.POSIXct("2017-03-14"))]
  
  redeem_rawdata<-copy(redeem_rawdata)%>%
    .[amount>=100,]%>%
    .[,create_time:=as.POSIXct(create_time)]%>%
    .[,type:=0]%>%
    .[,amount:=round(amount,2)]%T>%
    setkey(.,userid)
  
  users<-unique(redeem_rawdata$userid)
  num<-length(users)
  redeem_result<-data.table()
  pb <- tkProgressBar("进度","已完成 %", 0, 100)
  for(i in 1:num){
    user<-users[i]
    premium_temp<-premium_data[userid==user,]
    if(nrow(premium_temp)==0){
      next
    }
    invest_temp<-invest_simu_data[userid==user,]%T>%
      setorder(.,create_time)
    
    redeem_temp<-redeem_rawdata[userid==user,]%T>%setorder(.,create_time)
    redeem_result_user<-data.table()
    for(j in 1:nrow(redeem_temp)){
      redeem_now<-redeem_temp[j,]
      invest_before<-invest_temp[create_time<=redeem_now$create_time,]
      invest_temp<-invest_temp[create_time>redeem_now$create_time,]
      premium_before<-copy(premium_temp)%>%
        .[,":="(premium=premium+sum(invest_before$amount),
                regular_premium=regular_premium+sum(invest_before[type==2,]$amount),
                tPlus_premium=tPlus_premium+sum(invest_before[type==1,]$amount),
                current_premium=current_premium+sum(invest_before[type==0,]$amount))]
      
      # 赎回额既小于当前活期存量又小于T+N存量
      if(redeem_now$amount<=min(premium_before$current_premium,premium_before$tPlus_premium))
      {
        # 同时赎回额还小于投资前的T+N存量，且赎回时间与投资前的记录时间相差超过一天，则认为是T+N赎回
        if(difftime(redeem_now$create_time,premium_temp$log_day,units = "day")>=1&
           redeem_now$amount<=premium_temp$tPlus_premium)
        {
          redeem_now[,":="(type=1,amount=floor(amount/1000)*1000)]
        }else{# 否则认为是活期赎回
          redeem_now[,type:=0]
        }
      }
      # 赎回额既大于当前活期存量又大于T+N存量
      else if(redeem_now$amount>max(premium_before$current_premium,premium_before$tPlus_premium))
      {
        # 且赎回时间与投资前的记录时间相差超过一天，则认为是T+N赎回，赎回额等于当前T+N存量
        if(difftime(redeem_now$create_time,
                    max(premium_temp$log_day,invest_before$create_time),
                    units = "day")>=1&
           premium_before$tPlus_premium>0)
        {
          redeem_now[,":="(type=1,amount=premium_before$tPlus_premium)]
        }else{# 否则认为是活期赎回，赎回额等于当前活期存量
          redeem_now[,":="(type=0,amount=premium_before$current_premium)]
        }
      }
      # 赎回额处于当前活期存量和T+N存量之间，且T+N存量较大
      else if(premium_before$tPlus_premium>=premium_before$current_premium)
      {
        # 且赎回时间与投资前的记录时间相差超过一天，则认为是T+N赎回
        if(difftime(redeem_now$create_time,max(premium_temp$log_day,invest_before$create_time),units = "day")>=1)
        {
          redeem_now[,":="(type=1,amount=floor(amount/1000)*1000)]
        }else{
          redeem_now[,":="(type=0,amount=premium_before$current_premium)]
        }
      }
      # 赎回额处于当前活期存量和T+N存量之间，且活期存量较大
      else{
        redeem_now[,type:=0]
      }
      premium_temp<-premium_before[,log_day:=redeem_now$create_time]%>%
        .[,":="(premium=premium-redeem_now$amount,
                tPlus_premium=tPlus_premium-ifelse(redeem_now$type==1,redeem_now$amount,0),
                current_premium=current_premium-ifelse(redeem_now$type==0,redeem_now$amount,0))]
      
      
      redeem_result_user<-rbind(redeem_result_user,redeem_now)
    }
    redeem_result<-rbind(redeem_result,redeem_result_user)
    info<- sprintf("已完成 %d%%", round(i*100/num))  
    setTkProgressBar(pb, i*100/num, sprintf("进度 (%s)", info),info)
  }
  
  redeem_simu_data<-redeem_result[amount>0,]
  
  return(redeem_simu_data)
}











