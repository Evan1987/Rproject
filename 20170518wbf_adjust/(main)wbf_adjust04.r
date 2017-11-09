library(data.table)
library(magrittr)
library(stringr)
library(readr)
library(gurobi)
source('~/rstudio/20170518wbf_adjust/(fun)fun.select_wise_slim.r', encoding = 'UTF-8')
source('~/rstudio/20170518wbf_adjust/(fun)fun.premium_cal.r')
source('~/rstudio/20170518wbf_adjust/(fun)fun.safe_redeem_amount.r')
source('~/rstudio/20170518wbf_adjust/(fun)type_switch.r', encoding = 'UTF-8')
path="F:/Project/20170518沃百富数据调整/"
daily_rate=5/100/365
#############################  0.数据预处理  ##################################
# rawdata<-fread(paste0(path,"cash_record.csv"))%T>%
#   setnames(.,names(.),tolower(names(.)))%>%
#   .[,ord_no:=as.character(ord_no)]%T>%
#   setorder(.,dt,ord_no)
# 
# #实际数据处理
# newdata<-copy(rawdata)%>%
#   .[,ftype:=sapply(ftype,fun.ftype)]%>%
#   .[,adjust:=sapply(ftype2,fun.ftype2)]%>%
#   .[,trade_type:=sapply(trade_type,fun.ftype)]%>%
#   .[,dt:=as.Date(as.character(dt),"%Y%m%d")]%>%
#   .[,amount:=round(amount,2)]%>%
#   .[,premium_change:=sapply(trade_type,fun.premium)*amount]%>%
#   .[,-c("ftype2")]
# 
# # 获取用户首投订单信息
# invest1st_info<-{
#   tmp<-newdata[trade_type=="invest",][,.(dt=min(dt)),by=user_id]
#   newdata[trade_type=="invest",][tmp,on=c("dt"="dt","user_id"="user_id"),mult="first"]
# }
# 
# # 用户首投订单及非调整范围订单锁定
# newdata[adjust==0|ord_no%in%invest1st_info$ord_no,isfix:=1]%>%
#   .[,isfix:=replace(isfix,is.na(isfix),0)]
# write.csv(newdata,paste0(path,"newdata.csv"),row.names = F)

newdata<-read_csv(paste0(path,"newdata.csv"),col_types = cols(ord_no="c"))%>%as.data.table(.)

newdata_snap<-copy(newdata)
invest1st_info<-newdata_snap[trade_type=="invest",][,.(invest1st_dt=min(dt)),by=user_id]
# 结果数据处理
# exp_result<-fread(paste0(path,"exp_result.csv"))%T>%
#   setnames(.,names(.),c("trade_type","dt","user_num","num","amount"))%>%
#   .[,trade_type:=sapply(trade_type,fun.ftype)]%>%
#   .[trade_type%in%c("invest","redeem"),]%>%
#   .[,dt:=as.Date(as.character(dt),"%Y%m%d")]%>%
#   .[dt<=max(newdata$dt),]
# 
# write.csv(exp_result,paste0(path,"exp_result.csv"),row.names = F)

exp_result<-read_csv(paste0(path,"exp_result.csv"))%>%as.data.table(.)
# 实际数据与结果数据的比较
actual_data<-newdata[adjust==1,][,
                                 .(user_num=uniqueN(user_id),num=.N,amount=round(sum(amount),2)),
                                 by=.(dt,trade_type)]
total_data<-actual_data[exp_result,on=c("dt"="dt","trade_type"="trade_type")]%T>%{
  vars<-names(.)%>%.[which(str_detect(.,"i."))]
  setnames(.,vars,str_c("exp.",str_sub(vars,3)))
}%>%
  .[,":="(diff_user_num=user_num-exp.user_num,
          diff_num=num-exp.num,
          diff_amount=amount-exp.amount)]


for_adjust<-total_data[(trade_type=="invest"&amount>1e+6&exp.amount<=1e+6)|
                       (abs(diff_num)>=5)|
                       (abs(diff_user_num)>=3)|
                       (abs(diff_amount)>=exp.amount*0.01),]%T>%
  setorder(.,dt,trade_type)

first_dt<-min(for_adjust$dt)
end_dt<-max(for_adjust$dt)

# not_adjust_redeem<-total_data[trade_type=="redeem"&!dt%in%for_adjust_redeem$dt,]
############################################### 整体调整 #############################################

# 确定投资池和赎回池
now_data<-newdata[!(between(dt,first_dt,end_dt) & isfix==0),]

premium_first<-newdata[dt<first_dt,]%>%
  .[,.(premium_change=sum(premium_change)),by=.(user_id,dt)]%T>%
  setorder(.,user_id,dt)%>%
  .[,retain_days:=as.numeric(dplyr::lead(dt)-dt),by=user_id]%>%
  .[is.na(retain_days),retain_days:=as.numeric(first_dt-1-dt)]%>%
  .[,premium:=fun.premium_cal(premium_change,days = retain_days,rate = daily_rate),by=user_id]%>%
  .[,.(first_premium=last(premium)),by=user_id]%>%
  .[,first_premium:=pmax(first_premium,0)]

redeem_pool<-newdata[between(dt,first_dt,end_dt) & trade_type=="redeem" & isfix==0,]
invest_pool<-newdata[between(dt,first_dt,end_dt) & trade_type=="invest" & isfix==0,]%>%
{
# 获取在过程中用户存量净增长的用户，这些用户的相应额度投资订单可以延后
# 获取用户在调整区间首末的存量变化
  premium_diff<-newdata[between(dt,first_dt,end_dt),]%T>%
      setorder(.,user_id,dt,trade_type)%>%
      .[,.(net_change=sum(premium_change)),by=user_id]
    
  a<-premium_diff[net_change>0,]
  b<-newdata[between(dt,first_dt,end_dt) & trade_type=="invest" & isfix==0,]%T>%
    setorder(.,user_id,amount)%>%
    .[,cum_amount:=cumsum(amount),by=user_id]%>%
    .[a[,c("user_id","net_change")],on="user_id",avail_amount:=i.net_change]%>%
    .[,avail_amount:=replace(avail_amount,is.na(avail_amount),0)]%>%
    .[cum_amount<=avail_amount,]
  
  .[!ord_no%in%b$ord_no,]
}

pool_summary<-{
  redeem_pool_summary<-redeem_pool[,.(redeem_amount=sum(amount),redeem_num=.N),by=user_id]
  invest_pool_summary<-invest_pool[,.(invest_amount=sum(amount),invest_num=.N),by=user_id]
  a<-now_data[between(dt,first_dt,end_dt),]%>%
    .[,.(change_amount = sum(premium_change)),by=user_id]
  data.table(user_id=unique(c(redeem_pool_summary$user_id,invest_pool_summary$user_id)))%>%
    .[invest1st_info,on="user_id",invest1st_dt:=i.invest1st_dt]%>%
    .[redeem_pool_summary,on="user_id",":="(redeem_amount=i.redeem_amount,redeem_num=i.redeem_num)]%>%
    .[invest_pool_summary,on="user_id",":="(invest_amount=i.invest_amount,invest_num=i.invest_num)]%>%
    .[a,on="user_id",":="(invest_net=i.change_amount)]%>%
    {
      vars=names(.)%>%.[which(str_detect(.,"invest_")|str_detect(.,"redeem_")|str_detect(.,"invest_net"))]
      .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
    }%>%
    .[,judge_amount:=round(redeem_amount-invest_net,2)]
}

for_adjust_invest<-total_data[between(dt,first_dt,end_dt) & trade_type=="invest",]%>%setorder(.,dt)
invest_adjust_num = nrow(for_adjust_invest)

for_adjus_redeem<-total_data[between(dt,first_dt,end_dt) & trade_type=="redeem",]%>%setorder(.,dt)
redeem_adjust_num = nrow(for_adjus_redeem)

############################################# 投资调整 #############################################

for(i in 1:invest_adjust_num){
  wait_adjust<-for_adjust_invest[i,]
  target_dt<-wait_adjust$dt
  
  now_redeem<-now_data[dt==target_dt & trade_type =="redeem",]
  if(nrow(now_redeem)>0){
    premium_danger_now<-{
    
      now_change<-now_data[between(dt,first_dt,target_dt) & 
                             user_id %in% now_redeem$user_id,
                           c("dt","user_id","premium_change")]
      
      a<-copy(premium_first)%T>%
        setnames(.,"first_premium","premium_change")%>%
        .[user_id%in%now_redeem$user_id,]%>%
        .[,dt:=first_dt-1]
      
      rbind(a,now_change)%>%
        .[,.(premium_change=sum(premium_change)),by=.(user_id,dt)]%T>%
        setorder(.,user_id,dt)%>%
        .[,retain_days:=as.numeric(dplyr::lead(dt)-dt),by=user_id]%>%
        .[is.na(retain_days),retain_days:=as.numeric(target_dt-dt)]%>%
        .[,premium:=fun.premium_cal(premium_change,days = retain_days,rate = daily_rate),by=user_id]%>%
        .[,.(last_premium=last(premium)),by=user_id]%>%
        .[last_premium<0,]
    }
      
    
    if(nrow(premium_danger_now)>0){
      add_num = nrow(premium_danger_now)
      add_data<-invest_pool[0]
      for(j in 1:add_num){
        wait_add<-premium_danger_now[j,]
        lack_amount = -1*wait_add$last_premium
        target_add_data<-invest_pool[user_id==wait_add$user_id,]
        if(nrow(target_add_data)<=1|sum(target_add_data$amount)<=lack_amount){
          add_temp<-target_add_data
        }else{
          add_temp<-{
            model<-list(obj = target_add_data$amount,
                        A = matrix(target_add_data$amount,nrow=1),
                        sense = c(">="),
                        rhs = lack_amount,
                        vtype = "B",
                        modelsense = "min")
            result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
            if(result$status=="OPTIMAL"){
              x<-result$x
              tmp<-target_add_data[which(x==1),]
            }else{
              tmp<-target_add_data
            }
            tmp
          }
        }
        add_data<-rbind(add_data,add_temp)
      }
      add_data[,dt:=target_dt]
      add_data_summary<-add_data[,.(amount=sum(amount),num=.N),by=user_id]
      invest_pool<-invest_pool[!ord_no%in%add_data$ord_no,]
      now_data<-rbind(now_data,add_data)%T>%setorder(.,dt,user_id,trade_type)
      pool_summary<-pool_summary[add_data_summary,
                                 on="user_id",
                                 ":="(invest_amount = round(invest_amount-i.amount,2),
                                      invest_num = invest_num-i.num)]
    }
  }
  
  
  fixed_data<-now_data[dt==target_dt & trade_type=="invest" & adjust==1,]
  fixed_data_summary<-fixed_data[,.(amount=sum(amount),num=.N),by=user_id]
  fixed_user_num = nrow(fixed_data_summary)
  fixed_num = sum(fixed_data_summary$num)
  fixed_amount = sum(fixed_data_summary$amount)
  
  exp_user_num = wait_adjust$exp.user_num - fixed_user_num
  exp_num = (wait_adjust$exp.num - fixed_num)%>%pmax(.,0)
  exp_amount = round(wait_adjust$exp.amount - fixed_amount,2)
  
  
  avail_list<-pool_summary[invest1st_dt<=target_dt & invest_amount>0,]
  good_list<-avail_list[judge_amount>0,]
  bad_list<-avail_list[judge_amount<=0,]
  
  if(sum(good_list$invest_amount)>=exp_amount*0.99 & 
     sum(good_list$invest_amount)<=exp_amount){
    select_result<-invest_pool[user_id%in%good_list$user_id,]%>%setorder(.,user_id,amount)
  }else if(sum(good_list$invest_amount)>exp_amount){
    target_data<-invest_pool[user_id%in%good_list$user_id,]
    select_result<-{
      model<-list(obj = target_data$amount,
                  A = matrix(c(rep(1,nrow(target_data)),
                               rep(1,nrow(target_data)),
                               target_data$amount),
                             nrow=3,
                             byrow = T),
                  sense = c("<=",">=","<="),
                  rhs = c(exp_num+5,exp_num-5,exp_amount),
                  vtype = "B",
                  modelsense = "max")
      result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
      target_data[which(result$x==1),]
    }
    }else{
    select_result<-{
      a<-invest_pool[user_id%in%good_list$user_id,]
      a_num = nrow(a)
      a_amount = sum(a$amount)
      exp_num = (exp_num-a_num)%>%pmax(.,0)
      exp_amount = round(exp_amount-a_amount,2)
      b<-{
        target_data<-invest_pool[user_id%in%bad_list$user_id,]
        model<-list(obj = target_data$amount,
                    A = matrix(c(rep(1,nrow(target_data)),
                                 rep(1,nrow(target_data)),
                                 target_data$amount),
                               nrow=3,
                               byrow = T),
                    sense = c("<=",">=","<="),
                    rhs = c(exp_num-5,exp_num-5,exp_amount),
                    vtype = "B",
                    modelsense = "max")
        result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
        target_data[which(result$x==1),]
      }
    }
    print("there is badlist selection!")
  }
  
  select_result[,dt:=target_dt]
  select_result_summary<-select_result[,.(amount=sum(amount),num=.N),by=user_id]
  invest_pool<-invest_pool[!ord_no%in%select_result$ord_no,]
  now_data<-rbind(now_data,select_result)%T>%setorder(.,dt,user_id,trade_type)
  pool_summary<-pool_summary[select_result_summary,
                             on="user_id",
                             ":="(invest_amount = round(invest_amount-i.amount,2),
                                  invest_num = invest_num-i.num,
                                  judge_amount = round(judge_amount-i.amount,2))]
  
  result<-now_data[dt==target_dt & trade_type=="invest" & adjust==1,]%>%
    .[,.(user_num = uniqueN(user_id),num=.N,amount=sum(amount)),by=dt]
  print(paste(i,"finished!",
              "| exp_num:",wait_adjust$exp.num,
              "| exp_amount:",wait_adjust$exp.amount,
              "| actual_num:",result$num,
              "| actual_amount:",result$amount))
}


## 检查投资调整情况
View(now_data[adjust==1&trade_type=="invest"&dt<=end_dt&dt>=as.Date("2016-08-31"),]%>%
  .[,.(user_num=uniqueN(user_id),num=.N,amount=round(sum(amount),2)),by=dt]%>%
  exp_result[trade_type=="invest"&dt<=end_dt,c("dt","user_num","num","amount")][.,on="dt"]%>%
    .[,":="(diff_ratio=round(abs(i.amount-amount)/amount,4))])

write.csv(now_data,paste0(path,"newdata_invest_finished.csv"),row.names = F)

#############################################################   赎回调整 #############################
now_data<-read_csv(paste0(path,"newdata_invest_finished.csv"),col_types = cols(ord_no="c"))%>%as.data.table(.)
# raw_data<-read_csv("F:/rawdata06062.csv",col_types = cols(ord_no="c"))%>%as.data.table()%>%
#   .[,premium_change:=ifelse(trade_type=="invest",amount,-amount)]%>%
#   .[,":="(ftype="k",prd_id="YINKHQ")]

# now_data<-raw_data[trade_type=="invest",]%>%
#   rbind(.,my_data[trade_type=="redeem",-"isfix"])%T>%
#   setorder(.,dt,trade_type)
# 
# redeem_pool<-newdata[between(dt,first_dt,end_dt) & trade_type=="redeem" & isfix==0,-"isfix"]
premium_now<- data.table(user_id = unique(now_data$user_id))%>%
  .[premium_first,on="user_id",premium:=i.first_premium]%>%
  .[,premium:=replace(premium,is.na(premium),0)]%>%
  .[,dt:=first_dt-1]

safe_premium<-{
  danger_users<-now_data[between(dt,first_dt,end_dt) & trade_type=="redeem",]$user_id%>%unique(.)
  
  now_data[dt>=first_dt & user_id%in%danger_users,]%>%
    .[,.(premium_change=sum(premium_change)),by=.(user_id,dt)]%>%
    {
      a<-premium_now[user_id%in%danger_users,]%>%
        .[,premium_change:=premium*(1+daily_rate*1)]%>%
        .[,-"premium"]%>%
        .[,dt:=first_dt]
      rbind(.,a)%T>%
        setorder(.,user_id,-dt)%>%
        .[,safe_premium:=round(fun.safe_redeem_amount(premium_change),2),by=user_id]
    }%T>%
    setorder(.,user_id,dt)%T>%
    setnames(.,"dt","start_dt")%>%
    .[,end_dt:=dplyr::lead(start_dt,default = end_dt+1)-1,by=user_id]%>%
    .[safe_premium>0,]
}

################################ 赎回循环 ##################################  
for(i in 1:redeem_adjust_num){
  wait_adjust<-for_adjus_redeem[i,]
  target_dt<-wait_adjust$dt
  
  premium_now<-premium_now[,premium:=premium*(1+daily_rate*as.numeric(target_dt-dt))]%>%
    .[,dt:=target_dt]%>%
  {
    a<-now_data[dt==target_dt,]%>%
      .[,.(premium_change=sum(premium_change)),by=user_id]
    .[a,on="user_id",premium:=round(premium+premium_change,2)]
  }
  
  if(nrow(premium_now[premium< -200,])>0){
    stop("there is negative premium before set redeem! Please check!")
  }
  
  fixed_data<-now_data[dt==target_dt & trade_type=="redeem" & adjust==1,]
  fixed_data_summary<-fixed_data[,.(amount=sum(amount),num=.N),by=user_id]
  fixed_user_num = nrow(fixed_data_summary)
  fixed_num = sum(fixed_data_summary$num)
  fixed_amount = sum(fixed_data_summary$amount)
  
  exp_user_num = wait_adjust$exp.user_num - fixed_user_num
  exp_num = (wait_adjust$exp.num - fixed_num)%>%pmax(.,0)
  exp_amount = wait_adjust$exp.amount - fixed_amount
  
  avail_redeem_list<-{
    a<-safe_premium[start_dt<=target_dt & end_dt>=target_dt,c("user_id","safe_premium")]
    premium_now[a,on="user_id",safe_premium:=i.safe_premium]%>%
      .[,safe_premium:=replace(safe_premium,is.na(safe_premium),0)]%>%
      .[,max_redeem_amount:=round(premium-safe_premium,2)]%>%
      .[max_redeem_amount>0,]
  }
  
  target_data<-redeem_pool[user_id%in%avail_redeem_list$user_id,]
  
  target_data_summary<-target_data[,.(total_amount=sum(amount)),by=user_id]%>%
    .[avail_redeem_list[,c("user_id","max_redeem_amount")],
      on="user_id",
      max_redeem_amount:=i.max_redeem_amount]
  
  whole_avail_user<-target_data_summary[total_amount<=max_redeem_amount,]
  part_avail_user<-target_data_summary[total_amount>max_redeem_amount,]
  
  if(nrow(part_avail_user)>0){
    wait_select_num = nrow(part_avail_user)
    select_data<-redeem_pool[0]
    for(j in 1:wait_select_num){
      wait_select<-part_avail_user[j,]
      target_select_data<-redeem_pool[user_id==wait_select$user_id,]
      exp_select_amount = wait_select$max_redeem_amount
      select_temp<-{
        model<-list(obj = target_select_data$amount,
                    A = matrix(target_select_data$amount,
                               nrow=1,
                               byrow = T),
                    sense = c("<="),
                    rhs = c(exp_select_amount),
                    vtype = "B",
                    modelsense = "max")
        result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
        if(result$status=="OPTIMAL"){
          x<-result$x
          tmp<-target_select_data[which(x==1),]
        }else{
          tmp<-target_select_data[0]
        }
        tmp
      }
      select_data<-rbind(select_data,select_temp)
    }
  }
  
  target_data<-redeem_pool[user_id%in%whole_avail_user$user_id,]%>%rbind(.,select_data)
  
  print(paste("the target_pool amount : ",sum(target_data$amount)))
  
  select_result<-{
    model<-list(obj = target_data$amount,
                A = matrix(c(rep(1,nrow(target_data)),
                             rep(1,nrow(target_data)),
                             target_data$amount),
                           nrow=3,
                           byrow = T),
                sense = c("<=",">=","<="),
                rhs = c(exp_num+10,exp_num-10,exp_amount),
                vtype = "B",
                modelsense = "max")
    result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
    target_data[which(result$x==1),]
  }
  
  if(sum(select_result$amount)<=0.99*exp_amount){
    select_result<-{
      model<-list(obj = target_data$amount,
                  A = matrix(target_data$amount,
                             nrow=1,
                             byrow = T),
                  sense = c("<="),
                  rhs = c(exp_amount),
                  vtype = "B",
                  modelsense = "max")
      result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
      target_data[which(result$x==1),]
    }
  }
  
  select_result[,dt:=target_dt]
  redeem_pool<-redeem_pool[!ord_no%in%select_result$ord_no,]
  now_data<-rbind(now_data,select_result)%T>%setorder(.,dt,user_id,trade_type)
  
  premium_now<-{
    a<-select_result[,.(premium_change = sum(premium_change)),by=user_id]
    premium_now[a,on="user_id",premium:=round(premium+i.premium_change,2)]
  }
  
  result<-now_data[dt==target_dt & trade_type=="redeem" & adjust==1,]%>%
    .[,.(user_num = uniqueN(user_id),num=.N,amount=sum(amount)),by=dt]
  
  print(paste(i,"finished!",
              "| exp_num:",wait_adjust$exp.num,
              "| exp_amount:",wait_adjust$exp.amount,
              "| actual_num:",result$num,
              "| actual_amount:",result$amount))
}

## 检查赎回调整情况
View(now_data[adjust==1&trade_type=="redeem"&dt<=end_dt&dt>=as.Date("2016-08-31"),]%>%
       .[,.(user_num=uniqueN(user_id),num=.N,amount=round(sum(amount),2)),by=dt]%>%
       exp_result[trade_type=="redeem"&dt<=end_dt,c("dt","user_num","num","amount")][.,on="dt"]%>%
       .[,":="(diff_ratio=round(abs(i.amount-amount)/amount,4))])

write.csv(now_data,paste0(path,"newdata_finished.csv"),row.names = F)
## 检查整体数据的用户存量情况
premium_log<-now_data%T>%
  setorder(.,user_id,dt,trade_type)%>%
  .[,retain_days:=as.numeric(dplyr::lead(dt)-dt),by=user_id]%>%
  .[is.na(retain_days),retain_days:=as.numeric(end_dt-dt)]%>%
  .[,premium:=fun.premium_cal(premium_change,days = retain_days,rate = daily_rate),by=user_id]

premium_now<-premium_log[,.(premium=last(premium)),by=user_id]%>%.[,premium:=pmax(round(premium,2),0)]

write.csv(premium_now,paste0(path,"premium_now.csv"),row.names = F)


## 剩余订单信息
additional_ord<-newdata[!ord_no%in%now_data$ord_no,]
write.csv(additional_ord,paste0(path,"additional_ord.csv"),row.names = F)

additional_ord_summary<-additional_ord[,.(amount=sum(amount),num=.N),by=.(user_id,trade_type)]%>%
  dcast(.,user_id~trade_type,value.var=c("amount","num"))%>%
  {
    vars =names(.)%>%.[which(str_detect(.,"invest")|str_detect(.,"redeem"))]
    .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
  }

additional_ord_summary[,":="(diff_amount=abs(amount_invest-amount_redeem))]%>%
  .[,adjust_invest:=ifelse(amount_invest>=amount_redeem,1,0)]


write.csv(additional_ord_summary,paste0(path,"additional_ord_summary.csv"),row.names = F)


## 整体数据对比情况
total_data<-now_data[adjust==1&between(dt,first_dt,end_dt),]%>%
  .[,.(user_num=uniqueN(user_id),num=.N,amount=round(sum(amount),2)),by=.(dt,trade_type)]%>%
  .[exp_result[between(dt,first_dt,end_dt),],on=c("dt"="dt","trade_type"="trade_type")]%T>%{
  vars<-names(.)%>%.[which(str_detect(.,"i."))]
  setnames(.,vars,str_c("exp.",str_sub(vars,3)))
}%>%
  .[,":="(diff_user_num=user_num-exp.user_num,
          diff_num=num-exp.num,
          diff_amount=amount-exp.amount)]

write.csv(total_data,paste0(path,"total_data.csv"),row.names = F)



