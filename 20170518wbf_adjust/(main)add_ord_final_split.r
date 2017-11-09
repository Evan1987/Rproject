library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(gurobi)
library(tcltk)
path="F:/Project/20170518沃百富数据调整/result/"

# 原始结果剩余订单
additional_ord<-read_csv(paste0(path,"additional_ord.csv"),col_types = cols(ord_no="c"))%>%as.data.table(.)

additional_ord_summary<-additional_ord[,.(amount=sum(amount),num=.N),by=.(user_id,trade_type)]%>%
  dcast(.,user_id~trade_type,value.var=c("amount","num"))%>%
  {
    vars =names(.)%>%.[which(str_detect(.,"invest")|str_detect(.,"redeem"))]
    .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
  }%>%
  .[,":="(adjust_amount=abs(amount_invest-amount_redeem))]%>%
  .[,adjust_invest:=ifelse(amount_invest>=amount_redeem,1,0)]

# 筛选订单（自身无法完全冲抵的）
add_user_list<-additional_ord_summary[adjust_amount>0,]%>%setorder(.,-adjust_amount)
add_ord<-additional_ord[user_id%in%add_user_list$user_id,]
add_num<-nrow(add_user_list)
tol = 0.03
add_ord[,premium_cal:=ifelse(trade_type=="invest",1+tol,1)*premium_change]

# 正负冲抵的订单
slim_data<-add_ord[0]
pb <- tkProgressBar("进度","已完成 %", 0, 100)
for(i in 1:add_num){
  wait_slim<-add_user_list[i,]
  target_data<-add_ord[user_id==wait_slim$user_id,]
  
  select_result<-{
    model<-list(obj = target_data$amount,
                A = matrix(c(target_data$premium_cal,
                             target_data$premium_change),
                           nrow=2,
                           byrow = T),
                sense = c(">=","<="),
                rhs = c(0,0),
                vtype = "B",
                modelsense = "max")
    result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
    if(result$status=="OPTIMAL"){
      x<-result$x
      tmp<-target_data[which(x==1),]
    }else{
      tmp<-target_data[0]
    }
    tmp
  }
  slim_data<-rbind(slim_data,select_result)
  info<- sprintf("已完成 %d%%", round(i*100/add_num))  
  setTkProgressBar(pb, i*100/add_num, sprintf("进度 (%s)", info),info)
}


# 对冲抵订单进行统计
slim_data_summary<-slim_data[,.(premium_judge=sum(premium_change)),by=user_id]

# 最终的补充订单
add_ord_final<-add_ord[!ord_no%in%slim_data$ord_no,-"premium_cal"]
write.csv(add_ord_final,paste0(path,"add_ord_final.csv"),row.names = F)

# 全部的被筛减订单
slim_ord_all<-additional_ord[!ord_no%in%add_ord_final$ord_no,]
write.csv(slim_ord_all,paste0(path,"slim_ord_all.csv"),row.names = F)

################################### 临时调整 6.7 订单 #######################################

source('~/rstudio/20170518wbf_adjust/(fun)type_switch.r', encoding = 'UTF-8')
path="F:/Project/20170518沃百富数据调整/result/"
add_ord_final<-read_csv(paste0(path,"add_ord_final.csv"),col_types = cols(ord_no="c"))%>%as.data.table(.)
premium_now<-fread(paste0(path,"premium_now.csv"))
users<-unique(premium_now$user_id)
rawdata_0607<-fread(paste0("F:/Project/20170518沃百富数据调整/LJB_20170607.csv"))%T>%
  setnames(.,names(.),tolower(names(.)))%>%
  .[,ord_no:=as.character(ord_no)]%T>%
  setorder(.,dt,ord_no)%>%
  .[,ftype:=sapply(ftype,fun.ftype)]%>%
  .[,adjust:=sapply(ftype2,fun.ftype2)]%>%
  .[,trade_type:=sapply(trade_type,fun.ftype)]%>%
  .[,dt:=as.Date(as.character(dt),"%Y%m%d")]%>%
  .[,amount:=round(amount,2)]%>%
  .[,premium_change:=sapply(trade_type,fun.premium)*amount]%>%
  .[,-c("ftype2")]%>%
  .[(!user_id%in%users & trade_type=="invest")|adjust==0,isfix:=1]%>%
  .[,isfix:=replace(isfix,is.na(isfix),0)]

exp_result<-rawdata_0607[adjust==1,][,.(amount=sum(amount),num=.N,user_num=uniqueN(user_id)),by=trade_type]

add_ord_final<-rbind(add_ord_final,rawdata_0607[isfix==0,])

invest_pool<-add_ord_final[trade_type=="invest",]
redeem_pool<-add_ord_final[trade_type=="redeem",]
now_data<-rawdata_0607[isfix==1,]
now_data_summary<-now_data[,.(premium_change=sum(premium_change)),by=user_id]

premium_target<-data.table(user_id=unique(c(now_data_summary$user_id,users)))%>%
  .[premium_now,on="user_id",premium:=i.premium]%>%
  .[,premium:=replace(premium,is.na(premium),0)]%>%
  .[now_data_summary,on="user_id",premium_change:=i.premium_change]%>%
  .[,premium_change:=replace(premium_change,is.na(premium_change),0)]%>%
  .[,premium_end:=round(premium+premium_change,2)]

wait_add_invest<-premium_target[premium_end<0,]

#### invest
fixed_data<-now_data[trade_type=="invest" & adjust==1,][,.(amount=sum(amount),num=.N),by=dt]
fixed_num=fixed_data$num
fixed_amount=fixed_data$amount

exp_num = exp_result[trade_type=="invest",]$num-fixed_num
exp_amount = exp_result[trade_type=="invest",]$amount-fixed_amount

target_data<-invest_pool
select_invest_result<-{
  model<-list(obj=target_data$amount,
              A=matrix(c(rep(1,nrow(target_data)),
                         rep(1,nrow(target_data)),
                         target_data$amount),
                       nrow=3,byrow = T),
              sense = c(">=","<=","<="),
              rhs = c(exp_num-3,exp_num+3,exp_amount),
              vtype = "B",
              modelsense = "max")
  result<-gurobi(model=model,params = list(OutputFlag=0,timeLimit = 10))
  if(result$status=="OPTIMAL"){
    x<-result$x
    tmp<-target_data[which(x==1),]
  }else{
    tmp<-target_data[0]
  }
  tmp
}

add_ord_final<-add_ord_final[!ord_no%in%select_invest_result$ord_no,]
add_ord_summary<-add_ord_final[,.(amount=sum(amount),num=.N),by=.(user_id,trade_type)]%>%
  dcast(.,user_id~trade_type,value.var=c("amount","num"))%>%
  {
    vars =names(.)%>%.[which(str_detect(.,"invest")|str_detect(.,"redeem"))]
    .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
  }

now_data<-rbind(now_data,select_invest_result)

#### redeem
fixed_data<-now_data[trade_type=="redeem" & adjust==1,][,.(amount=sum(amount),num=.N),by=dt]
if(nrow(fixed_data)>0){
  fixed_num=fixed_data$num
  fixed_amount=fixed_data$amount
}else{
  fixed_num=0
  fixed_amount=0
}
exp_num = exp_result[trade_type=="redeem",]$num-fixed_num
exp_amount = exp_result[trade_type=="redeem",]$amount-fixed_amount

target_data<-redeem_pool[user_id%in%add_ord_summary[amount_invest==0,]$user_id,]
select_redeem_result<-{
  model<-list(obj=target_data$amount,
              A=matrix(c(rep(1,nrow(target_data)),
                         rep(1,nrow(target_data)),
                         target_data$amount),
                       nrow=3,byrow = T),
              sense = c(">=","<=","<="),
              rhs = c(exp_num-3,exp_num+3,exp_amount),
              vtype = "B",
              modelsense = "max")
  result<-gurobi(model=model,params = list(OutputFlag=0,timeLimit = 10))
  if(result$status=="OPTIMAL"){
    x<-result$x
    tmp<-target_data[which(x==1),]
  }else{
    tmp<-target_data[0]
  }
  tmp
}
add_ord_final<-add_ord_final[!ord_no%in%select_redeem_result$ord_no,]
add_ord_summary<-add_ord_final[,.(amount=sum(amount),num=.N),by=.(user_id,trade_type)]%>%
  dcast(.,user_id~trade_type,value.var=c("amount","num"))%>%
  {
    vars =names(.)%>%.[which(str_detect(.,"invest")|str_detect(.,"redeem"))]
    .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
  }


now_data<-rbind(now_data,select_redeem_result)

#### 结果数量检查
now_result<-now_data[adjust==1,][,.(amount=sum(amount),num=.N,user_num=uniqueN(user_id)),by=trade_type]


#### 结果存量检查
now_data_summary<-now_data[,.(premium_change=sum(premium_change)),by=user_id]
premium_check<-data.table(user_id=unique(c(now_data_summary$user_id,users)))%>%
  .[premium_now,on="user_id",premium:=i.premium]%>%
  .[,premium:=replace(premium,is.na(premium),0)]%>%
  .[now_data_summary,on="user_id",premium_change:=i.premium_change]%>%
  .[,premium_change:=replace(premium_change,is.na(premium_change),0)]%>%
  .[,premium_end:=round(premium+premium_change,2)]%>%
  .[premium_end<0,]%>%
  .[add_ord_summary,on="user_id",amount_invest:=i.amount_invest]%>%
  .[,amount_invest:=replace(amount_invest,is.na(amount_invest),0)]%>%
  .[amount_invest>0,]


write.csv(now_data,paste0(path,"0607data.csv"),row.names = F)
write.csv(add_ord_final,paste0(path,"add_ord_final0607.csv"),row.names = F)


################## 建立拆解计划 ########################
source('~/rstudio/20170518wbf_adjust/(fun)fun.calendar_split.r', encoding = 'UTF-8')
calendar_limit<-{
  redeem_invest_ratio=(sum(add_ord_summary$amount_redeem)/sum(add_ord_summary$amount_invest))%>%round(.,2)
  invest_limit = c(100,100,rep(200,4),100)*1e+4
  redeem_limit = redeem_invest_ratio*invest_limit
  data.table(id=0:6,invest_limit,redeem_limit)
}

# 详细计划
split_result<-fun.calendar_split(add_ord_final,start_day = as.Date("2017-06-08"),calendar_limit)

# 计划汇总
split_result_summary<-split_result[,.(amount=sum(amount),num=.N),by=.(dt,trade_type)]%>%
  dcast(.,dt~trade_type,value.var=c("amount","num"),fill=0)%>%
  .[,weekday:=weekdays(dt)]

write.csv(split_result,paste0(path,"split_result.csv"),row.names = F)

write.csv(split_result_summary,paste0(path,"split_result_summary.csv"),row.names = F)



