
library(data.table)
library(magrittr)
library(lubridate)
source('~/rstudio/match150/(fun)fun.asset_deadline.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)fun.time_seq.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)fun.redeem_select_asset.r', encoding = 'UTF-8')
path<-"F:/Project/20170315资产匹配穿透150人调研/时间流模拟/"
n=150

#正常投资、赎回的处理权重
fun.weight<-function(amount,log_time,now_time,type=c("invest","redeem")){
  type_par<-switch(type,
                     "invest"=1,
                     "redeem"=1)
  time_weight=difftime(as.POSIXct(now_time),as.POSIXct(log_time),units = "days")%>%as.numeric(.)*type_par
  weight=log10(amount+1)*10+time_weight
  return(weight)
}

#由于赎回导致的已匹配资金解绑，产生的额外投资处理权重,type=0->活期，1->T+N
fun.extra_invest<-function(amount,log_time,now_time,type=c(0,1)){
  type_par<-switch(type+1,
                   0,
                   20)
  time_weight=difftime(as.POSIXct(now_time),as.POSIXct(log_time),units = "days")%>%as.numeric(.)*type_par
  weight=log10(amount+1)*10+time_weight
  return(weight)
}


############################## 0.模拟数据源加载 #################################
# 0.0时间流序列时间轴
Dseq<-seq.Date(from = as.Date("2017-03-15"),to = as.Date("2017-03-22"),by = "day")
HMSseq<-7:22
time_seq<-fun.time_seq(Dseq = Dseq,HMSseq = HMSseq)%>%c(.,as.POSIXct("2017-03-15 00:00:00"))%>%sort(.)

# 0.1匹配起点资产状态
asset_inital<-fread(paste0(path,"asset_list.csv"))
# 0.2匹配起点资产匹配信息
match_record_initial<-fread(paste0(path,"match_record.csv"))%>%
  .[,log_time:=as.POSIXct("2017-03-14 23:59:59")]

#资产整体数据
asset<-fread(paste0(path,"asset_simu_data.csv"))%>%
  .[,":="(create_time=as.POSIXct(create_time),
          end_time=as.POSIXct(end_time))]

# 0.3匹配起点用户资金序列状态，增加额度权重=log10(amount+1)
user_list<-fread(paste0(path,"user_list.csv"))%>%
  .[,log_time:=as.POSIXct("2017-03-14 23:59:59")]

regular<-user_list[,c("userid","unmatched_regular_premium","log_time")][
  unmatched_regular_premium>0,][
    ,":="(unmatched_premium=unmatched_regular_premium,
          unmatched_regular_premium=NULL)]

tPlus<-user_list[,c("userid","unmatched_tPlus_premium","log_time")][
  unmatched_tPlus_premium>0,][
    ,":="(unmatched_premium=unmatched_tPlus_premium,
          unmatched_tPlus_premium=NULL)]

current<-user_list[,c("userid","unmatched_current_premium","log_time")][
  unmatched_current_premium>0,][
    ,":="(unmatched_premium=unmatched_current_premium,
          unmatched_current_premium=NULL)]

# 0.3资产入库时间流整体
in_asset<-copy(asset)%>%
  .[create_time>=as.POSIXct("2017-03-15"),]%>%
  .[,deadline:=sapply(create_time,fun.asset_deadline)%>%as.POSIXct(.,origin="1970-01-01 00:00:00")]%>%
  .[,":="(unmatched_amount=corpusamount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]
# 0.4资产出库时间流整体
out_asset<-copy(asset)%>%
  .[end_time<=as.POSIXct("2017-03-22"),]
# 0.5投资时间流整体
invest<-fread(paste0(path,"invest_simu_data.csv"))%>%
  .[,":="(create_time=as.POSIXct(create_time))]
# 0.6赎回时间流整体
redeem<-fread(paste0(path,"redeem_simu_data.csv"))%>%
  .[,":="(create_time=as.POSIXct(create_time))]

# 0.7循环初始值
#----当前匹配信息表
match_status_now<-copy(match_record_initial)
#----当前资产信息表
asset_now<-asset[asset_inital,on=c("id"="id"),nomatch=0,mult="all"][,c("amount"):=NULL]%>%
  .[,deadline:=sapply(create_time,FUN = fun.asset_deadline)%>%as.POSIXct(.,origin="1970-01-01 00:00:00")]
#----当前用户信息表
user_list_now<-copy(user_list)

#----当前定期序列
end<-time_seq[1]
regular_now<-copy(regular)%>%.[,":="(redeemID=NA,weight=NA)]
#----当前T+N序列
tPlus_now<-copy(tPlus)%>%.[,":="(redeemID=NA,weight=NA)]
#----当前活期序列
current_now<-copy(current)%>%.[,":="(redeemID=NA,weight=NA)]
#----赎回记录总表，先创建空的
redeem_record<-data.table(id=0,
                          userid="0",
                          create_time=as.POSIXct("2017-03-23 0:00:00"),
                          amount=1000,
                          type=1,
                          status=1)%>%.[-1,]
#----赎回日志总表，先创建空的
redeem_log<-data.table(redeemID=0,
                       free_redeem_amount=5.00,
                       matched_redeem_amount=1000)%>%.[-1,]

#----匹配总表，先创建空的
match_record<-data.table()

############################# 1. 时间流处理 ############################
i=1
start=time_seq[i]
end=time_seq[i+1]
#----当前时间片内入库的资产
in_asset_seq<-in_asset[between(create_time,start,end),]
#----当前时间片内出库的资产
out_asset_seq<-out_asset[between(end_time,start,end),]
#----当前时间片内发起的投资
invest_seq<-invest[between(create_time,start,end),]%>%.[,":="(redeemID=NA,weight=NA)]
#----当前时间片内发起的赎回，加权重，加赎回序号，加赎回成功判定。status：1.未完成，2.已完成。
redeem_seq<-redeem[between(create_time,start,end),]%T>%
  setorder(.,create_time)%>%
  .[,":="(id=.I+nrow(redeem_record),status=1)]
redeem_record<-rbind(redeem_record,redeem_seq)
redeem_now<-redeem_record[status==1,]

########## 1.1 资产整理 ##########
# 1.1.1 出库资产处理
if(nrow(out_asset_seq)>0){
  #在资产队列里删除这些资产的信息
  asset_now<-asset_now[!(id%in%out_asset_seq$id),]
  match_status_now<-match_status_now[!(asset_id%in%out_asset_seq$id),]
  #在资金队列里添加与出库资产匹配的资金流
  #----总出库资金流
  out_money_flow<-match_status_now[asset_id%in%out_asset_seq$id,]%>%
    .[out_asset_seq[,c("id","end_time")],on=c("asset_id"="id"),nomatch=NA,mult="all"]%>%
    .[,":="(redeemID=NA,weight=NA)]
  
  #----定期出库资金流
  out_regular_flow<-out_money_flow[type=="regular",c("userid","amount","end_time","redeemID","weight")]%>%
    .[,":="(unmatched_premium=amount,
            amount=NULL,
            log_time=end_time,
            end_time=NULL)]
    
  #----T+N出库资金流
  out_tPlus_flow<-out_money_flow[type=="tPlus",c("userid","amount","end_time","redeemID","weight")]%>%
    .[,":="(unmatched_premium=amount,
            amount=NULL,
            log_time=end_time,
            end_time=NULL)]
  #----活期出库资金流
  out_current_flow<-out_money_flow[type=="current",c("userid","amount","end_time","redeemID","weight")]%>%
    .[,":="(unmatched_premium=amount,
            amount=NULL,
            log_time=end_time,
            end_time=NULL)]
  
  #----出库资金流与现有资金流合并，并按照权重进行排序
  regular_now<-rbind(regular_now,out_regular_flow)%T>%setorder(.,-weight)
  tPlus_now<-rbind(tPlus_now,out_tPlus_flow)%T>%setorder(.,-weight)
  current_now<-rbind(current_now,out_current_flow)%T>%setorder(.,-weight)
}

# 1.1.2 入库资产处理
if(nrow(in_asset_seq)>0){
  # 在资产队列里新增入库资产
  asset_now<-rbind(asset_now,in_asset_seq)
}

########## 1.2 资金处理 ##########
# 1.2.1 投资处理，进入相应队列表
for(k in 0:2){
  name<-switch(k+1,
               "current",
               "tPlus",
               "regular")
  tmp<-invest_seq[type==k,c("userid","create_time","amount","weight","redeemID")]%>%{
    colnames(.)<-c("userid","log_time","unmatched_premium","weight","redeemID")
    eval(parse(text = paste0("rbind(",name,"_now,.)")))
  }%>%.[,.(unmatched_premium=sum(unmatched_premium),
           log_time=min(log_time)),
        by=.(userid,redeemID)]%>%.[,remark:=NA]
  eval(parse(text = paste0(name,"_now<-tmp")))
}

# 1.2.2 赎回处理，更新相应资金队列表和资产队列表，甚至投资队列表
if(nrow(redeem_seq)>0){
  extra_invest<-data.table()
  extra_in_asset<-data.table()
  for(j in 1:nrow(redeem_seq)){
    redeem_focus<-redeem_seq[j,]
    #赎回类型
    type<-redeem_focus$type
    name<-switch(type+1,
                 "current","tPlus")
    cash_flow<-switch(type+1,
                      current_now,
                      tPlus_now)
    
    #先查找在未匹配的资金队列里的未匹配金额（只会有一条记录）
    focus<-cash_flow[userid==redeem_focus$userid&is.na(redeemID),]
    #检验这部分资金是否足够rest_redeem_amount==0
    if(nrow(focus)==0){
      rest_redeem_amount<-redeem_focus$amount
    }else{
      rest_redeem_amount<-ifelse(focus$unmatched_premium>=redeem_focus$amount,
                                 0,
                                 redeem_focus$amount-focus$unmatched_premium)
    }
      
    #如果资金足够则改变资金队列里的相应记录，改变赎回记录中的相应记录的status
    if(rest_redeem_amount==0){
      cash_flow[userid==redeem_focus$userid&is.na(redeemID),
                unmatched_premium:=unmatched_premium-redeem_focus$amount]
      redeem_record[id==redeem_focus$id,status:=2]
      redeem_log_temp<-data.table(redeemID=redeem_focus$id,
                                  free_redeem_amount=redeem_focus$amount,
                                  matched_redeem_amount=rest_redeem_amount)
    }
    #否则如果资金不够，只能从资产表中赎回
    else{
      cash_flow[userid==redeem_focus$userid&is.na(redeemID),
                unmatched_premium:=0]
      redeem_log_temp<-data.table(redeemID=redeem_focus$id,
                                  free_redeem_amount=redeem_focus$amount-rest_redeem_amount,
                                  matched_redeem_amount=rest_redeem_amount)
        
      #确定剩余赎回所对应的资产表
      asset_focus<-match_status_now[userid==redeem_focus$userid&type==name,
                                    c("userid","type","amount","asset_id")]%>%
        asset_now[,c("id","unmatched_amount","avail_num","deadline")][.,on=c("id"="asset_id"),nomatch=0,mult="all"]
      
      redeem_output<-fun.redeem_select_asset(rest_redeem_amount,asset_focus)
      
      extra_in_asset_temp<-redeem_output$extra_asset%>%.[,":="(redeemID=redeem_focus$id,
                                                          create_time=redeem_focus$create_time)]
      
      extra_in_asset<-rbind(extra_in_asset,extra_in_asset_temp)
      
      if(redeem_output$extra_label){
        extra_invest_temp<-redeem_output$extra_invest%>%.[,":="(userid=redeem_focus$userid,
                                                           create_time=redeem_focus$create_time,
                                                           amount=amount,
                                                           type=type,
                                                           redeemID=NA,
                                                           remark="extra")]%>%
          .[,weight:=fun.extra_invest(amount = amount,
                                      log_time = create_time,
                                      now_time = end,
                                      type = type),by=.I]
        
        extra_invest<-rbind(extra_invest,extra_invest_temp)
      }
    }
    
    redeem_log<-rbind(redeem_log,redeem_log_temp)
    if(type==1){
      tPlus_now<-cash_flow
    }else{
      current_now<-cash_flow
    }
  }
}

# 1.2.3 更新资金队列

















