library(DBI)
library(data.table)
library(magrittr)
library(stringr)
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)
source('~/rstudio/match150/(fun)match_temp5.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)fun.judge_const.r')
fun.type<-function(x){
  switch(x,
         "current","tPlus","regular")
}
# 常数
n=200
target_ratio = 0.85
#*********************** 数据提取 ******************************
asset_conn<-MySQL_conn_select("asset_product")
cash_conn<-MySQL_conn_select("cash_product")
# 提取已匹190人以上资产的匹配用户金额信息
z0<-"select	a.asset_id,
a.user_id as userid,
a.money_account_type,
sum(a.hold_amount) as amount
from ast_matched_record a
left join
(
  select 	asset_id,
  count(distinct user_id) as num
  from ast_matched_record
  where yn=0 and status=1
  group by asset_id
  having num>190
)b on a.asset_id=b.asset_id
  where a.yn=0 and a.status=1 and b.asset_id is not null
  group by a.asset_id,a.user_id,a.money_account_type"
  
# 提取当前参与匹配的未匹金额
z1<-"select user_id as userid,account_type as money_account_type,unmatched_amount as amount
from ast_money_account 
where yn=0 and unmatched_amount>0"

# 当前用户存量信息
z2<-"SELECT 	user_id as userid,
un_match_amount+match_amount+asset_out_amount as premium
FROM user_account_info
WHERE (match_amount + un_match_amount + asset_out_amount) > 0"

# 当前资产总金额
z3<-"SELECT sum(corpusAmount) as amount 
FROM ast_loan_asset WHERE yn = 0 AND STATUS IN(400,600) and corpusAmount>0"


res<-dbSendQuery(asset_conn,z0)
fullMatchedRecord<-dbFetch(res,n=-1)%>%as.data.table(.)%>%.[,id:=.I]
dbClearResult(res)

res<-dbSendQuery(asset_conn,z1)
wait_cash_seq<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)

res<-dbSendQuery(asset_conn,z3)
asset_total_amount<-dbFetch(res,n=-1)%>%.$amount
dbClearResult(res)
dbDisconnect(asset_conn)

res<-dbSendQuery(cash_conn,z2)
user_premium_data<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)
dbDisconnect(cash_conn)

# 资产存量比
cash_vs_asset = round(asset_total_amount/sum(user_premium_data$premium),2)

# 提取待释放资产的匹配人数和资产信息
assetMatchSummary<-fullMatchedRecord[,.(num=uniqueN(userid),amount=sum(amount)),by=asset_id]%>%
  .[,avg_match_amount:=round(amount/n)]
fullAssetList = assetMatchSummary$asset_id
# 用户匹配信息（已匹未匹金额，预期匹配比例）
user_list<-copy(user_premium_data)%>%
  .[wait_cash_seq[money_account_type==1,],on="userid",unmatched_amount:=i.amount]%>%
  .[,unmatched_amount:=replace(unmatched_amount,is.na(unmatched_amount),0)]%>%
  .[,exp_match_amount:=round(premium-unmatched_amount,2)]%>%
  .[,exp_match_ratio:=round(1-unmatched_amount/premium,2)]
#*********************** 释放与保留数据筛选 **************************
releaseRecord<-{
  # 数据情况汇总
  totalMatchSummary<-copy(fullMatchedRecord)%>%
    .[,type:=sapply(money_account_type,fun.type)]%>%
    dcast(.,asset_id+userid~type,value.var="amount",fill=0)%>%
    {
      vars = c("regular","tPlus","current")
      .[,total_amount:=rowSums(.SD,dims = 1),.SDcols=vars]%T>%
        setnames(.,vars,str_c(vars,"amount",sep="_"))
    }%>%
    .[assetMatchSummary,on="asset_id",avg_match_amount:=i.avg_match_amount]%>%
    setorder(.,asset_id,total_amount)%>%
    .[,rank:=seq_along(userid),by=asset_id]%>%
    {
      #avail_user_list<-user_list[premium<1000|exp_match_ratio>cash_vs_asset,]
      avail_user_list<-user_list[premium<1000,]
      .[avail_user_list[,c("userid")],on="userid"]
    }
  # 待释放用户名单信息筛选
  releaseList<-totalMatchSummary[(total_amount<5000&regular_amount<500&tPlus_amount<500&rank<=50),]
  # 待释放用户匹配金额筛选
  fullMatchedRecord[releaseList[,c("asset_id","userid")],on=c("asset_id","userid")]%>%
    setorder(.,asset_id)
}

# 汇总待匹资产
asset_data<-fullMatchedRecord[!id%in%releaseRecord$id,]%>%
  .[,.(matched_num=uniqueN(userid),matched_amount=sum(amount)),by=asset_id]%>%
  .[assetMatchSummary,on="asset_id",total_amount:=i.amount]%>%
  .[,":="(unmatched_amount=round(total_amount-matched_amount,2),
          avail_num=n-matched_num)]%>%
  .[,avg_amount:=round(unmatched_amount/avail_num)]%>%
  setorder(.,-avg_amount)%>%
  .[,c("asset_id","unmatched_amount","avail_num")]%>%
  setnames(.,"asset_id","id")
asset_list<-copy(asset_data)
# 汇总待匹资金（如果全都能匹配的话）
simu_cash_seq<-rbind(wait_cash_seq,releaseRecord[,c("userid","amount","money_account_type")])%>%
  .[,.(unmatched_premium=sum(amount)),by=.(userid,money_account_type)]

#################################### 1 匹配主循环 #########################################
simu_regular_seq<-simu_cash_seq[money_account_type==3,-"money_account_type"]%>%setorder(.,-unmatched_premium)
simu_tPlus_seq<-simu_cash_seq[money_account_type==2,-"money_account_type"]%>%setorder(.,-unmatched_premium)

r_regular= ceiling(nrow(simu_regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num)/0.05)*0.05
r_tPlus = ceiling((nrow(simu_tPlus_seq)/(sum(asset_list[unmatched_amount>0,]$avail_num)-nrow(simu_regular_seq)))/0.05)*0.05


regular_seq<-wait_cash_seq[money_account_type==3,-"money_account_type"]%T>%
  setorder(.,-amount)%T>%
  setnames(.,"amount","unmatched_premium")
tPlus_seq<-wait_cash_seq[money_account_type==2,-"money_account_type"]%T>%
  setorder(.,-amount)%T>%
  setnames(.,"amount","unmatched_premium")
current_seq<-wait_cash_seq[money_account_type==1,-"money_account_type"]%>%
  .[user_premium_data,on="userid",premium:=i.premium]%>%
  .[,unmatched_premium:=floor(premium*target_ratio-(premium-amount))%>%pmax(.,0)]%>%
  .[unmatched_premium>0,c("userid","unmatched_premium","amount")]%T>%
  setorder(.,-unmatched_premium)%T>%
  setnames(.,"amount","remain_amount")

initial_regular_seq<-copy(regular_seq)
initial_tPlus_seq<-copy(tPlus_seq)
initial_current_seq<-copy(current_seq)


match_record<-data.table()
for(i in 1:nrow(asset_list)){
  # wait_asset直接从asset_data选取，因此顺序不会发生变化
  wait_asset<-asset_data[i,]
  # 队列形成
  regular_seq<-regular_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  tPlus_seq<-tPlus_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  current_seq<-current_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  
  if(fun.judge_const(a=1,
                     b=1,
                     wait_asset,
                     first_seq = regular_seq,
                     second_seq = tPlus_seq,
                     third_seq = current_seq)>=wait_asset$unmatched_amount){
    a = 1
    b = 1
  }else if(fun.judge_const(a=r_regular,
                           b=1,
                           wait_asset,
                           first_seq = regular_seq,
                           second_seq = tPlus_seq,
                           third_seq = current_seq)>=wait_asset$unmatched_amount){
    a = r_regular
    b = 1
  }else if(fun.judge_const(a=r_regular,
                           b=r_tPlus,
                           wait_asset,
                           first_seq = regular_seq,
                           second_seq = tPlus_seq,
                           third_seq = current_seq)>=wait_asset$unmatched_amount){
    a = r_regular
    b = r_tPlus
  }else{
    next
  }
  # 1.2 定期匹配=>T+N匹配=>活期匹配
  
  temp<-match_temp5(unmatched_amount = wait_asset$unmatched_amount,
                    avail_num = wait_asset$avail_num,
                    first_seq = regular_seq,
                    second_seq = tPlus_seq,
                    third_seq = current_seq,
                    a = a,
                    b = b,
                    item = c("regular","tPlus","current"))%>%
    .[,asset_id:=wait_asset$id]%>%
    .[,id:=id+nrow(match_record)]
  
  match_record<-rbind(match_record,temp)
  
  
  ## 更新 user_list和asset_list
  wait_asset[,":="(unmatched_amount=round(unmatched_amount-sum(temp$amount),2),
                   avail_num=avail_num-uniqueN(temp$userid))]
  
  asset_list[id==wait_asset$id,
             ":="(unmatched_amount=wait_asset$unmatched_amount,
                  avail_num=wait_asset$avail_num)]
  
  if(nrow(temp[type=="regular"])>0){
    regular_seq<-regular_seq[temp[type=="regular",c("userid","amount")],
                             on="userid",
                             unmatched_premium:=round(unmatched_premium-i.amount,2)]
  }
  if(nrow(temp[type=="tPlus"])>0){
    tPlus_seq<-tPlus_seq[temp[type=="tPlus",c("userid","amount")],
                         on="userid",
                         unmatched_premium:=round(unmatched_premium-i.amount,2)]
  }
  if(nrow(temp[type=="current"])>0){
    current_seq<-current_seq[temp[type=="current",c("userid","amount")],
                             on="userid",
                             ":="(unmatched_premium=round(unmatched_premium-i.amount,2),
                                  remain_amount=round(remain_amount-i.amount,2))]
  }
  
  releaseCash<-releaseRecord[asset_id==wait_asset$id,]%T>%setnames(.,"amount","unmatched_premium")
  regular_seq<-rbind(regular_seq,releaseCash[money_account_type==3,c("userid","unmatched_premium")])%>%
    .[,.(unmatched_premium=sum(unmatched_premium)),by=userid]
  tPlus_seq<-rbind(tPlus_seq,releaseCash[money_account_type==2,c("userid","unmatched_premium")])%>%
    .[,.(unmatched_premium=sum(unmatched_premium)),by=userid]
  current_seq<-{
    a<-releaseCash[money_account_type==1,c("userid","unmatched_premium")]%>%
      .[,remain_amount:=unmatched_premium]
    rbind(current_seq,a)
  }%>%
    .[,.(remain_amount=sum(remain_amount)),by=userid]%>%
    .[user_premium_data,on="userid",premium:=i.premium]%>%
    .[,unmatched_premium:=floor(premium*target_ratio-(premium-remain_amount))%>%pmax(.,0)]%>%
    .[,c("userid","unmatched_premium","remain_amount")]
  
  print(paste(i,"finished !"))
}

# 最终的匹配记录
finalMatchRecord<-fullMatchedRecord[!id%in%releaseRecord$id,]%>%
  .[,type:=sapply(money_account_type,fun.type)]%>%
  .[,-"money_account_type"]%>%
  rbind(.,match_record)%>%
  .[,.(amount=sum(amount)),by=.(asset_id,userid,type)]

# 最终的资产匹配情况与释放前的比较
finalMatchSummary<-assetMatchSummary[,c("asset_id","num")]%>%
  .[finalMatchRecord[,.(final_num=uniqueN(userid)),by=asset_id],on="asset_id",final_num:=i.final_num]%>%
  .[is.na(final_num),final_num:=num]%>%
  .[,release_num:=num-final_num]


# 待匹资金的变化  
amountDiff<-{
  finalAmount = 
    sum(current_seq$unmatched_premium)+
    sum(regular_seq$unmatched_premium)+
    sum(tPlus_seq$unmatched_premium)
  initialAmount = 
    sum(initial_current_seq$unmatched_premium)+
    sum(initial_tPlus_seq$unmatched_premium)+
    sum(initial_regular_seq$unmatched_premium)
  finalAmount-initialAmount
}

# 用户匹配情况的变化
userMatchDiffer<-{
  finalUserList<-copy(user_premium_data)%>%
    .[current_seq,on="userid",unmatched_amount:=i.remain_amount]%>%
    .[,unmatched_amount:=replace(unmatched_amount,is.na(unmatched_amount),0)]%>%
    .[,exp_match_amount:=round(premium-unmatched_amount,2)]%>%
    .[,exp_match_ratio:=round(1-unmatched_amount/premium,2)]
  
  copy(user_list)%>%
    .[,c("userid","premium","unmatched_amount","exp_match_ratio")]%>%
    .[finalUserList,on="userid",":="(final_unmatched_amount=i.unmatched_amount,
                                     final_exp_match_ratio=i.exp_match_ratio)]%>%
    .[,diff_match_ratio:=round(final_exp_match_ratio-exp_match_ratio,2)]
}



  
  
  
  
  
  
  
  
  
  