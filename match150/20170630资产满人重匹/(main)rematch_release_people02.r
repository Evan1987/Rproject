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
path = "F:/Project/20170603资产匹配穿透200人上线/20170630资产满人数释放/"
n=200
target_ratio = 0.85
rules<-list(breaks=c(0,80,200,300,500,700,1000,2000,3000,5000,7000,10000,30000,50000,Inf),
            bigger=c(1,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05,0.06,0.065,0.07,0.075),
            smaller=c(1,0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.055,0.06,0.065,0.07))
################################### 数据提取 #########################################
# asset_conn<-MySQL_conn_select("asset_product")
# cash_conn<-MySQL_conn_select("cash_product")
# local_conn<-MySQL_conn_select("local")
# # 提取已匹190人以上资产的匹配用户金额信息
# z0<-"select	a.asset_id,
# a.user_id as userid,
# a.money_account_type,
# sum(a.hold_amount) as amount
# from ast_matched_record a
# left join
# (
#   select 	asset_id,
#   count(distinct user_id) as num
#   from ast_matched_record
#   where yn=0 and status=1
#   group by asset_id
#   having num>190
# )b on a.asset_id=b.asset_id
#   where a.yn=0 and a.status=1 and b.asset_id is not null
#   group by a.asset_id,a.user_id,a.money_account_type"
# 
# # 提取当前参与匹配的未匹金额
# z1<-"select user_id as userid,account_type as money_account_type,unmatched_amount as amount
# from ast_money_account 
# where yn=0 and unmatched_amount>0"
# 
# # 当前用户存量信息
# z2<-"SELECT 	user_id as userid,
# un_match_amount+match_amount+asset_out_amount as premium
# FROM user_account_info
# WHERE (match_amount + un_match_amount + asset_out_amount) > 0"
# 
# # 当前资产情况
# z3<-"select	a.*,
# 	200-b.matched_num as avail_num
# from
# (
# select	id,
# corpusAmount,
# aunualInterestRate as rate
# from ast_loan_asset 
# where yn=0 and status in (400,600) and corpusAmount>0
# )a
# left join
# (
# select	asset_id,
# count(distinct user_id) as matched_num
# from ast_matched_record
# where yn=0 and status=1
# group by asset_id
# )b on a.id=b.asset_id"
# 
# # 全部资产的匹配情况
# z4<-"select	asset_id,
# 		user_id as userid,
# 		money_account_type,
# 		sum(hold_amount) as amount
# from ast_matched_record 
# where yn=0 and status=1 
# group by asset_id,user_id,money_account_type"
# 
# 
# res<-dbSendQuery(asset_conn,z0)
# fullMatchedRecord<-dbFetch(res,n=-1)%>%as.data.table(.)%>%.[,id:=.I]
# dbClearResult(res)
# 
# res<-dbSendQuery(asset_conn,z1)
# wait_cash_seq<-dbFetch(res,n=-1)%>%as.data.table(.)
# dbClearResult(res)
# 
# res<-dbSendQuery(asset_conn,z3)
# asset_info<-dbFetch(res,n=-1)%>%as.data.table(.)
# dbClearResult(res)
# dbDisconnect(asset_conn)
# 
# res<-dbSendQuery(cash_conn,z2)
# user_premium_data<-dbFetch(res,n=-1)%>%as.data.table(.)
# dbClearResult(res)
# dbDisconnect(cash_conn)
# 
# res<-dbSendQuery(local_conn,z4)
# matched_record<-dbFetch(res,n=-1)%>%as.data.table(.)
# dbClearResult(res)
# dbDisconnect(local_conn)
#
# asset_info[matched_record[,.(num=uniqueN(userid)),by=asset_id],on=c("id"="asset_id"),avail_num:=n-num]
# write.csv(asset_info,paste0(path,"asset_info.csv"),row.names = F)
# write.csv(fullMatchedRecord,paste0(path,"fullMatchedRecord.csv"),row.names = F)
# write.csv(user_premium_data,paste0(path,"user_premium_data.csv"),row.names = F)
# write.csv(wait_cash_seq,paste0(path,"wait_cash_seq.csv"),row.names = F)
# write.csv(matched_record,paste0(path,"matched_record.csv"),row.names = F)


##################### 数据读取 ####################

asset_info<-fread(paste0(path,"asset_info.csv"))
fullMatchedRecord<-fread(paste0(path,"fullMatchedRecord.csv"))
matched_record<-fread(paste0(path,"matched_record.csv"))
user_premium_data<-fread(paste0(path,"user_premium_data.csv"))
wait_cash_seq<-fread(paste0(path,"wait_cash_seq.csv"))

# 资产存量比
cash_vs_asset = round(sum(asset_info$corpusAmount)/sum(user_premium_data$premium),2)

# 用户匹配信息（已匹未匹金额，预期匹配比例）
user_list<-copy(user_premium_data)%>%
  .[wait_cash_seq[money_account_type==1,],on="userid",unmatched_amount:=i.amount]%>%
  .[,unmatched_amount:=replace(unmatched_amount,is.na(unmatched_amount),0)]%>%
  .[,exp_match_amount:=round(premium-unmatched_amount,2)]%>%
  .[,exp_match_ratio:=round(1-unmatched_amount/premium,2)]

# 计算待释放资产所匹配用户的可释放金额
full_user_list<-user_list[userid%in%unique(fullMatchedRecord$userid),]%>%
  .[exp_match_ratio>=cash_vs_asset,
    ratio:=cut(premium,breaks=rules$breaks,
               labels = rules$bigger,
               right = F)%>%as.character(.)%>%as.numeric(.)]%>%
  .[exp_match_ratio<cash_vs_asset,
    ratio:=cut(premium,
               breaks=rules$breaks,
               labels = rules$smaller,
               right = F)%>%as.character(.)%>%as.numeric(.)]%>%
  .[,releaseAmount:=floor(exp_match_amount*ratio)]

######################### 直接释放与保留数据筛选 #########################
releaseRecord<-{
  # 数据情况汇总
  totalMatchSummary<-copy(fullMatchedRecord)%>%
    .[,type:=sapply(money_account_type,fun.type)]%>%
    dcast(.,asset_id+userid~type,value.var="amount",fill=0)%>%
    {
      vars = c("regular","tPlus","current")
      .[,total_amount:=rowSums(.SD,dims = 1),.SDcols=vars]%T>%
        setnames(.,vars,str_c(vars,"amount",sep="_"))
    }%T>%
    setorder(.,userid,total_amount)%>%
    .[,cum_amount:=cumsum(total_amount),by=userid]%>%
    .[full_user_list,on="userid",releaseAmount:=i.releaseAmount]
  
  releaseList<-totalMatchSummary[cum_amount<=releaseAmount,]
  # 待释放用户匹配金额筛选
  fullMatchedRecord[releaseList[,c("asset_id","userid")],on=c("asset_id","userid")]%>%
    setorder(.,asset_id)
}

# 汇总待匹资产
asset_data<-fullMatchedRecord[!id%in%releaseRecord$id,]%>%
  .[,.(matched_num=uniqueN(userid),matched_amount=sum(amount)),by=asset_id]%>%
  .[asset_info,on=c("asset_id"="id"),total_amount:=i.corpusAmount]%>%
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
finalMatchSummary<-asset_info[,c("id","avail_num")]%>%
  .[finalMatchRecord[,.(final_avail_num=n-uniqueN(userid)),by=asset_id],
    on=c("id"="asset_id"),
    final_avail_num:=i.final_avail_num]%>%
  .[is.na(final_avail_num),final_avail_num:=avail_num]%>%
  .[,release_num:=final_avail_num-avail_num]


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



#################### 结果打印 #####################
# amountDiff: -57454587
write.csv(finalMatchSummary,paste0(path,"result/finalMatchSummary.csv"),row.names = F)
write.csv(finalMatchRecord,paste0(path,"result/finalMatchRecord.csv"),row.names = F)
write.csv(userMatchDiffer,paste0(path,"result/userMatchDiffer.csv"),row.names = F)



  
  
  
  
  