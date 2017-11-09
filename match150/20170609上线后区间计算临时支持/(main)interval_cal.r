library(data.table)
library(magrittr)
library(stringr)
library(RMySQL)

source('~/rstudio/match150/(fun)match_temp5.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)fun.judge_const.r')
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)

fun.type<-function(x){
  switch(x,
         "1"="current",
         "2"="tPlus",
         "3"="regular")
}
asset_conn<-MySQL_conn_select(connect_name = "asset_product")
cash_conn<-MySQL_conn_select(connect_name = "cash_product")
n=200
#################################### 0.1 参数提取 #########################################
z3<-"SELECT id,corpusAmount as amount FROM ast_loan_asset WHERE yn = 0 AND STATUS IN(400,600) and corpusAmount>0"
res<-dbSendQuery(asset_conn,z3)
asset_data_total<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)

z1<-"select user_id as userid,account_type as type,unmatched_amount 
from ast_money_account where yn=0 and unmatched_amount>0"
res<-dbSendQuery(asset_conn,z1)
user_match_data<-dbFetch(res,n=-1)%>%as.data.table(.)%>%
  .[,type:=as.character(type)%>%sapply(.,fun.type)]%>%
  dcast(.,userid~type,value.var=c("unmatched_amount"),fill=0)%T>%
  {
    vars = names(.)%>%.[which(str_detect(.,"regular")|str_detect(.,"tPlus")|str_detect(.,"current"))]
    setnames(.,vars,str_c("unmatched_",vars,"_premium"))
  }
# names in user_match_data :userid,unmatched_amount_regular,unmatched_amount_tPlus,unmatched_amount_current
dbClearResult(res)
dbDisconnect(asset_conn)

asset_data<-copy(asset_data_total)%T>%
  setnames(.,"amount","unmatched_amount")%>%
  .[,avail_num:=n]%>%
  .[,avg_avail_amount:=round(unmatched_amount/avail_num,2)]%T>%
  setorder(.,-unmatched_amount)
asset_list<-copy(asset_data)

z4<-"SELECT 	user_id as userid,
		un_match_amount+match_amount+asset_out_amount as premium,
un_match_amount+match_amount+asset_out_amount - cur_regular_money as premium_current,
cur_regular_money as premium_regular 
FROM user_account_info
WHERE (match_amount + un_match_amount + asset_out_amount) > 0"
res<-dbSendQuery(cash_conn,z4)
user_premium_data<-dbFetch(res,n=-1)%>%as.data.table(.)%>%
{
  if(!"premium_tPlus"%in%names(.)){
    .[,premium_tPlus:=0.0]
  }else{.}
}
dbClearResult(res)
dbDisconnect(cash_conn)
#################################### 0.2 参数计算 #########################################
user_data<-user_match_data[user_premium_data,on="userid",nomatch=NA]%>%
{
  vars = names(.)%>%.[which(str_detect(.,"unmatched"))]
  .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
}

target_ratio=ceiling(sum(asset_data_total$amount)/sum(user_data$premium)*100)/100

user_list_pre<-copy(user_data)%>%
{
  if(!c("unmatched_regular_premium")%in%names(.)){
    .[,unmatched_regular_premium:=0.0]
  }else{.}
}%>%
{
  if(!"unmatched_current_premium"%in%names(.)){
    .[,unmatched_current_premium:=0.0]
  }else{.}
}%>%
{
  if(!"unmatched_tPlus_premium"%in%names(.)){
    .[,unmatched_tPlus_premium:=0.0]
  }else{.}
}


##  缩放区间以开关释放待匹金额
adjust_ratio=c(0,.94,.96,.98,1)
# breaks = c(0,100,17978.61,50057.10,252671.92,Inf)
breaks = fun.wise_break(user_list = user_list_pre,
                        asset_data_total = asset_data_total,
                        adjust_ratio = adjust_ratio,
                        safe_amount = 50000000,
                        slim_contribution = c(3,3,4))


user_list<-copy(user_list_pre)%>%
  .[,adjust_ratio:=cut(premium,
                       breaks=breaks,
                       labels=adjust_ratio,
                       right = F)%>%as.character(.)%>%as.numeric(.)]%>%
  # 剩余待匹活期为（存量*比例-定期-TN）-已匹活期，再取正值
  .[,unmatched_current_premium:=
      (premium*target_ratio-premium_regular-premium_tPlus)*adjust_ratio-
      (premium_current-unmatched_current_premium_formal)]%>%
  .[,unmatched_current_premium:=(pmax(unmatched_current_premium,0)/1)%>%floor(.)*1]%>%
  .[,current_hold:=round(unmatched_current_premium_formal-unmatched_current_premium,2)]

user_list_snap<-copy(user_list)
# write.csv(user_list[,c("userid","unmatched_current_premium")],"F:/newdata.csv",row.names = F)
all(user_list_pre$unmatched_current_premium>=user_list_snap$unmatched_current_premium)
sum(user_list_snap$unmatched_current_premium)

















#################################### 1 匹配主循环 #########################################
regular_seq<-user_list[unmatched_regular_premium>0,c("userid","unmatched_regular_premium")]%T>%
  setnames(.,"unmatched_regular_premium","unmatched_premium")%T>%
  setorder(.,-unmatched_premium)

tPlus_seq<-user_list[unmatched_tPlus_premium>0,c("userid","unmatched_tPlus_premium")]%T>%
  setnames(.,"unmatched_tPlus_premium","unmatched_premium")%T>%
  setorder(.,-unmatched_premium)

current_seq<-user_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
  setnames(.,"unmatched_current_premium","unmatched_premium")%T>%
  setorder(.,-unmatched_premium)

# r_regular=ceiling((nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num))/0.05)*0.05

r_regular=nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num)


r_tPlus=ceiling((nrow(tPlus_seq)/(sum(asset_list[unmatched_amount>0,]$avail_num)-nrow(regular_seq)))/0.05)*0.05

match_record<-data.table()
for(i in 1:nrow(asset_list)){
  # wait_asset直接从asset_data选取，因此顺序不会发生变化
  wait_asset<-asset_data[i,]
  # 队列形成
  regular_seq<-user_list[unmatched_regular_premium>0,c("userid","unmatched_regular_premium")]%T>%
    setnames(.,"unmatched_regular_premium","unmatched_premium")%T>%
    setorder(.,-unmatched_premium)
  
  tPlus_seq<-user_list[unmatched_tPlus_premium>0,c("userid","unmatched_tPlus_premium")]%T>%
    setnames(.,"unmatched_tPlus_premium","unmatched_premium")%T>%
    setorder(.,-unmatched_premium)
  
  current_seq<-user_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
    setnames(.,"unmatched_current_premium","unmatched_premium")%T>%
    setorder(.,-unmatched_premium)
  
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
                   avail_num=n-uniqueN(temp$userid))]%>%
    .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]
  
  asset_list[id==wait_asset$id,
             ":="(unmatched_amount=wait_asset$unmatched_amount,
                  avail_num=wait_asset$avail_num,
                  avg_avail_amount=wait_asset$avg_avail_amount)]
  
  
  if(nrow(temp[type=="regular"])>0){
    user_list<-temp[type=="regular",c("userid","amount")][user_list,on=c("userid"="userid"),
                                                          nomatch=NA,
                                                          mult="all"]%>%
      .[,amount:=ifelse(is.na(amount),0,amount)]%>%
      .[,unmatched_regular_premium:=round(unmatched_regular_premium-amount,2)]%>%
      .[,amount:=NULL]
  }
  if(nrow(temp[type=="tPlus"])>0){
    user_list<-temp[type=="tPlus",c("userid","amount")][user_list,on=c("userid"="userid"),
                                                        nomatch=NA,
                                                        mult="all"]%>%
      .[,amount:=ifelse(is.na(amount),0,amount)]%>%
      .[,unmatched_tPlus_premium:=round(unmatched_tPlus_premium-amount,2)]%>%
      .[,amount:=NULL]
  }
  if(nrow(temp[type=="current"])>0){
    user_list<-temp[type=="current",c("userid","amount")][user_list,on=c("userid"="userid"),
                                                          nomatch=NA,
                                                          mult="all"]%>%
      .[,amount:=ifelse(is.na(amount),0,amount)]%>%
      .[,unmatched_current_premium:=round(unmatched_current_premium-amount,2)]%>%
      .[,amount:=NULL]
  }
  print(paste(i,"finished !"))
}




