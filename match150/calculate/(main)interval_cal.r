library(data.table)
library(magrittr)
library(stringr)
library(RMySQL)

# source('~/rstudio/match150/(fun)match_temp5.r', encoding = 'UTF-8')
# source('~/rstudio/match150/(fun)fun.judge_const.r')
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)
source('~/rstudio/match150/calculate/(fun)fun.wise_slim.r', echo=TRUE)

asset_conn<-MySQL_conn_select(connect_name = "asset_product")
cash_conn<-MySQL_conn_select(connect_name = "cash_product")
#################################### 0.1 真实数据提取 #########################################
# 当前总资产状态
# z3<-"SELECT id,corpusAmount as amount FROM ast_loan_asset WHERE yn = 0 AND STATUS IN(400,600) and corpusAmount>0"
# res<-dbSendQuery(asset_conn,z3)
# asset_data_total<-dbFetch(res,n=-1)%>%as.data.table(.)
# dbClearResult(res)
# 
# asset_data<-copy(asset_data_total)%T>%
#   setnames(.,"amount","unmatched_amount")%>%
#   .[,avail_num:=n]%>%
#   .[,avg_avail_amount:=round(unmatched_amount/avail_num,2)]%T>%
#   setorder(.,-unmatched_amount)
# asset_list<-copy(asset_data)


# 当前总资产状态
z3<-"SELECT sum(corpusAmount) as amount FROM ast_loan_asset WHERE yn = 0 AND STATUS IN(400,600) and corpusAmount>0"
res<-dbSendQuery(asset_conn,z3)
asset_total_amount<-dbFetch(res,n=-1)%>%.$amount
dbClearResult(res)

# 当前待匹资产的总可匹配人数
z2<-"SELECT sum(avail_num) as num from ast_matching_asset_group WHERE unmatched_amount > 0 AND yn = 0"
res<-dbSendQuery(asset_conn,z2)
remainTotalNum<-dbFetch(res,n=-1)%>%.$num
dbClearResult(res)

# 当前未匹配用户资金
z1<-"select user_id as userid,account_type as type,unmatched_amount 
from ast_money_account where yn=0 and unmatched_amount>0"
res<-dbSendQuery(asset_conn,z1)
user_match_data<-dbFetch(res,n=-1)%>%as.data.table(.)
log_time = Sys.time()
# names in user_match_data :userid,unmatched_amount_regular,unmatched_amount_tPlus,unmatched_amount_current
dbClearResult(res)
dbDisconnect(asset_conn)

# 当前用户存量信息
z4<-"SELECT 	user_id as userid,
un_match_amount+match_amount+asset_out_amount as premium
FROM user_account_info
WHERE (match_amount + un_match_amount + asset_out_amount) > 0"
res<-dbSendQuery(cash_conn,z4)
user_premium_data<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)
dbDisconnect(cash_conn)
#################################### 0.2 削减参数计算 #########################################
# 汇总用户数据
user_data<-user_match_data[type==1,][user_premium_data,on="userid",premium:=i.premium]%>%
  .[,exp_match_amount:=(premium-unmatched_amount)]
first_seq = user_match_data[type==3&unmatched_amount>0,]$unmatched_amount%>%sort(.,decreasing=T)
second_seq = user_match_data[type==2&unmatched_amount>0,]$unmatched_amount%>%sort(.,decreasing=T)

fun.judge_match_ability<-function(total_avail_num,
                                  avail_num,
                                  first_seq,
                                  second_seq,
                                  third_seq){
  total_avail_num<-ifelse(is.na(total_avail_num),0,total_avail_num)%>%ifelse(.<avail_num,.+avail_num,.)
  third_seq<-sort(third_seq,decreasing=T)
  second_seq<-sort(second_seq,decreasing=T)
  first_seq<-sort(first_seq,decreasing=T)
  
  a = round(length(first_seq)/total_avail_num,4)%>%pmin(.,1)
  b = round(length(second_seq)/(pmax(total_avail_num-length(first_seq),0)+1),4)%>%pmin(.,1)
  
  m_a<-min(floor(a*avail_num),length(first_seq))
  m_b<-min(floor((avail_num-m_a)*b),length(second_seq))
  m_c<-min(avail_num-m_a-m_b,length(third_seq))
  
  judge_const<-
    (
      head(first_seq,m_a)%>%
        sum(.)
    )+
    (
      head(second_seq,m_b)%>%
        sum(.)
    )+
    (
      head(third_seq,m_c)%>%
        sum(.)
    )
  return(judge_const)
}

ratio = seq(0.7,1,0.01)
num = length(ratio)
amount = rep(0,num)
maxability = rep(0,num)
for(i in 1:num){
  r = ratio[i]
  remain_data<-copy(user_data)%>%
    .[,remain_amount:=(premium*r-exp_match_amount)%>%pmax(.,0)%>%floor(.)]
  amount[i] = sum(remain_data$remain_amount)
  third_seq = remain_data[remain_amount>0,]$remain_amount%>%sort(.,decreasing=T)
  maxability[i]=fun.judge_match_ability(total_avail_num = remainTotalNum,
                                        avail_num = 200,
                                        first_seq = first_seq,
                                        second_seq = second_seq,
                                        third_seq = third_seq)
  # maxability[i] ={
  #   a<-remain_data$remain_amount%>%sort(.,decreasing=T)
  #   sum(head(a,200))
  # }
}

result = data.table(ratio,amount,maxability,log_time=log_time)


# 当前资产存量比（系统变量）
target_ratio=ceiling(asset_total_amount/sum(user_data$premium)*100)/100


