library(data.table)
library(magrittr)
library(RMySQL)

conn<-dbConnect(MySQL(),
                host = "120.27.167.74",
                port = 80,
                user="datacenter_read",
                password='Zjy-yinker20150309',
                dbname="jianlc_asset")
my_conn<-dbConnect(MySQL(),
                   host = "127.0.0.1",
                   port = 3306,
                   user="root",
                   password='871226',
                   dbname="yinker")

z0<-"select	account_type,
		target_match_amount as amount
from ast_money_account
where yn=0 and account_type=1 and target_match_amount>0
union
select 	account_type,
unmatched_amount as amount
from ast_money_account
where yn=0 and account_type=3 and unmatched_amount>0
union
select account_type,
unmatched_amount as amount
from ast_money_account
where yn=0 and account_type=2 and unmatched_amount>0"
res<-dbSendQuery(conn,z0)
remain_cash<-dbFetch(res,n=-1)%>%as.data.table(.)%T>%setnames(.,"amount","unmatched_premium")
dbClearResult(res)

z1<-"SELECT COUNT(1) as asset_num,
sum(avail_num) as avail_num,
SUM(unmatched_amount) as asset_amount 
FROM ast_matching_asset_group 
WHERE yn = 0 AND STATUS IN(1)"
res<-dbSendQuery(conn,z1)
remain_asset<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)
dbDisconnect(conn)

current_seq<-remain_cash[account_type==1,]%>%setorder(.,-unmatched_premium)
tPlus_seq<-remain_cash[account_type==2,]%>%setorder(.,-unmatched_premium)
regular_seq<-remain_cash[account_type==3,]%>%setorder(.,-unmatched_premium)
################# 计算当前可匹资金和可匹资产 #################
now_time = Sys.time()
cash_vs_asset_result<-data.table(remain_current = sum(current_seq$unmatched_premium),
                                 remain_regular = sum(regular_seq$unmatched_premium),
                                 remain_tPlus = sum(tPlus_seq$unmatched_premium))%>%
  cbind(.,remain_asset[,-"avail_num"])%>%
  .[,log_time:=now_time]

dbWriteTable(my_conn,"cash_vs_asset",cash_vs_asset_result,append=T,row.names=F)
################# 计算当前可匹能力 #################
fun.judge_const<-function(a,b,avail_num,first_seq,second_seq,third_seq){
  m_a<-min(floor(a*avail_num),nrow(first_seq))
  m_b<-min(floor((avail_num-m_a)*b),nrow(second_seq))
  m_c<-min(avail_num-m_a-m_b,nrow(third_seq))
  
  judge_const<-
    (
      first_seq$unmatched_premium%>%
        head(.,m_a)%>%
        sum(.)
    )+
    (
      second_seq$unmatched_premium%>%
        head(.,m_b)%>%
        sum(.)
    )+
    (
      third_seq$unmatched_premium%>%
        head(.,m_c)%>%
        sum(.)
    )
  return(judge_const)
}

avail_num_list = seq(10,200,by=10)
nlen = length(avail_num_list)
max_avail_amount = rep(0,nlen)
avail_num = ifelse(nrow(remain_asset)==0,0,remain_asset$avail_num)
for(i in 1:nlen){
  num = avail_num_list[i]
  a = round(nrow(regular_seq)/(avail_num+num),4)%>%pmin(.,1)
  b = round(nrow(tPlus_seq)/(avail_num+num-nrow(regular_seq)),4)%>%pmin(.,1)
  max_avail_amount[i]=fun.judge_const(a = a,
                                      b = b,
                                      avail_num = num,
                                      first_seq = regular_seq,
                                      second_seq = tPlus_seq,
                                      third_seq = current_seq
                                      )
}

result<-data.table(asset_num = avail_num_list,
                   max_avail_amount = max_avail_amount,
                   log_time = now_time)

dbWriteTable(my_conn,"match_ability",result,append=T,row.names=F)
dbDisconnect(my_conn)





