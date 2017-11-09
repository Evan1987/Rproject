library(data.table)
library(magrittr)
library(stringr)
library(RMySQL)
source('~/rstudio/match150/20170613临时资产置换/(fun)trans_temp.r', encoding = 'UTF-8', echo=TRUE)
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)

# asset_conn<-MySQL_conn_select("dev2",db_name = "jianlc_asset_test")

asset_conn<-MySQL_conn_select("asset_product")
cash_conn<-MySQL_conn_select("cash_product")

z0<-"select	user_id as userid,
		asset_id,
money_account_type as type,
sum(hold_amount) as amount
from ast_matched_record
where status=1 and yn=0 
group by userid,asset_id,money_account_type"
res<-dbSendQuery(asset_conn,z0)
match_record<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)

z1<-"select	id,
		corpusAmount,
aunualInterestRate as rate
from ast_loan_asset
WHERE yn = 0 AND STATUS IN(400,600)"
res<-dbSendQuery(asset_conn,z1)
asset_data<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)

dbDisconnect(asset_conn)


z2<-"select	user_id as userid,
		amount,
`time` as num
from regular_info
where yn=0 and status=10"

res<-dbSendQuery(cash_conn,z2)
regular_info<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)

dbDisconnect(cash_conn)
####################################################################################################

fun.type<-function(x){
  switch(x,"1"="current","3"="regular")
}
exp_current_rate = 5

n=200
asset_data<-asset_data[!is.na(rate),]

path="F:/Project/20170603资产匹配穿透200人上线/20170613资产临时置换/"
emergency_users<-fread(paste0(path,"userid.csv"))

# 目标利率
regular_rate<-fread(paste0(path,"regular_rate_exp.csv"))
# 用户定期汇总评价
regular_summary<-copy(regular_info)%>%
  .[,value:=num*amount]%>%
  .[,.(mix_value=sum(value),mix_amount=sum(amount)),by=userid]%>%
  .[,avg_num:=round(mix_value/mix_amount/0.5)*0.5]%>%
  .[regular_rate,on=c("avg_num"="num")]


match_record<-match_record[asset_data,on=c("asset_id"="id"),rate:=i.rate]%>%
  .[,daily_interest:=rate/100/365*amount]%>%
  .[,isfix:=0]

match_summary<-match_record[,
                            .(total_amount=sum(amount),total_daily_interest=sum(daily_interest)%>%round(.,2)),
                            by=.(userid,type)]%>%
  .[,type:=as.character(type)]%>%
  .[,type:=sapply(type,fun.type)]%>%
  dcast(.,userid~type,value.var=c("total_amount","total_daily_interest"),fill=0)%>%
  .[regular_summary,on="userid",":="(exp_regular_rate=i.rate,avg_num=i.avg_num)]%>%
  .[,exp_regular_rate:=replace(exp_regular_rate,is.na(exp_regular_rate),0)]%>%
  .[,avg_num:=replace(avg_num,is.na(avg_num),0)]%>%
  .[,general_regular_rate:=round(total_daily_interest_regular/(total_amount_regular+0.01)*365*100,2)]%>%
  .[,general_current_rate:=round(total_daily_interest_current/(total_amount_current+0.01)*365*100,2)]%>%
  .[,general_rate:=round((total_daily_interest_current+total_daily_interest_regular)/
                           (total_amount_current+total_amount_regular+0.01)*100*365,2)]%>%
  .[,general_exp_rate:=round((total_amount_current*exp_current_rate+total_amount_regular*exp_regular_rate)/
                               (total_amount_current+total_amount_regular+0.01),2)]


regularFocus<-match_summary[avg_num>0 &total_amount_regular>0,c("userid",
                                        "total_amount_current",
                                        "total_amount_regular",
                                        "exp_regular_rate",
                                        "avg_num",
                                        "general_regular_rate",
                                        "general_current_rate",
                                        "general_rate",
                                        "general_exp_rate")]%>%
  .[emergency_users,on="userid",isE:=i.isE]%>%
  .[,isE:=replace(isE,is.na(isE),0)]%>%
  .[,diff_reg_rate:=general_regular_rate-exp_regular_rate]%>%
  .[,diff_rate:=general_rate-general_exp_rate]%T>%
  setorder(.,-avg_num,diff_rate,diff_reg_rate)

regularFocusBad<-regularFocus[diff_reg_rate<0 & diff_rate<0,]
quantile(regularFocusBad$diff_rate,probs = seq(0,1,0.1))

regularFocusWorse<-regularFocusBad[diff_rate<=-1.65,]



wait_up<-match_summary[general_rate<(general_exp_rate-0.5) & general_exp_rate>0,]

wait_down<-match_summary[general_rate>general_exp_rate+0.05 & 
                           general_exp_rate<6 &
                           general_rate>6 &
                           general_current_rate>6 &
                           (!userid%in%wait_up$userid),]

wanna_up<-match_record[userid%in%wait_up$userid & type==3,]%>%
  .[regular_summary,on="userid",exp_rate:=i.rate]%>%
  .[,exp_rate:=replace(exp_rate,is.na(exp_rate),0)]%>%
  .[,diff_rate:=rate-exp_rate]%>%
  .[diff_rate< -0.5,]%>%
  .[emergency_users,on="userid",isE:=1]%>%
  .[,isE:=replace(isE,is.na(isE),0)]%T>%
  setorder(.,-isE,diff_rate,-exp_rate,-amount)%>%
  .[,id:=.I]
wanna_up_snap<-copy(wanna_up)

wanna_down<-match_record[userid%in%wait_down$userid & type==1 & rate>6.3,]%T>%
  setorder(.,-rate,-amount)%>%
  .[,exp_rate:=exp_current_rate]%>%
  .[,id:=.I]


adjust_num = nrow(wanna_up_snap)
trans_log<-data.table()

for(i in 1:adjust_num){
  wait_trans<-wanna_up_snap[i,]
  asset_match_status<-match_record[,.(avail_num=n-uniqueN(userid)),by=asset_id]
  user_match_status<-match_record[,.(total_amount=sum(amount)),by=.(userid,asset_id)]
  
  wait_trans<-wait_trans[asset_match_status,on="asset_id",avail_num:=i.avail_num]%>%
    .[user_match_status,on=c("userid"="userid","asset_id"="asset_id"),total_amount:=i.total_amount]
  
  if(wait_trans$avail_num==0&wait_trans$amount<wait_trans$total_amount){
    next
  }
  if(nrow(wanna_down)==0){
    break
  }
  
  wanna_down_list<-wanna_down[rate>wait_trans$rate,]%>%
  {
    if(nrow(.)>0){
      .[,":="(match.up=ifelse(rate>=wait_trans$exp_rate,
                              ifelse(wait_trans$exp_rate>8,0,0.5*(rate-wait_trans$exp_rate)),
                              2*(wait_trans$exp_rate-rate)),
              match.amount=(abs(amount-wait_trans$amount)/wait_trans$amount))]%>%
        .[,punishment:=match.up+7*match.amount]%T>%  
        #惩罚函数，用来决定哪些用户资产优先来与之置换
        setorder(.,punishment)
    }else{next}
  }%>%
    .[asset_match_status,on="asset_id",avail_num:=i.avail_num]%>%
    .[user_match_status,on=c("userid"="userid","asset_id"="asset_id"),total_amount:=i.total_amount]
  
  temp<-trans_temp(wait_trans,wanna_down_list)
  
  if(nrow(temp)==0){
    next
  }
  temp[,trans_no:=i]
  adjust_temp<-head(temp,2)%>%{
    vars = c("userid","asset_id","type")
    cbind(.[1,]%>%.[,.SD,.SDcols=vars]%T>%setnames(.,vars,str_c(vars,"1")),
          .[2,-"id"]%T>%setnames(.,vars,str_c(vars,"2")))
  }%>%.[,amount:=abs(amount)]
  wanna_down<-wanna_down[temp[amount<0,],
                         on=c("asset_id"="asset_id",
                              "userid"="userid",
                              "type"="type"),
                         amount:=round(amount+i.amount,2)]%>%
    .[amount>0,]
  wanna_up<-wanna_up[temp[amount<0,],
                     on=c("asset_id"="asset_id",
                          "userid"="userid",
                          "type"="type"),
                     amount:=round(amount+i.amount,2)]%>%
    .[amount>0,]

  match_record<-{
    a<-match_record[isfix==0,][temp[amount<0,],
                               on=c("asset_id"="asset_id",
                                    "userid"="userid",
                                    "type"="type"),
                               amount:=round(amount+i.amount,2)]
    b<-temp[,-"trans_no"][amount>0,isfix:=1]
    d<-match_record[isfix==1,]
    rbindlist(list(a,b,d),fill = T)
  }%>%.[amount>0,]
  
  trans_log<-rbind(trans_log,adjust_temp)
  print(paste(i,"finished!"))
}
