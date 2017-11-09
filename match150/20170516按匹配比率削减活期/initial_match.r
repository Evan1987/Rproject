library(data.table)
library(magrittr)
library(stringr)
library(RODBC)
source('~/rstudio/!custom/(fun)mysqlconn.R')
source('~/rstudio/match150/(fun)match_temp.r', encoding = 'UTF-8')
conn<-mysqlconn("mysql_settings.csv")

n=200
# 资金序列总表
rawdata<-sqlQuery(conn,"select user_id,account_type,amount from ast_money_account")%>%
  as.data.table(.)%T>%
  {names(.)<-c("userid","type","amount")}
# 资产序列总表
asset_list<-sqlQuery(conn,"select asset_id as id,amount from ast_matching_asset_group")%>%
  as.data.table(.)%>%
  .[,avail_num:=n]%>%
  .[,unmatched_amount:=amount]%>%
  .[,avg_avail_amount:=round(amount/avail_num,2)]%T>%
  setorder(.,-unmatched_amount)
asset_data<-copy(asset_list)

# 当前资产资金比例
target_ratio=ceiling(sum(asset_data$unmatched_amount)/sum(rawdata$amount)*100)/100

# 建立用户资金序列表
user_list<-dcast(rawdata,userid~type,value.var = "amount",fill=0)%T>%
{names(.)<-c("userid","unmatched_current_premium_formal","unmatched_regular_premium")}%>%
  .[,unmatched_tPlus_premium:=0]%>%
  .[,premium:=rowSums(.SD,dims = 1),.SDcols=names(.)%>%.[which(str_detect(.,"unmatched"))]]%>%
  .[,ratio:=((unmatched_regular_premium+unmatched_tPlus_premium)/premium)%>%round(.,4)]%>%
  .[,unmatched_current_premium:=(ifelse(ratio>=target_ratio,0,target_ratio-ratio)*premium/10)%>%floor(.)*10]%>%
  .[,current_hold:=unmatched_current_premium_formal-unmatched_current_premium]
user_data<-copy(user_list)

(sum(user_data$unmatched_regular_premium)+sum(user_data$unmatched_current_premium))/sum(asset_data$unmatched_amount)

#################################### 1 主循环 #########################################
regular_seq<-user_list[unmatched_regular_premium>0,c("userid","unmatched_regular_premium")]%T>%
  setorder(.,-unmatched_regular_premium)
tPlus_seq<-user_list[unmatched_tPlus_premium>0,c("userid","unmatched_tPlus_premium")]%T>%
  setorder(.,-unmatched_tPlus_premium)
current_seq<-user_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
  setorder(.,-unmatched_current_premium)

# r_regular=ceiling((nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num))/0.05)*0.05

r_regular=nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num)


r_tPlus=ceiling((nrow(tPlus_seq)/(sum(asset_list[unmatched_amount>0,]$avail_num)-nrow(regular_seq)))/0.05)*0.05

match_record<-data.table()
for(i in 1:nrow(asset_list)){
  # wait_asset直接从asset_data选取，因此顺序不会发生变化
  wait_asset<-asset_data[i,]
  # 队列形成
  regular_seq<-user_list[unmatched_regular_premium>0,c("userid","unmatched_regular_premium")]%T>%
    setorder(.,-unmatched_regular_premium)
  tPlus_seq<-user_list[unmatched_tPlus_premium>0,c("userid","unmatched_tPlus_premium")]%T>%
    setorder(.,-unmatched_tPlus_premium)
  current_seq<-user_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
    setorder(.,-unmatched_current_premium)
  
  #  1.1.1 可行解是否存在下界的判定
  m_regular<-min(floor(r_regular*wait_asset$avail_num),nrow(regular_seq))
  m_tPlus<-min(floor((wait_asset$avail_num-m_regular)*r_tPlus),nrow(tPlus_seq))
  m_current<-min(wait_asset$avail_num-m_regular-m_tPlus,nrow(current_seq))
  
  judge_const<-(
    regular_seq$unmatched_regular_premium%>%
      head(.,m_regular)%>%
      sum(.))+
    (
      tPlus_seq$unmatched_tPlus_premium%>%
        head(.,m_tPlus)%>%
        sum(.))+
    (
      current_seq$unmatched_current_premium%>%
        head(.,m_current)%>%
        sum(.)
    )
  
  # 1.1.2 存在可行解则继续，否则跳过此资产
  if(judge_const<wait_asset$unmatched_amount){
    next
  }
  
  # 1.2 定期匹配=>T+N匹配=>活期匹配
  
  temp<-match_temp(unmatched_amount = wait_asset$unmatched_amount,
                   avail_num = wait_asset$avail_num,
                   regular_seq = regular_seq,
                   tPlus_seq = tPlus_seq,
                   current_seq = current_seq,
                   r_regular = r_regular,
                   r_tPlus = r_tPlus)%>%
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
      .[,unmatched_regular_premium:=unmatched_regular_premium-amount]%>%
      .[,amount:=NULL]
  }
  if(nrow(temp[type=="tPlus"])>0){
    user_list<-temp[type=="tPlus",c("userid","amount")][user_list,on=c("userid"="userid"),
                                                        nomatch=NA,
                                                        mult="all"]%>%
      .[,amount:=ifelse(is.na(amount),0,amount)]%>%
      .[,unmatched_tPlus_premium:=unmatched_tPlus_premium-amount]%>%
      .[,amount:=NULL]
  }
  if(nrow(temp[type=="current"])>0){
    user_list<-temp[type=="current",c("userid","amount")][user_list,on=c("userid"="userid"),
                                                          nomatch=NA,
                                                          mult="all"]%>%
      .[,amount:=ifelse(is.na(amount),0,amount)]%>%
      .[,unmatched_current_premium:=unmatched_current_premium-amount]%>%
      .[,amount:=NULL]
  }
  print(paste(i,"finished !"))
}


##################### 2.结果分析 ############################
asset_match_result<-match_record[,.(amount=sum(amount),num=uniqueN(userid)),by=.(asset_id,type)]%>%
  dcast(.,asset_id~type,value.var=c("amount","num"),fill = 0)%>%
  .[asset_data[,c("id","amount")],on=c("asset_id"="id")]


user_match_result<-{
  a<-match_record[,.(matched_premium=sum(amount)),by=.(userid,type)]%>%
    dcast(.,userid~type,value.var="matched_premium",fill=0)%T>%
    setnames(.,c("regular","current"),c("matched_regular_premium","matched_current_premium"))
  
  xx<-a[user_list,on="userid",nomatch=NA]%>%{
    vars=names(.)%>%.[which(str_detect(.,"premium"))]
    .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
  }%>%.[,c("userid",
           "matched_current_premium",
           "unmatched_current_premium",
           "current_hold",
           "matched_regular_premium",
           "unmatched_regular_premium",
           "unmatched_tPlus_premium",
           "premium")]%>%
    .[,matched_ratio:=round(rowSums(.SD,dim=1)/premium,2),.SDcols=names(.)%>%.[which(str_detect(.,"matched"))]]
  xx
}



