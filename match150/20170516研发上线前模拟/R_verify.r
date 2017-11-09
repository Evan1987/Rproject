source('~/rstudio/!custom/(fun)mysqlconn.R')
conn<-mysqlconn("mysql_settings.csv")

rawdata<-sqlQuery(conn,"select user_id,account_type,amount from ast_money_account")%>%
  as.data.table(.)%T>%
  {names(.)<-c("userid","type","amount")}

regular_seq<-rawdata[type==3,-"type"]%T>%
  setorder(.,-amount)%T>%
  {names(.)<-c("userid","unmatched_regular_premium")}
tPlus_seq<-rawdata[type==2,-"type"]%T>%
  setorder(.,-amount)%T>%
  {names(.)<-c("userid","unmatched_tPlus_premium")}
current_seq<-rawdata[type==1,-"type"]%T>%
  setorder(.,-amount)%T>%
  {names(.)<-c("userid","unmatched_tPlus_premium")}

user_list<-dcast(rawdata,userid~type,value.var = "amount",fill=0)%T>%
  {names(.)<-c("userid","unmatched_current_premium","unmatched_regular_premium")}%>%
  .[,unmatched_tPlus_premium:=0]




asset_list<-sqlQuery(conn,"select asset_id as id,amount,150 as avail_num from ast_matching_asset_group")%>%
  as.data.table(.)%>%
  .[,unmatched_amount:=amount]%>%
  .[,avg_avail_amount:=round(amount/avail_num,2)]%T>%
  setorder(.,-unmatched_amount)

asset_data<-copy(asset_list)
