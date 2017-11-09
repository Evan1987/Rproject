library(data.table)
library(magrittr)
library(stringr)
source('~/rstudio/match150/(fun)match_temp5.r', encoding = 'UTF-8')
source('~/RStudio/match150/20170527研发第二次模拟/(fun)fun.wise_break.r', echo=TRUE)
source('~/rstudio/match150/(fun)fun.judge_const.r')
path="F:/Project/20170315资产匹配穿透150人调研/20170527研发小额模拟/"

n=200

user_info<-fread(paste0(path,"ast_money_account.csv"))%>%
  .[type==1,c("userid","user_level")]

asset_data_total<-fread(paste0(path,"ast_matching_asset_group.csv"))
asset_data<-asset_data_total[asset_type==1,]%>%
  .[,-"asset_type"]%T>%
  setnames(.,c("amount","num"),c("unmatched_amount","avail_num"))%>%
  .[,avail_num:=n]%>%
  .[,avg_avail_amount:=round(unmatched_amount/avail_num,2)]%T>%
  setorder(.,-unmatched_amount)

asset_list<-copy(asset_data)

user_data<-fread(paste0(path,"user_list_adjusted.csv"))

target_ratio=ceiling(sum(asset_data$unmatched_amount)/sum(user_data$premium)*100)/100

user_list_pre<-copy(user_data)%T>%
  setnames(.,
           c("premium_regular","premium_current"),
           c("unmatched_regular_premium","unmatched_current_premium_formal"))%>%
  .[,unmatched_tPlus_premium:=0.0]%>%
  .[,unmatched_current_premium:=premium*target_ratio-(premium-unmatched_current_premium_formal)]%>%
  .[,unmatched_current_premium:=pmax(unmatched_current_premium,0)]

adjust_ratio=c(0,.94,.96,.98,1)
breaks = fun.wise_break(user_list = user_list_pre,
                        asset_data_total = asset_data_total[asset_type==1,],
                        adjust_ratio = adjust_ratio,
                        safe_amount = 10000000,
                        slim_contribution = c(2,3,5))

user_list<-copy(user_list_pre)%>%
  .[,adjust_ratio:=cut(premium,
                       breaks=breaks,
                       labels=adjust_ratio,
                       right = F)%>%as.character(.)%>%as.numeric(.)]%>%
  .[,unmatched_current_premium:=
      (premium*target_ratio-(premium-unmatched_current_premium_formal))*adjust_ratio]%>%
  .[,unmatched_current_premium:=(pmax(unmatched_current_premium,0)/1)%>%floor(.)*1]%>%
  .[,current_hold:=round(unmatched_current_premium_formal-unmatched_current_premium,2)]

user_list_snap<-copy(user_list)


all(user_list$unmatched_current_premium_formal>=user_list$unmatched_current_premium)

(sum(user_list_snap$unmatched_regular_premium)+sum(user_list_snap$unmatched_current_premium))-sum(asset_data$unmatched_amount)

(sum(user_list_snap$unmatched_regular_premium)+sum(user_list_snap$unmatched_current_premium))-sum(asset_data_total$amount)


#################################### 1 主循环 #########################################
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


##################### 2.结果分析 ############################
# asset_match_result<-match_record[,.(amount=sum(amount),num=uniqueN(userid)),by=.(asset_id,type)]%>%
#   dcast(.,asset_id~type,value.var=c("amount","num"),fill = 0)%>%
#   .[asset_data[,c("id","amount")],on=c("asset_id"="id")]
# 
# 
# user_match_result<-{
#   a<-match_record[,.(matched_premium=sum(amount)),by=.(userid,type)]%>%
#     dcast(.,userid~type,value.var="matched_premium",fill=0)%T>%
#     setnames(.,c("regular","current"),c("matched_regular_premium","matched_current_premium"))
#   
#   xx<-a[user_list,on="userid",nomatch=NA]%>%{
#     vars=names(.)%>%.[which(str_detect(.,"premium"))]
#     .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
#   }%>%.[,c("userid",
#            "matched_current_premium",
#            "unmatched_current_premium",
#            "current_hold",
#            "matched_regular_premium",
#            "unmatched_regular_premium",
#            "unmatched_tPlus_premium",
#            "premium")]%>%
#     .[,matched_ratio:=round(rowSums(.SD,dim=1)/premium,2),.SDcols=names(.)%>%.[which(str_detect(.,"matched"))]]
#   xx
# }
match_record_snap<-copy(match_record)
# cur_match<-match_record[type=="current",]%T>%setorder(.,asset_id,-amount)
# min_cur_amount<-cur_match[,.(min_amount=min(amount),
#                              quantile_5_amount=quantile(amount,0.05),
#                              quantile_10_amount=quantile(amount,0.1),
#                              median_amount=median(amount)),by=asset_id]%>%
#   .[asset_data,on=c("asset_id"="id"),total_amount:=i.unmatched_amount]

user_list_result<-copy(user_list)


################################# 2.小额匹配主循环 #########################################

source('~/rstudio/match150/20170527研发第二次模拟/(fun)fun.v_asset_simu.r', echo=TRUE)
v_asset_data<-fun.v_asset_simu(amount = 47000000,num=250,sd = 50000,min_amount = 100000)%>%
  data.table(unmatched_amount=.)%>%
  .[,":="(id=.I+max(asset_data$id),avail_num=n)]%>%
  .[,avg_avail_amount:=round(unmatched_amount/avail_num,2)]%T>%
  setorder(.,-unmatched_amount)


v_asset_data<-data.table(unmatched_amount=rep(2e+5,235))%>%
  .[,":="(id=.I+max(asset_data$id),avail_num=n)]%>%
  .[,avg_avail_amount:=round(unmatched_amount/avail_num,2)]%T>%
  setorder(.,-unmatched_amount)

v_asset_data<-data.table(unmatched_amount=c(rep(1e+5,26),rep(2e+5,93),rep(3e+5,67),rep(4e+5,13),rep(5e+5,1)))%>%
  .[,":="(id=.I+max(asset_data$id),avail_num=n)]%>%
  .[,avg_avail_amount:=round(unmatched_amount/avail_num,2)]%T>%
  setorder(.,-unmatched_amount)

# v_asset_data<-asset_data_total[asset_type==2,]%>%
#   .[,-"asset_type"]%T>%
#   setnames(.,c("amount","num"),c("unmatched_amount","avail_num"))%>%
#   .[,avail_num:=n]%>%
#   .[,avg_avail_amount:=round(unmatched_amount/avail_num,2)]%T>%
#   setorder(.,-unmatched_amount)

v_asset_list<-copy(v_asset_data)
v_cash_list<-user_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
  setnames(.,"unmatched_current_premium","unmatched_premium")%>%
  {
    a<-match_record_snap[,.(amount=sum(amount)),by=userid]
    .[a,on="userid",matched_premium:=i.amount]
  }%>%
  .[,matched_premium:=replace(matched_premium,is.na(matched_premium),0)]%>%
  .[user_data,on="userid",premium:=i.premium]%>%
  .[,matched_ratio:=round(matched_premium/premium,2)]

if(nrow(v_cash_list)>sum(v_asset_data$avail_num)){
  slim_v_cash_list<-copy(v_cash_list)%>%
    .[,weight:=unmatched_premium*(1-matched_ratio)^2]%T>%
    setorder(.,-weight)%>%
    head(.,sum(v_asset_data$avail_num))%>%
    .[,-"weight"]
}else{
  slim_v_cash_list<-copy(v_cash_list)
}

A_seq<-slim_v_cash_list[matched_ratio<=0.5&unmatched_premium>=3000,]%T>%
  setorder(.,-unmatched_premium)%>%
  .[,c("userid","unmatched_premium")]
B_seq<-slim_v_cash_list[matched_ratio<=0.7&(!userid%in%A_seq$userid),]%T>%
  setorder(.,-unmatched_premium)%>%
  .[,c("userid","unmatched_premium")]
C_seq<-slim_v_cash_list[!userid%in%c(A_seq$userid,B_seq$userid),]%T>%
  setorder(.,-unmatched_premium)%>%
  .[,c("userid","unmatched_premium")]

r_A=ceiling((nrow(A_seq)/sum(v_asset_list[unmatched_amount>0,]$avail_num))/0.05)*0.05
r_B=ceiling((nrow(B_seq)/(sum(v_asset_list[unmatched_amount>0,]$avail_num)-nrow(A_seq)))/0.05)*0.05
match_record<-data.table()
mean_judge<-rep(0,nrow(v_asset_list))
for(i in 1:nrow(v_asset_list)){
  wait_asset<-v_asset_data[i,]
  # 队列形成
  A_seq<-A_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  B_seq<-B_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  C_seq<-C_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  #  1.1.1 可行解是否存在下界的判定
  if(judge_const<-fun.judge_const(a=1,
                                 b=1,
                                 wait_asset,
                                 first_seq = A_seq,
                                 second_seq = B_seq,
                                 third_seq = C_seq)>=wait_asset$unmatched_amount){
    a = 1
    b = 1
  }else if(judge_const<-fun.judge_const(a=r_A,
                                       b=1,
                                       wait_asset,
                                       first_seq = A_seq,
                                       second_seq = B_seq,
                                       third_seq = C_seq)>=wait_asset$unmatched_amount){
    a = r_A
    b = 1
  }else if(judge_const<-fun.judge_const(a=r_A,
                                       b=r_B,
                                       wait_asset,
                                       first_seq = A_seq,
                                       second_seq = B_seq,
                                       third_seq = C_seq)>=wait_asset$unmatched_amount){
    a = r_A
    b = r_B
  }else{
    next
  }
  mean_judge[i]<-round(judge_const/wait_asset$avail_num,2)
 
  # 1.2 定期匹配=>T+N匹配=>活期匹配
  
  temp<-match_temp5(unmatched_amount = wait_asset$unmatched_amount,
                    avail_num = wait_asset$avail_num,
                    first_seq = A_seq,
                    second_seq = B_seq,
                    third_seq = C_seq,
                    a = a,
                    b = b,
                    item = c("A","B","C"))%>%
    .[,asset_id:=wait_asset$id]%>%
    .[,id:=id+nrow(match_record)]
  
  match_record<-rbind(match_record,temp)
  
  
  ## 更新 user_list和asset_list
  wait_asset[,":="(unmatched_amount=round(unmatched_amount-sum(temp$amount),2),
                   avail_num=avail_num-uniqueN(temp$userid))]
  
  v_asset_list[id==wait_asset$id,
               ":="(unmatched_amount=wait_asset$unmatched_amount,
                    avail_num=wait_asset$avail_num)]
  
  
  if(nrow(temp[type=="A",])>0){
    A_seq[temp[type=="A",c("userid","amount")],
          on=c("userid"="userid"),
          unmatched_premium:=round(unmatched_premium-ifelse(is.na(amount),0,amount),2)]
  }
  if(nrow(temp[type=="B",])>0){
    B_seq[temp[type=="B",c("userid","amount")],
          on=c("userid"="userid"),
          unmatched_premium:=round(unmatched_premium-ifelse(is.na(amount),0,amount),2)]
  }
  if(nrow(temp[type=="C",])>0){
    C_seq[temp[type=="C",c("userid","amount")],
          on=c("userid"="userid"),
          unmatched_premium:=round(unmatched_premium-ifelse(is.na(amount),0,amount),2)]
  }
  print(paste(i,"finished !"))
}

A_seq<-A_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
B_seq<-B_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
C_seq<-C_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)

judge_const_df<-data.table(value=mean_judge)%>%.[,id:=.I]

table(v_asset_data$unmatched_amount)
sum(v_asset_list$unmatched_amount)
max(B_seq$unmatched_premium)
hist(B_seq$unmatched_premium,labels = T,breaks = seq(0,1000,by=100))
hist(C_seq$unmatched_premium,labels = T,breaks = seq(0,5000,by=500))
