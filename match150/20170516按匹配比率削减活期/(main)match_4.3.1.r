library(data.table)
library(magrittr)
library(dplyr)
library(tidyr)
# 【FUN】建立匹配缩放函数，对相应产品的存量金额进行区间放缩。
source('~/rstudio/match150/(fun)match_adjust.r', encoding = 'UTF-8')
# 【FUN】建立存量分解模拟函数，按照指定期望比例正态随机出不同产品的存量
source('~/rstudio/match150/(fun)premium_split_simulation.r', encoding = 'UTF-8')
#【FUN】根据待匹配金额计算出匹配明细表
source('~/rstudio/match150/(fun)match_temp.r', encoding = 'UTF-8')

#解决多产品匹配的资产偏倚问题，减小小额用户的资产占用
######################## 0. 初始条件及数据集处理模拟 ##########################
#基本常量
n=200
plain_rate=2.5
regular_adjust=F
tPlus_adjust=F
# tPlus_breaks=c(0,1e+04,10e+04,50e+04,100e+04,5000e+04)
# tPlus_adjust_ratio=c(1,1,0.95,0.9,0.8)
current_adjust=F
# current_breaks=c(0,1e+09)
# # current_adjust_ratio_hat<-(sum(asset_data$amount)-sum(user_data$regular_premium)-sum(user_data$tPlus_premium))/
# #   (sum(user_data$premium)-sum(user_data$regular_premium)-sum(user_data$tPlus_premium))
# current_adjust_ratio=0.67

################## 0.1 用户信息表 #######################
user_data<-fread("F:/Project/20170315资产匹配穿透150人调研/user_data.csv")%>%
  .[premium>=100,]%>%
  premium_split_simulation(user_data = .,ratio_mean=c(0.14,0,NA),ratio_sd=c(0.05,0,NA))
################## 0.2 资产信息表 #######################
asset_detail<-fread("F:/Project/20170315资产匹配穿透150人调研/20170314ast_distribution.csv")
asset_data<-fread("F:/Project/20170315资产匹配穿透150人调研/asset_data.csv")%>%
  .[asset_detail,on=c("id"="id"),rate:=i.aunualinterestrate]%>%
  .[,amount:=round(amount/1000)*1000]%>%
  .[,":="(unmatched_amount=amount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=unmatched_amount/avail_num]%T>%
  setorder(.,-unmatched_amount)

target_ratio=ceiling(sum(asset_data$unmatched_amount)/sum(user_data$premium)*100)/100

################# 0.3 循环初始化 ########################
asset_list<-copy(asset_data)%T>%
  setorder(.,-unmatched_amount)

user_list<-copy(user_data)%T>%
  setnames(.,"unmatched_current_premium","unmatched_current_premium_formal")%>%
  .[,ratio:=((unmatched_regular_premium+unmatched_tPlus_premium)/premium)%>%round(.,4)]%>%
  .[,unmatched_current_premium:=(ifelse(ratio>=target_ratio,0,target_ratio-ratio)*premium/10)%>%floor(.)*10]%>%
  .[,current_minus_amount:=(unmatched_current_premium_formal-unmatched_current_premium)%>%round(.,2)]

match_record<-data.table()
################# 0.4 循环前序列数据校正 ######################
if(regular_adjust){
  user_list<-match_adjust(user_data = user_list,
                          item = "regular",
                          breaks = regular_breaks,
                          adjust_ratio = regular_adjust_ratio)
}
if(tPlus_adjust){
  user_list<-match_adjust(user_data = user_list,
                          item = "tPlus",
                          breaks = tPlus_breaks,
                          adjust_ratio = tPlus_adjust_ratio)
}
if(current_adjust){
  user_list<-match_adjust(user_data = user_list,
                          item = "current",
                          breaks = current_breaks,
                          adjust_ratio = current_adjust_ratio)
}
user_list_snap<-copy(user_list)
#################################### 1 主循环 #########################################
regular_seq<-user_list[unmatched_regular_premium>0,c("userid","unmatched_regular_premium")]%T>%
  setorder(.,-unmatched_regular_premium)
tPlus_seq<-user_list[unmatched_tPlus_premium>0,c("userid","unmatched_tPlus_premium")]%T>%
  setorder(.,-unmatched_tPlus_premium)
current_seq<-user_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
  setorder(.,-unmatched_current_premium)

current_minus<-user_list[current_minus_amount>0,c("userid","current_minus_amount")]%T>%
  setnames(.,"current_minus_amount","unmatched_premium")
# r_regular=ceiling((nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num))/0.05)*0.05

r_regular=nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num)

r_tPlus=ceiling((nrow(tPlus_seq)/(sum(asset_list[unmatched_amount>0,]$avail_num)-nrow(regular_seq)))/0.05)*0.05

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

user_list<-user_list[,unmatched_current_premium:=unmatched_current_premium+current_minus_amount]%>%
  .[,-c("unmatched_current_premium_formal","current_minus_amount","ratio")]

##################### 2.结果分析 ############################
# user_match_result<-tbl_df(match_record)%>%
#   left_join(.,select_(asset_data,.dots=c("id","rate")),by=c("asset_id"="id"))%>%
#   group_by(.,userid,type)%>%
#   summarise(.,matched_amount=sum(amount),annual_interest=sum(amount*rate/100))%>%{
#     interest<-group_by(.,userid)%>%summarise(.,total_annual_interest=sum(annual_interest))
#     select_(.,.dots=c("userid","type","matched_amount"))%>%
#       spread(.,type,matched_amount)%>%
#       left_join(.,interest,by=c("userid"="userid"))
#   }%>%
#   mutate(.,regular=ifelse(is.na(regular),0,regular),
#          tPlus=ifelse(is.na(tPlus),0,tPlus),
#          current=ifelse(is.na(current),0,current))%>%
#   as.data.table(.)
# 
# asset_match_result<-match_record[,.(amount=sum(amount),num=uniqueN(userid)),by=.(asset_id,type)]%>%
#   dcast(.,asset_id~type,value.var=c("amount","num"),fill = 0)%>%
#   .[asset_data[,c("id","amount")],on=c("asset_id"="id")]
# 
# 
# 
# user_result<-copy(user_list)%>%
#   .[,c("userid","premium","regular_premium","tPlus_premium")]%>%
#   .[,current_premium:=round(premium-regular_premium-tPlus_premium,2)]%>%
#   .[match_result,on=c("userid"="userid"),":="(matched_regular_premium=i.regular,
#                                               matched_tPlus_premium=i.tPlus,
#                                               matched_current_premium=i.current,
#                                               asset_interest=i.total_annual_interest)]%T>%{
#                                                 tmp<-.[,2:length(names(.))]
#                                                 .[,2:length(names(.))][is.na(tmp)]<-0
#                                               }%>%
#   .[,unmatched_premium:=(premium-matched_regular_premium-matched_tPlus_premium-matched_current_premium)%>%round(.,2)]%>%
#   .[,general_rate:=((asset_interest+unmatched_premium*plain_rate/100)/premium*100)%>%round(.,2)]
# 
# 
# hist(user_result$general_rate,labels = T,
#      main=paste(current_adjust_ratio,"- Histogram of Users' General Rate"),
#      xlab = "Rate*100")
# 
# hist(user_result[general_rate==plain_rate,]$unmatched_premium,labels=T,
#      main=paste(current_adjust_ratio,"- Histogram of Non-matched-users' Premium"),
#      xlab="premium")

path<-"F:/Project/20170315资产匹配穿透150人调研/20170517资产分级用户按理论利率重匹/"
invest_rawdata_for_simu<-fread("F:/Project/20170315资产匹配穿透150人调研/时间流模拟/bak/invest_rawdata_for_simu.csv")
redeem_rawdata_for_simu<-fread("F:/Project/20170315资产匹配穿透150人调研/时间流模拟/bak/redeem_rawdata_for_simu.csv")
user_list<-fread(paste0(path,"user_list.csv"))
match_record<-fread(paste0(path,"match_record.csv"))
user_premium<-match_record[,.(premium=sum(amount)),by=.(userid,type)]%>%
  dcast(.,userid~type,value.var="premium",fill=0)%T>%
  setnames(.,c("regular","current"),c("regular_premium","current_premium"))%>%
  .[,tPlus_premium:=0]
premium_initial<-user_list[user_premium,on="userid",":="(regular_premium=i.regular_premium,
                                                         tPlus_premium=i.tPlus_premium)]%>%{
  vars<-c("regular_premium","tPlus_premium")
  .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
}
  

source('~/rstudio/match150/(fun)invest_simu.r', encoding = 'UTF-8')
source('~/rstudio/match150/(fun)redeem_simu.r', encoding = 'UTF-8')
invest_simu_data<-invest_simu(invest_rawdata_for_simu,P_regular = 0.15,P_tPlus = 0)
redeem_simu_data<-redeem_simu(redeem_rawdata_for_simu,invest_simu_data,premium_initial = premium_initial)



