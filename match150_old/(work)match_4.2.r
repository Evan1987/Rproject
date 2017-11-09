library(data.table)
library(magrittr)
# 【FUN】建立匹配缩放函数，对相应产品的存量金额进行区间放缩。
source('~/rstudio/match150/(fun)match_adjust.r', encoding = 'UTF-8')
# 【FUN】建立存量分解模拟函数，按照指定期望比例正态随机出不同产品的存量
source('~/rstudio/match150/(fun)premium_split_simulation.r', encoding = 'UTF-8')
#【FUN】根据待匹配金额计算出匹配明细表
source('~/rstudio/match150/(fun)match_temp.r', encoding = 'UTF-8')

#解决多产品匹配的资产偏倚问题，减小小额用户的资产占用
######################## 0. 初始条件及数据集处理模拟 ##########################
#基本常量
n=150
regular_adjust=F
tPlus_adjust=F
# tPlus_breaks=c(0,1e+04,10e+04,50e+04,100e+04,5000e+04)
# tPlus_adjust_ratio=c(1,1,0.95,0.9,0.8)
current_adjust=F
################## 0.1 用户信息表 #######################
user_data<-fread("F:/Project/20170315资产匹配穿透150人调研/user_data.csv")%>%
  .[premium>=100,]%>%
  premium_split_simulation(user_data = .)
################## 0.2 资产信息表 #######################
asset_data<-fread("F:/Project/20170315资产匹配穿透150人调研/asset_data.csv")%>%
  .[,amount:=round(amount/1000)*1000]%>%
  .[,":="(unmatched_amount=amount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=unmatched_amount/avail_num]%T>%
  setorder(.,-unmatched_amount)
################# 0.3 循环初始化 ########################
asset_list<-copy(asset_data)%T>%
  setorder(.,-unmatched_amount)

user_list<-copy(user_data)%>%
  .[,":="(regular_premium_ratio=NULL,
          tPlus_premium_ratio=NULL)]

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

#################################### 1 主循环 #########################################
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
  
  r_regular=ceiling((nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num))/0.05)*0.05
  r_tPlus=1
  
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
  wait_asset[,":="(unmatched_amount=unmatched_amount-sum(temp$amount),
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
