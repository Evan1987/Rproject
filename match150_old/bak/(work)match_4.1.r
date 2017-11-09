library(data.table)
library(magrittr)

#解决多产品匹配的资产偏倚问题，减小小额用户的资产占用
######################## 0. 初始条件及数据集处理模拟 ##########################
#基本常量
n=150
r_regular=0.6
r_tPlus=0.75
##################### 0.1 匹配差异化处理函数 ###################
regular_adjust=F
tPlus_adjust=F
tPlus_breaks=c(0,1e+04,10e+04,50e+04,100e+04,5000e+04)
tPlus_adjust_ratio=c(1,1,0.95,0.9,0.8)
current_adjust=F

# 【FUN】建立匹配缩放函数，对相应产品的存量金额进行区间放缩。
match_adjust<-function(user_data,item="tPlus",breaks,adjust_ratio){
  library(data.table)
  library(magrittr)
  
  if(length(adjust_ratio)!=length(breaks)-1){
    stop("the adjust value length must be smaller than length of breaks by 1 !")
  }
  
  colname<-switch(item,
                  "tPlus" = "unmatched_tPlus_premium",
                  "regular" = "unmatched_regular_premium",
                  "current" = "unmatched_current_premium")
  user_adjust_data<-copy(user_data)%>%
    .[,adjust_ratio:=eval(parse(text=paste0("cut(",colname,",breaks,right=F)")))]
  
  levels(user_adjust_data$adjust_ratio)<-adjust_ratio
  
  user_adjust_data[,adjust_ratio:=as.character(adjust_ratio)%>%as.numeric(.)]%>%
    .[,eval(parse(text=paste0(colname,"_formal:=",colname)))]%>%
    .[,eval(parse(text=paste0(colname,":=adjust_ratio*",colname)))]%T>%
    setkey(.,userid)
  
  return(user_adjust_data)
}

##################### 0.2 不同产品存量模拟函数 ###################
# 【FUN】建立存量分解模拟函数，按照指定期望比例正态随机出不同产品的存量。
premium_split_simulation<-function(user_data,
                                   item=c("regular","tPlus","current"),
                                   ratio_mean=c(0.15,0.6,0.25),
                                   ratio_sd=c(0.05,0.2,NA),
                                   unit_premium=c(1000,1000,NA)){
  library(data.table)
  library(magrittr)
  if(length(item)!=length(ratio_mean)){
    stop("premium split ratio must have same length of items !")
  }
  result<-copy(user_data)%>%
    .[,regular_premium_ratio:=rnorm(nrow(.),
                                    mean=ratio_mean[1],
                                    sd=ratio_sd[1])%>%abs(.)%>%round(.,4)]%>%
    .[,regular_premium_ratio:=ifelse(regular_premium_ratio>1,1,regular_premium_ratio)]%>%
    .[,regular_premium:=(regular_premium_ratio*premium/unit_premium[1])%>%floor(.)*unit_premium[1]]%>%
    .[,regular_premium_ratio:=regular_premium/premium]%>%
    .[,tPlus_premium_ratio:=rnorm(nrow(.),
                                  mean=ratio_mean[2],
                                  sd=ratio_sd[2])%>%abs(.)%>%round(.,4)]%>%
    .[,tPlus_premium_ratio:=ifelse(tPlus_premium_ratio+regular_premium_ratio>1,
                                   1-regular_premium_ratio,
                                   tPlus_premium_ratio)]%>%
    .[,tPlus_premium:=(tPlus_premium_ratio*premium/unit_premium[2])%>%floor(.)*unit_premium[2]]%>%
    .[,":="(tPlus_premium_ratio=tPlus_premium/premium,
            unmatched_regular_premium=regular_premium,
            unmatched_tPlus_premium=tPlus_premium)]%>%
    .[,unmatched_current_premium:=premium-unmatched_regular_premium-unmatched_tPlus_premium]
  
  return(result)
}

##################### 0.3 不同产品存量模拟函数 ###################
#【FUN】根据待匹配金额计算出匹配明细表
match_temp<-function(unmatched_amount,
                     avail_num,
                     regular_seq,
                     tPlus_seq,
                     current_seq,
                     r_regular=0.6,
                     r_tPlus=0.75){
  # 【FUN】建立匹配输出函数，对每一个资产的每次匹配输出匹配结果
  match_result_details<-function(user_seq,unmatched_amount,item="regular"){
    
    colname<-switch(item,
                    "tPlus" = "unmatched_tPlus_premium",
                    "regular" = "unmatched_regular_premium",
                    "current" = "unmatched_current_premium")
    
    user_seq2<-copy(user_seq)%>%
      .[,cum_sum_item:=eval(parse(text=paste0("cumsum(",colname,")")))]%>%
      .[,rest:=cum_sum_item-unmatched_amount]%>%
      .[,amount:=eval(parse(text=paste0(colname)))-ifelse(rest<0,0,rest)]%>%
      .[,id:=seq(1 ,nrow(.),by=1)]%>%
      .[amount>0,]
    
    temp<-data.table(id=user_seq2$id,
                     userid=user_seq2$userid,
                     type=item,
                     amount=user_seq2$amount)
    return(temp)
  }
  
  m_regular<-min(nrow(regular_seq),floor(r_regular*avail_num))
  m_tPlus<-min(nrow(tPlus_seq),floor((avail_num-m_regular)*r_tPlus))
  m_current<-min(nrow(current_seq),avail_num-m_regular-m_tPlus)
  
  temp_regular<-temp_tPlus<-temp_current<-data.table()
  
  if(m_regular>0){
    temp_regular<-match_result_details(user_seq = head(regular_seq,m_regular),
                                       unmatched_amount = unmatched_amount,
                                       item = "regular")
  }
  if(m_tPlus>0){
    temp_tPlus<-match_result_details(user_seq = head(tPlus_seq,m_tPlus),
                                     unmatched_amount = unmatched_amount-sum(temp_regular$amount),
                                     item = "tPlus")
  }
  if(m_current>0){
    temp_current<-match_result_details(user_seq = head(current_seq,m_current),
                                       unmatched_amount = unmatched_amount-
                                         sum(temp_regular$amount)-sum(temp_tPlus$amount),
                                       item = "current")
  }
  temp<-rbind(temp_regular,temp_tPlus,temp_current)%>%
    .[,id:=seq(1,nrow(.))]
  return(temp)
}

################## 0.3 用户信息表 #######################
user_data<-fread("~/RStudio/match150/user_data.csv")%>%
  .[premium>=100,]%>%
  premium_split_simulation(user_data = .)

################## 0.4 资产信息表 #######################
asset_data<-fread("~/RStudio/match150/asset_data.csv")%>%
  .[,amount:=round(amount/1000)*1000]%>%
  .[,":="(unmatched_amount=amount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=unmatched_amount/avail_num]%T>%
  setorder(.,-unmatched_amount)

################# 0.5 循环初始化 ########################
asset_list<-copy(asset_data)%T>%
  setorder(.,-unmatched_amount)

user_list<-copy(user_data)%>%
  .[,":="(regular_premium_ratio=NULL,
          tPlus_premium_ratio=NULL)]

match_record<-data.table()
r_record<-data.table()
################# 0.6 循环前数据校正 ######################
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

source('~/RStudio/match150/(fun)r_opt.r', encoding = 'UTF-8')
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
  
  #  1.1.1 可行解是否存在下界的判定
  r<-r_opt(unmatched_amount = wait_asset$unmatched_amount,
           avail_num = wait_asset$avail_num,
           regular_seq = regular_seq,
           tPlus_seq = tPlus_seq,
           current_seq = current_seq,
           r_regular_init=0.6,
           r_tPlus_init=0.75)
  temp_r<-data.table(asset_id=wait_asset$id,
                     r_regular=r[1],
                     r_tPlus=r[2])
  r_record<-rbind(r_record,temp_r)
  if(sum(r)==0){next}
  
  # 1.2 定期匹配=>T+N匹配=>活期匹配
  
  temp<-match_temp(unmatched_amount = wait_asset$unmatched_amount,
                   avail_num = wait_asset$avail_num,
                   regular_seq = regular_seq,
                   tPlus_seq = tPlus_seq,
                   current_seq = current_seq,
                   r_regular = r[1],
                   r_tPlus = r[2])%>%
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
