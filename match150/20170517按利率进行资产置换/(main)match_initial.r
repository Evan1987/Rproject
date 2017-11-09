library(data.table)
library(magrittr)
library(stringr)
source('~/rstudio/match150/(fun)match_temp.r', encoding = 'UTF-8')
path="F:/Project/20170315资产匹配穿透150人调研/20170517按利率进行资产置换/"

# 不同产品的利率设定
regular_rate_exp<-fread(paste0(path,"regular_rate_exp.csv"))
# tPlus产品的模拟条件
tPlus_simu_ratio=0.7
sd=0.05
n=200
# 资产信息
asset_data<-fread(paste0(path,"20170516ast_info.csv"))%>%
  .[,":="(unmatched_amount=corpusamount,
          avail_num=n)]%T>%
  setorder(.,-unmatched_amount)
# 用户存量信息及预期享受利率信息
user_list<-fread(paste0(path,"20170516user_premium.csv"))%>%
  .[,premium_tPlus:=(premium_cur*(rnorm(nrow(.),mean = tPlus_simu_ratio,sd=sd)%>%pmin(.,1))/1000)%>%floor(.)*1000]%>%
  .[,premium_cur:=premium_cur-premium_tPlus]%>%
  .[,premium_regular:=0.0]%>%
{   regular_info<-fread(paste0(path,"regular_info.csv"))%>%
    .[regular_rate_exp,on="num"]%>%
    .[,regular_daily_interest:=rate/100/365*amount]
    user_regular<-regular_info[,.(amount=sum(amount),regular_daily_interest=sum(regular_daily_interest)),by=userid]%>%
      .[amount>=1000,]
    .[user_regular,on="userid",":="(premium_regular=i.amount,regular_daily_interest=i.regular_daily_interest)]
}%>%
  {
    vars<-names(.)%>%.[which(str_detect(.,"regular"))]
    .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
}%>%
  .[,premium:=premium_regular+premium_cur+premium_tPlus]
# all.equal(user_list$premium,user_list$premium_regular+user_list$premium_cur+user_list$premium_tPlus)

# 计算活期调整比例的准线
target_ratio=(sum(asset_data$corpusamount)/sum(user_list$premium)*100)%>%ceiling(.)/100
# target_ratio=1
vars=names(user_list)%>%.[which(str_detect(.,"premium"))]
cash_list<-user_list[,.SD,.SDcols=c("userid",vars)]%T>%
  setnames(.,
           c("premium_regular","premium_cur","premium_tPlus"),
           c("unmatched_regular_premium","unmatched_current_premium_formal","unmatched_tPlus_premium"))%>%
  .[,exp_match_ratio:=(unmatched_regular_premium+unmatched_tPlus_premium)/premium]%>%
  .[,unmatched_current_premium:=(ifelse(exp_match_ratio>=target_ratio,
                                        0,
                                        target_ratio-exp_match_ratio)*premium/10)%>%floor(.)*10]%>%
  .[,current_hold:=(unmatched_current_premium_formal-unmatched_current_premium)%>%round(.,2)]

cash_list_snap<-copy(cash_list)
sum(asset_data$unmatched_amount)/(sum(cash_list_snap$unmatched_regular_premium)+
                                    sum(cash_list_snap$unmatched_tPlus_premium)+
                                    sum(cash_list_snap$unmatched_current_premium))
#################################### 1 主循环 #######################################
asset_list<-copy(asset_data)

regular_seq<-cash_list[unmatched_regular_premium>0,c("userid","unmatched_regular_premium")]%T>%
  setorder(.,-unmatched_regular_premium)
tPlus_seq<-cash_list[unmatched_tPlus_premium>0,c("userid","unmatched_tPlus_premium")]%T>%
  setorder(.,-unmatched_tPlus_premium)
current_seq<-cash_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
  setorder(.,-unmatched_current_premium)

r_regular=ceiling((nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num))*100)/100
# r_regular=nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num)
r_tPlus=ceiling((nrow(tPlus_seq)/(sum(asset_list[unmatched_amount>0,]$avail_num)-nrow(regular_seq)))*100)/100

match_record<-data.table()
for(i in 1:nrow(asset_list)){
  # wait_asset直接从asset_data选取，因此顺序不会发生变化
  wait_asset<-asset_data[i,]
  # 队列形成
  regular_seq<-cash_list[unmatched_regular_premium>0,c("userid","unmatched_regular_premium")]%T>%
    setorder(.,-unmatched_regular_premium)
  tPlus_seq<-cash_list[unmatched_tPlus_premium>0,c("userid","unmatched_tPlus_premium")]%T>%
    setorder(.,-unmatched_tPlus_premium)
  current_seq<-cash_list[unmatched_current_premium>0,c("userid","unmatched_current_premium")]%T>%
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
  
  
  ## 更新 cash_list和asset_list
  wait_asset[,":="(unmatched_amount=round(unmatched_amount-sum(temp$amount),2),
                   avail_num=n-uniqueN(temp$userid))]
  
  asset_list[id==wait_asset$id,
             ":="(unmatched_amount=wait_asset$unmatched_amount,
                  avail_num=wait_asset$avail_num)]
  
  
  if(nrow(temp[type=="regular"])>0){
    cash_list<-cash_list[temp[type=="regular",c("userid","amount")],
                         on="userid",
                         unmatched_regular_premium:=round(unmatched_regular_premium-i.amount,2)]
  }
  if(nrow(temp[type=="tPlus"])>0){
    cash_list<-cash_list[temp[type=="tPlus",c("userid","amount")],
                         on="userid",
                         unmatched_tPlus_premium:=round(unmatched_tPlus_premium-i.amount,2)]
  }
  if(nrow(temp[type=="current"])>0){
    cash_list<-cash_list[temp[type=="current",c("userid","amount")],
                         on="userid",
                         unmatched_current_premium:=round(unmatched_current_premium-i.amount,2)]
  }
  print(paste(i,"finished !"))
}
##################### 2.结果输出 ############################
path2<-"F:/Project/20170315资产匹配穿透150人调研/20170517按利率进行资产置换/match_result/"
write.csv(user_list,paste0(path2,"user_list.csv"),row.names = F)
write.csv(asset_list,paste0(path2,"asset_list.csv"),row.names = F)
write.csv(cash_list,paste0(path2,"cash_list.csv"),row.names = F)
write.csv(asset_data,paste0(path2,"asset_data.csv"),row.names = F)
write.csv(match_record,paste0(path2,"match_record.csv"),row.names = F)


