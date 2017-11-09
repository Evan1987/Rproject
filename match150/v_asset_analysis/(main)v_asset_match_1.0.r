library(data.table)
library(magrittr)
source('~/rstudio/match150/v_asset_analysis/(fun)match_temp3.r', encoding = 'UTF-8')

path<-"F:/Project/20170315资产匹配穿透150人调研/20170417小V资产补充匹配/"
rt=5.5
plain_rate=2.5
# 待匹用户的权重生成函数
fun.weight<-function(r_target,r_actual,user_property){
  return(10*(r_target-r_actual)+100*user_property)
}
vec.fun.weight<-Vectorize(fun.weight)

#待匹用户基于未匹存量和权重的待匹金额生成函数
fun.premium<-function(weight,premium,max_premium=5000,min_premium=100){
  x<-premium*min(weight,100)/100
  result<-ifelse(x>max_premium,max_premium,ifelse(x<min_premium,min(min_premium,premium),x))
  return(floor(result/50)*50)
}
vec.fun.premium<-Vectorize(fun.premium)

#小V资产信息生成函数
fun.v_asset_simu<-function(asset_num,max_amount=200000,min_amount=1000,asset_avail_num=150){
  v_asset<-{
    x<-runif(asset_num,min=1000,max=200000)
    data.table(id=1:asset_num,amount=round(x),avail_num=asset_avail_num)
  }
  return(v_asset)
}

#读取用户原始数据
user_result<-fread(paste0(path,"user_result.csv"))%>%
  .[,general_rate:=((asset_interest+unmatched_premium*plain_rate/100)/premium*100)%>%round(.,2)]

#生成小V资产
v_asset_data<-fun.v_asset_simu(1000)%>%.[,unmatched_amount:=amount]%>%setorder(.,-unmatched_amount)

v_cash<-user_result[general_rate<rt,]%>%
  .[,wait_v_premium:=current_premium-matched_current_premium]%>%
  .[,c("userid","premium","asset_interest","general_rate","wait_v_premium")]%>%
  .[,user_property:=runif(nrow(.),min = 0,max = 0.9)%>%round(.,2)]%>%
  .[,weight:=vec.fun.weight(rt,general_rate,user_property)]%>%
  .[,unmatched_premium:=vec.fun.premium(weight,wait_v_premium)]%>%
  {
    weight = .[,"weight"]$weight
    unit = floor((max(weight)-min(weight))/3)
    breaks=c(0,1:2*unit,max(weight)+unit)
    .[,weight_label:=cut(weight,breaks = breaks,labels = 0:(length(breaks)-2),right = F)%>%
        as.character(.)%>%
        as.numeric(.)]
  }%T>%
  setorder(.,-weight_label,-unmatched_premium)

v_cash_list<-split(v_cash[,c("userid","unmatched_premium","weight_label")],by="weight_label")
v_asset_list<-copy(v_asset_data)
match_record<-data.table()
##### 利用与大资产匹配相似的方法进行匹配
A_seq<-v_cash_list$`2`%T>%setorder(.,-unmatched_premium)%>%.[,-"weight_label"]
B_seq<-v_cash_list$`1`%T>%setorder(.,-unmatched_premium)%>%.[,-"weight_label"]
C_seq<-v_cash_list$`0`%T>%setorder(.,-unmatched_premium)%>%.[,-"weight_label"]

r_A=ceiling((nrow(A_seq)/sum(v_asset_list[unmatched_amount>0,]$avail_num))/0.05)*0.05
r_B=ceiling((nrow(B_seq)/(sum(v_asset_list[unmatched_amount>0,]$avail_num)-nrow(A_seq)))/0.05)*0.05

for(i in 1:nrow(v_asset_list)){
  wait_asset<-v_asset_data[i,]
  # 队列形成
  A_seq<-A_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  B_seq<-B_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  C_seq<-C_seq[unmatched_premium>0,]%>%setorder(.,-unmatched_premium)
  #  1.1.1 可行解是否存在下界的判定
  m_A<-min(floor(r_A*wait_asset$avail_num),nrow(A_seq))
  m_B<-min(floor((wait_asset$avail_num-m_A)*r_B),nrow(B_seq))
  m_C<-min(wait_asset$avail_num-m_A-m_B,nrow(C_seq))
  
  judge_const<-(
    A_seq$unmatched_premium%>%
      head(.,m_A)%>%
      sum(.))+
    (
      B_seq$unmatched_premium%>%
        head(.,m_B)%>%
        sum(.))+
    (
      C_seq$unmatched_premium%>%
        head(.,m_C)%>%
        sum(.)
    )
  
  # 1.1.2 存在可行解则继续，否则跳过此资产
  if(judge_const<wait_asset$unmatched_amount){
    next
  }
  
  # 1.2 定期匹配=>T+N匹配=>活期匹配
  
  temp<-match_temp3(unmatched_amount = wait_asset$unmatched_amount,
                   avail_num = wait_asset$avail_num,
                   A_seq = A_seq,
                   B_seq = B_seq,
                   C_seq = C_seq,
                   r_A = r_A,
                   r_B = r_B)%>%
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
          unmatched_premium:=unmatched_premium-ifelse(is.na(amount),0,amount)]
  }
  if(nrow(temp[type=="B",])>0){
    B_seq[temp[type=="B",c("userid","amount")],
          on=c("userid"="userid"),
          unmatched_premium:=unmatched_premium-ifelse(is.na(amount),0,amount)]
  }
  if(nrow(temp[type=="C",])>0){
    C_seq[temp[type=="C",c("userid","amount")],
          on=c("userid"="userid"),
          unmatched_premium:=unmatched_premium-ifelse(is.na(amount),0,amount)]
  }
  print(paste(i,"finished !"))
}

match_result<-match_record[,.(amount=sum(amount)),by=userid]
v_cash_new<-v_cash[match_result,on=c("userid"="userid"),v_matched_premium:=ifelse(is.na(i.amount),0,i.amount)]%>%
  .[,unmatched_premium_new:=unmatched_premium-v_matched_premium]

