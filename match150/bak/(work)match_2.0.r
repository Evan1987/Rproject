library(data.table)
library(magrittr)

n=150
low_amount=0
match_ratio=1
low_premium=100

asset_data<-fread("F:/Project/20170315资产匹配穿透150人调研/asset_data.csv")%>%
  .[,":="(unmatched_amount=amount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=unmatched_amount/avail_num]%T>%
  setkey(.,id)

user_data<-fread("F:/Project/20170315资产匹配穿透150人调研/user_data.csv")%>%
  .[premium>=100,]%>%
  .[,target:=premium*match_ratio]%>%
  .[,unmatched_premium:=target]%T>%
  setkey(.,userid)

asset_list<-copy(asset_data)
user_list<-copy(user_data)

match_record<-data.table()
i=1
while(nrow(asset_list[unmatched_amount>low_amount&avail_num>0,])>0){
  
  wait_asset_data<-asset_list[unmatched_amount>low_amount&avail_num>0,]%T>%
    setorder(.,-unmatched_amount)%>%
    head(.,1)
  avail_user_list<-copy(user_list)%>%
    .[,diff_premium_amount:=abs(unmatched_premium-wait_asset_data$avg_avail_amount)]%T>%
    setorder(.,diff_premium_amount)%>%
    head(.,1)

  temp<-data.table(id=i,
                   asset_id=wait_asset_data$id,
                   userid=avail_user_list$userid,
                   amount=min(avail_user_list$unmatched_premium,
                              wait_asset_data$unmatched_amount)
                  )
  
  match_record<-rbind(match_record,temp)%T>%
    setkey(.,asset_id)
  
  wait_asset_data[,":="(unmatched_amount=unmatched_amount-temp$amount,
                        avail_num=n-length(unique(match_record[asset_id==temp$asset_id,]$userid))
                        )]%>%
    .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]%T>%
    setkey(.,id)
  
  avail_user_list[,unmatched_premium:=unmatched_premium-temp$amount]
  
  
  asset_list<-asset_list[id==wait_asset_data$id,
                         ":="(unmatched_amount=wait_asset_data$unmatched_amount,
                              avail_num=wait_asset_data$avail_num,
                              avg_avail_amount=wait_asset_data$avg_avail_amount)]
  
  user_list<-user_list[userid==avail_user_list$userid,
                       unmatched_premium:=avail_user_list$unmatched_premium]
  print(paste(i,"finished!"))
  i=i+1
  if(min(asset_list$avg_avail_amount)==0){
    break
  }
}












  