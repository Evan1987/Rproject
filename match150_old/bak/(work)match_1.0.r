library(data.table)
library(magrittr)

n=150
low_amount=0
match_ratio=c(0.7,0.8)
premium_bound=c(500000,1000000)
low_premium<-100


asset_data<-fread("F:/Project/20170315资产匹配穿透150人调研/asset_data.csv")%>%
  .[,":="(unmatched_amount=amount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=unmatched_amount/avail_num]%T>%
  setkey(.,id)

user_data<-fread("F:/Project/20170315资产匹配穿透150人调研/user_data.csv")%>%
  .[premium>=100,]%>%
  .[premium>=premium_bound[2],target:=premium*match_ratio[1]]%>%
  .[premium>=premium_bound[1]&premium<premium_bound[2],target:=premium*match_ratio[2]]%>%
  .[,unmatched_premium:=target]%T>%
  setkey(.,userid)

asset_list<-copy(asset_data)
user_list<-copy(user_data)

match_record<-data.table()
i=1
while(nrow(asset_list[unmatched_amount>low_amount&avail_num>0,])>0){
  wait_user_data<-setorder(user_list,-unmatched_premium)%>%
    head(.,1)
  
  if(nrow(asset_list[unmatched_amount>=wait_user_data$unmatched_premium,])>0){
    avail_asset_list<-asset_list[unmatched_amount>=wait_user_data$unmatched_premium,]%T>%
      setorder(.,-avg_avail_amount)%>%
      head(.,1)
    
    temp<-data.table(id=i,
                     asset_id=avail_asset_list$id,
                     userid=wait_user_data$userid,
                     amount=wait_user_data$unmatched_premium)
    
    match_record<-rbind(match_record,temp)%T>%
      setkey(.,asset_id)
    
    avail_asset_list_new<-copy(avail_asset_list)%>%
      .[,":="(unmatched_amount=unmatched_amount-wait_user_data$unmatched_premium,
              avail_num=n-length(unique(match_record[asset_id==avail_asset_list$id,]$userid))
              )]%>%
      .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]%T>%
      setkey(.,id)
    
    wait_user_data[,unmatched_premium:=0]
  }
  else{
    avail_asset_list<-asset_list[avg_avail_amount>0,]%T>%
      setorder(.,-unmatched_amount)%>%
      head(.,1)
    
    temp<-data.table(id=i,
                     asset_id=avail_asset_list$id,
                     userid=wait_user_data$userid,
                     amount=avail_asset_list$unmatched_amount)
    
    match_record<-rbind(match_record,temp)%T>%
      setkey(.,asset_id)
    
    avail_asset_list_new<-copy(avail_asset_list)%>%
      .[,":="(unmatched_amount=0,
              avail_num=n-length(unique(match_record[asset_id==avail_asset_list$id,]$userid)))]%>%
      .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]%T>%
      setkey(.,id)
    
    wait_user_data[,unmatched_premium:=unmatched_premium-avail_asset_list$unmatched_amount]
  }
  
  asset_list<-asset_list[id==avail_asset_list_new$id,
                         ":="(unmatched_amount=avail_asset_list_new$unmatched_amount,
                              avail_num=avail_asset_list_new$avail_num,
                              avg_avail_amount=avail_asset_list_new$avg_avail_amount)]
  
  user_list<-user_list[userid==wait_user_data$userid,
                       unmatched_premium:=wait_user_data$unmatched_premium]
  print(paste(i,"finished!"))
  i=i+1
}