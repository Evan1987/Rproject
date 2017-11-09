library(data.table)
library(magrittr)

path = "F:/meituan_Project/20170928金刚位复购优惠券推荐/for release test/"
campaignList = fread(paste0(path,"campaignListSim.csv"))

sample_user_tenant = fread(paste0(path,"sample_user_tenant.txt"))


# dwa_crm_campaign_user_authority
user_authority = data.table(mt_user_id=unique(sample_user_tenant$mt_user_id))
fwrite(user_authority,paste0(path,"dwa_crm_campaign_user_authority.txt"))

# dwa_crm_campaign_waimai_rfm_candidate_daily
# dwa_crm_campaign_birthday_candidate_daily
hit1Item = sample_user_tenant[campaignList[,-c("start_time","end_time")],
                              on="tenant_id",
                              ":="(poi_id=i.poi_id,
                                   coupon_value=i.amt,
                                   campaign_id=i.id,
                                   campaign_type=i.campaign_type)]%>%
  .[,stat_date:='2017-10-29']%>%
  .[complete.cases(.),]


waimai_rfm_hit = hit1Item[campaign_type!=106,]
birthday_hit = hit1Item[campaign_type==106,]

write.csv(waimai_rfm_hit,paste0(path,"dwa_crm_campaign_waimai_rfm_candidate_daily.csv"),row.names = F)
write.csv(birthday_hit,paste0(path,"dwa_crm_campaign_birthday_candidate_daily.csv"),row.names = F)

# dim_crm_campaign_chain
typeTransfer<-function(x){
  switch(x+1,
         106,107,103,104,105,100,101,102)
}
campaignList = campaignList[,campaign_type:=sapply(coupon_type,typeTransfer)]%>%
  .[,-"coupon_type"]

campaignSet = campaignList[,c("id","tenant_id","campaign_type","start_time","end_time")]%>%
  unique(.)%>%
  .[,":="(status=1,dp="ACTIVE")]%>%
  setnames(.,"campaign_type","restrict_type")

write.csv(campaignSet,paste0(path,"dim_crm_campaign_chain.csv"),row.names = F)

# dim_crm_campaign_tenant_poi
campaignPoiMap = campaignList[,c("id","tenant_id","poi_id")]%>%
  unique(.)%T>%
  setnames(.,"id","campaign_id")%>%
  .[,":="(partition_date = '2017-10-29',id=.I)]%>%
  .[,c("id","campaign_id","tenant_id","poi_id","partition_date")]

write.csv(campaignPoiMap,paste0(path,"dim_crm_campaign_tenant_poi.csv"),row.names = F)
  
