library(data.table)
library(magrittr)
library(stringr)

fun.type<-function(x){
  switch(x,
         "current",
         "tPlus",
         "regular")
}

path="F:/Project/20170527研发小额模拟/"


asset_data<-fread(paste0(path,"ast_matching_asset_group.csv"))

asset_summary<-asset_data[,.(amount=sum(amount),n=.N),by=asset_type] # 1=>big 2=>small

match_record<-fread(paste0(path,"ast_matched_record.csv"))%>%
  .[,type_name:=sapply(type,fun.type)]%>%
  .[,-"type"]%T>%
  setnames(.,"type_name","type")
user_info<-fread(paste0(path,"ast_money_account.csv"))%>%
  .[,type_name:=sapply(type,fun.type)]%>%
  .[,-"type"]%T>%
  setnames(.,"type_name","type")

user_unmatch_summary<-user_info[,.(unmatched_amount=sum(unmatch_amount)),by=.(userid,type)]%>%
  dcast(.,userid~type,value.var="unmatched_amount",fill=0)
user_match_summary<-match_record[,.(matched_amount=sum(match_amount)),by=.(userid,type)]%>%
  dcast(.,userid~type,value.var="matched_amount",fill=0)


user_list<-user_unmatch_summary[user_match_summary,
                                on="userid",
                                ":="(matched_regular=i.regular,matched_current=i.current)]%>%
  .[,":="(matched_regular=replace(matched_regular,is.na(matched_regular),0),
          matched_current=replace(matched_current,is.na(matched_current),0))]%>%
  .[,":="(premium_regular=regular+matched_regular,premium_current=current+matched_current)]%>%
  .[,premium:=premium_regular+premium_current]%>%
  {
    vars=names(.)%>%.[which(str_detect(.,"premium"))]%>%c("userid",.)
    .[,.SD,.SDcols=vars]
  }


user_list_adjusted<-copy(user_list)%>%
  .[,premium_regular:=floor(premium_regular/1000)*1000]%>%
  .[,premium:=premium_current+premium_regular]


write.csv(user_list_adjusted,paste0(path,"user_list_adjusted.csv"),row.names = F)


















