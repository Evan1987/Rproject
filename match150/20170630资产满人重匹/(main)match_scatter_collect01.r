library(data.table)
library(magrittr)
library(stringr)
library(RMySQL)
source('~/rstudio/match150/20170630资产满人重匹/(fun)move_info.r', echo=TRUE)
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)
path = "F:/Project/20170603资产匹配穿透200人上线/20170630资产满人数释放/"

conn<-MySQL_conn_select("local")
z0<-"select user_id as userid,asset_id,hold_amount as amount from ast_matched_record where yn=0 and status=1"
res<-dbSendQuery(conn,z0)
final_match_record<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)
dbDisconnect(conn)
# final_match_record<-fread(paste0(path,"result/finalMatchRecord.csv"))
asset_info<-fread(paste0(path,"asset_info.csv"))

full_asset<-final_match_record[,.(num=uniqueN(userid),amount=sum(amount)),by=asset_id]%>%
  .[asset_info,on=c("asset_id"="id"),rate:=i.rate]%>%
  .[amount>=100000,]

rate_set<-unique(full_asset$rate)%>%sort(.)
set_num = length(rate_set)

final_match<-data.table()
index=0
trans_log<-data.table()
for(i in 1:set_num){
  asset_set<-full_asset[rate==rate_set[i],]
  # 该资产集下的匹配记录
  match_record<-final_match_record[asset_id%in%asset_set$asset_id,]
  # 该资产集下的匹配记录汇总
  match_summary<-match_record[,.(amount=sum(amount)),by=.(asset_id,userid)]
  # 筛选该资产集下的高频用户并按照频次降序
  user_match_status<-match_summary[,.(num=uniqueN(asset_id)),by=userid]%>%
    .[num>1,]%>%setorder(.,-num)
  
  duplicate_user = user_match_status$userid
  user_num = length(duplicate_user)
  # 按用户整合
  for(j in 1:user_num){
    # 选定目标整合用户和汇集资产
    target_user <- duplicate_user[j]
    asset_num = uniqueN(match_summary[userid==target_user,]$asset_id)
    if(asset_num<2){
      next
    }
    container_list <- match_summary[userid==target_user,]%>%
      setorder(.,-amount)%>%
      .$asset_id
    
    while(length(container_list)>1){
      container = container_list[1]
      replacer_list = copy(container_list)%>%.[-1]
      replacer_total_summary<-match_summary[asset_id%in%replacer_list,]
      # mover: container资产上除target_user外的高频用户
      mover_list<-match_summary[asset_id==container&
                                  userid!=target_user&
                                  userid%in%replacer_total_summary$userid,]%>%setorder(.,amount)
      if(nrow(mover_list)>0){
        for(k in 1:nrow(mover_list)){
          mover = mover_list[k]
          avail_replacer = {
            pre_avail_list<-replacer_total_summary[userid==mover$userid,]$asset_id
            match_summary[asset_id%in%pre_avail_list&userid==target_user,]$asset_id
          }
          if(length(avail_replacer)==0){
            next
          }else{
            result = move_info(target_user,mover,avail_replacer)
            trans_temp = result$trans_result
            log_temp = result$log_result
            match_summary<-{
              a<-match_summary[!asset_id%in%trans_temp$asset_id,]
              b<-match_summary[asset_id%in%trans_temp$asset_id,]%>%
                rbind(.,trans_temp)%>%
                .[,.(amount=round(sum(amount),2)),by=.(asset_id,userid)]%>%
                .[amount>0,]
              rbind(a,b)
            }
            index = index+1
            print(paste("set:",i,"user_num:",j,"index:",index,"finished!"))
            trans_log<-rbind(trans_log,log_temp[,id:=index])
          }
        }
      }
      container_list <- match_summary[userid==target_user&asset_id%in%replacer_list,]%>%
        setorder(.,-amount)%>%
        .$asset_id
    }
  }
  final_match<-rbind(final_match,match_summary)
}




initial_match <- final_match_record[asset_id%in%full_asset$asset_id,]%>%
  .[,.(amount=sum(amount)),by=.(asset_id,userid)]

asset_summary<-{
  initial_asset_summary<-initial_match[,.(amount=sum(amount),num=uniqueN(userid)),by=asset_id]
  final_asset_summary<-final_match[,.(amount=sum(amount),num=uniqueN(userid)),by=asset_id]
  initial_asset_summary[final_asset_summary,on="asset_id",":="(final_amount=i.amount,
                                                               final_num=i.num)]%>%
    .[,":="(diff_amount=round(final_amount-amount,2),diff_num=final_num-num)]
}

user_summary<-{
  initial_user_summary<-initial_match[,.(amount=sum(amount)),by=userid]
  final_user_summary<-final_match[,.(amount=sum(amount)),by=userid]
  initial_user_summary[final_user_summary,on="userid",final_amount:=i.amount]%>%
    .[,diff_amount:=round(final_amount-amount,2)]
}













