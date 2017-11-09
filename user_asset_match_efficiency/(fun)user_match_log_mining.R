# Mining the useful efficiency data from raw matching log of one specified person.
# e.g. In consideration of meaningful demand, the input data must belong one person
user_match_log_mining<-function(o1){
  userid<-o1$userid[1]
  if(min(o1$log_create_date)>"2016-12-31"){
    blank<-head(o1,1)%>%
      .[,":="(ratio=0,premium=0,matched_amount=0,outed_unmatched_amount=0,unouted_unmatched_amount=0)]
    o1<-rbind(blank,o1)
  }
  
  o1[,":="(premium_diff=round(c(0,diff(premium)),2),
           id=as.numeric(row.names(o1)))]
  o1[premium_diff>=100,remarks:="invest"]%>%
    .[premium_diff<=-100,remarks:="redeem"]%T>%
    setkey(.,id)
  
  startid<-o1[remarks=="invest",]$id
  num<-length(startid)
  
  #如果没有出现投资行为，则输出空结果
  if(num==0){
    return(data.table())
  }
  else{
    cycleid<-c(startid,max(o1$id))
    result<-data.table()
    for(i in 1:num){
      j<-startid[i]
      start_ratio<-o1[id==j-1,]$matched_amount/o1[id==j,]$premium*100
      start_date<-o1[id==j,]$log_create_date
      isbreak<-0
      while(j<cycleid[i+1]){
        if(!is.na(o1[id==j,]$remarks)& o1[id==j,]$remarks=="redeem"){
          #用户赎回中，已匹配金额的赎回量小于赎回前的匹配比例（赎回后比例上升），
          #则认为这种赎回变相加速了匹配效率，在赎回前截止。
          if(o1[id==j,]$ratio>o1[id==j-1,]$ratio){
            end_date<-o1[id==j-1,]$log_create_date
            end_ratio<-o1[id==j-1,]$ratio
            premium<-o1[id==j-1,]$premium
            isbreak<-1
            break
          }
        }
        if(o1[id==j,]$ratio>=80){
          end_date<-o1[id==j,]$log_create_date
          end_ratio<-o1[id==j,]$ratio
          premium<-o1[id==j,]$premium
          isbreak<-1
          break
        }
        j=j+1
        isbreak<-0
      }
      if(!isbreak){
        if(j==max(o1$id)){
          end_date<-o1[id==j,]$log_create_date
          end_ratio<-o1[id==j,]$ratio
          premium<-o1[id==j,]$premium
        }
        else{
          end_date<-o1[id==j-1,]$log_create_date
          end_ratio<-o1[id==j-1,]$ratio
          premium<-o1[id==j-1,]$premium
        }
      }
      log<-data.table(type=o1[id==startid[i],]$type,start_date,start_ratio,end_date,end_ratio,premium)
      result<-rbind(result,log)
    }
    result[,userid:=userid]
  }
  return(result)
}

