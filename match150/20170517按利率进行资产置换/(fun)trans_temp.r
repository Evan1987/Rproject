trans_temp<-function(wait_trans,wanna_down_list,match_record){
  # 如果要交换的资金超过wanna_down用户的总资金，则wanna_down资产的avail_num可以多一个人
  if(wait_trans$avail_num==0){
    wanna_down_list<-wanna_down_list[amount>=wait_trans$amount,]
  }
  wanna_down_list<-wanna_down_list[,avail_num:=avail_num+ifelse(total_amount>wait_trans$amount,0,1)]%>%
    .[avail_num>0,]
  if(nrow(wanna_down_list)==0){
    return(data.table())
  }else{
    wanna_down_target<-head(wanna_down_list,1)
    trans_amount=min(wanna_down_target$amount[1],wait_trans$amount[1])
    vars=c("id","userid","asset_id","type")
    result<-{
      a<-wait_trans[,.SD,.SDcols=vars]
      b<-wanna_down_target[,.SD,.SDcols=vars]
      x<-rbind(a,b)%>%.[,amount:=-trans_amount]
      y<-copy(x)[,asset_id:=rev(asset_id)]%>%
        .[,amount:=trans_amount]
      rbind(x,y)
    }
    return(result)
  } 
  
}