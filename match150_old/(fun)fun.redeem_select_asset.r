
#对已匹配资产的资金进行赎回时，决定选取的资产，并返回其赎回额度信息
#redeem_amount：赎回额度
#asset_focus：可赎回的资产，必须包含的字段有 id（资产ID），amount（该用户在该资产已匹配的额度），avail_num（该资产可匹配人数）
fun.redeem_select_asset<-function(redeem_amount,asset_focus){
  hh<-asset_focus[,c("id","amount","avail_num")]
  remainamount=redeem_amount
  result<-data.table()
  extra_invest<-data.table()
  extra_label<-F
  while(remainamount>0){
    hh[,isfullredeem:=ifelse(amount<=remainamount,1,0)]%>%
      .[,next_avail_num:=isfullredeem+avail_num]%>%
      .[,scores:=ifelse(!isfullredeem,remainamount,amount)/(next_avail_num+0.01)]%T>%
      setorder(.,scores)
    temp<-hh[1,]%>%.[,amount:=min(amount,remainamount)]
    if(temp$next_avail_num==0){
      temp<-hh[1,]%>%.[,next_avail_num:=1]
      extra_invest_temp<-data.table(amount=temp$amount-remainamount)
      extra_label<-T
      extra_invest<-rbind(extra_invest,extra_invest_temp)
    }
    result<-rbind(result,temp)
    hh<-hh[-1,]
    remainamount<-remainamount-temp$amount
  }
  result[,c("scores","avail_num"):=NULL]%T>%
    setnames(.,c("amount","next_avail_num"),c("unmatched_amount","avail_num"))

    return(structure(list(extra_asset=result,
                        extra_invest=extra_invest,
                        extra_label=extra_label),
                   class="redeem influence"))
}