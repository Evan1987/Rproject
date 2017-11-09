move_info<-function(target_user,mover,avail_replacer){
  result<-match_summary[asset_id%in%avail_replacer&userid==target_user,]%T>%
    setorder(.,amount)%>%
    .[,cum_amount:=cumsum(amount)]%>%
    .[,rest:=round(cum_amount-mover$amount,2)]%>%
    .[,trans_amount:=round(amount-ifelse(rest<0,0,rest),2)]%>%
    .[trans_amount>0,c("asset_id","userid","trans_amount")]%>%
    setnames(.,"trans_amount","amount")
  
  trans_result<-{
    mover[,amount:=sum(result$amount)]
    a<-rbind(result,mover)%>%
      .[,amount:=-amount]
    b<-rbind(copy(result)%>%.[,userid:=mover$userid],
             copy(result)%>%.[,asset_id:=mover$asset_id])
    rbind(a,b)
  }
  
  log_result<-{
    vars = c("asset_id","userid")
    a<-mover[,.SD,.SDcols=vars]%>%setnames(.,vars,str_c("mover_",vars))
    b<-copy(result)%>%setnames(.,vars,str_c("replacer_",vars))
    cbind(a,b)
  }
  
  return(list(trans_result=trans_result,log_result=log_result))
}