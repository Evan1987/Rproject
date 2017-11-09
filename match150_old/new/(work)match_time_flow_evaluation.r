


#--时间片端点
index=1
start=time_seq[index]
end=time_seq[index+1]

#--当前时间片内入库的资产信息
in_asset_seq<-filter(in_asset,create_time>=start&create_time<end)
#--当前时间片内出库的资产信息
out_asset_seq<-filter(out_asset,end_time>=start&end_time<end)
#--当前时间片内发起的投资序列
invest_seq<-filter(invest,create_time>=start&create_time<end)%>%mutate(.,remark=as.integer(NA))
#--当前时间片内发起的赎回，加权重，加赎回序号，加赎回成功状态判定。status：1.未完成，2.已完成。
redeem_seq<-filter(redeem,create_time>=start&create_time<end)%>%
  arrange(.,create_time)%>%
  rownames_to_column(.,"id")%>%
  mutate(.,id=as.numeric(id)+nrow(redeem_record),
         status=1,update_time=create_time)%>%
  select(.,id,everything())
#--赎回总记录更新
redeem_record<-rbind(redeem_record,redeem_seq)
#--当前剩余未完成的赎回队列
redeem_now<-filter(redeem_record,status==1)

############### 1.资金资产序列整理 ##################
# 1.1 出库资产的资金处理
#--产出：资产序列去掉出库资产，资金序列增加出库解绑资金
if(nrow(out_asset_seq)>0){
  #在资产队列里删除这些资产的信息
  asset_now<-filter(asset_now,!(id%in%out_asset_seq$id))
  #在资金队列里添加与出库资产匹配的资金流
  #--总出库资金流
  out_money_flow<-filter(match_status_now,asset_id%in%out_asset_seq$id)%>%
    left_join(.,select_(out_asset_seq,.dots=c("id","end_time")),by=c("asset_id"="id"))%>%
    mutate(.,remark=1)
  #--各类型出库资金流与现有资金流合并
  #----合并过程中，以用户为key，不区分资金来源（未给因出库导致的资金序列加标记），记录时间取最小的
  for(i in 0:2){
    temp<-filter(out_money_flow,type==i)%>%
      select_(.,.dots=c("userid","amount","end_time","remark"))%>%
      rename(.,unmatched_premium=amount,log_time=end_time)
    
    switch(i+1,
          current_now<-rbind(current_now,temp)%>%
             group_by(.,userid,remark)%>%
             summarise(.,unmatched_premium=sum(unmatched_premium),log_time=min(log_time)),
          tPlus_now<-rbind(tPlus_now,temp)%>%
             group_by(.,userid,remark)%>%
             summarise(.,unmatched_premium=sum(unmatched_premium),log_time=min(log_time)),
          regular_now<-rbind(regular_now,temp)%>%
             group_by(.,userid,remark)%>%
             summarise(.,unmatched_premium=sum(unmatched_premium),log_time=min(log_time))
    )
  }
  #在匹配信息中删除这些资产信息
  match_status_now<-filter(match_status_now,!asset_id%in%out_asset_seq$id)
}
#输出成功！
print(paste(index,"[",start,"-",end,")",":out_asset OK!"))

# 1.2 正常投资处理（包含额外投资）
#--产出：资金序列增加相应投资资金
#--投资进入相应的资金序列，与原有序列按用户key合并
#--更新额外投资的队列，只保留冻结的（isFrozen==1）
unfreeze_invest_seq<-filter(extra_invest_now,isFrozen==0)%>%
  select_(.,.dots=c("userid","create_time","amount","type"))%>%
  mutate(.,remark=2)
invest_seq<-rbind(invest_seq,unfreeze_invest_seq)
extra_invest_now<-filter(extra_invest_now,isFrozen==1)

for(k in 0:2){
  tmp<-filter(invest_seq,type==k)%>%
    select_(.,.dots=c("userid","create_time","amount","remark"))%>%
    rename(.,log_time=create_time,unmatched_premium=amount)%>%{
    switch(k+1,
           rbind(current_now,.),
           rbind(tPlus_now,.),
           rbind(regular_now,.)
    )
  }%>%
    group_by(.,userid,remark)%>%
    summarise(.,unmatched_premium=sum(unmatched_premium),log_time=min(log_time))
  
  name<-switch(k+1,"current","tPlus","regular_now")
  eval(parse(text = paste0(name,"_now<-tmp")))
}

#输出成功！
print(paste(index,"[",start,"-",end,")",":invest & unfreezed extra invest OK!"))

# 1.3 赎回处理（超级复杂！）
#--产出：对应类型的未匹配资金减少，产生赎回资产，产生额外解绑投资资金

# 1.3.1 历史残留赎回记录的资金冲抵
#--产出：更新赎回资产中的待匹配资产信息，更新资金序列
#--之前时间片的赎回资产问题再一次在资金队列里进行遍历
if(nrow(redeem_asset)>0){
  #历史赎回记录进行冲抵时，按额度顺序排列，进行遍历
  redeem_asset<-arrange(redeem_asset,userid,unmatched_amount)
  for(m in 1:nrow(redeem_asset)){
    redeem_asset_focus<-redeem_asset[m,]
    type<-redeem_asset_focus$type
    
    #优先选取冻结在extra_invest队列里的资金
    extra_invest_focus<-filter(extra_invest_now,
                               isFrozen==1,
                               userid==redeem_asset_focus$userid,
                               type==redeem_asset_focus$type)%>%
      select_(.,.dots=c("id","unmatched_premium"))%>%
      arrange(.,unmatched_premium)
    
    if(nrow(extra_invest_focus)>0){
      extra_invest_focus<-mutate(extra_invest_focus,
                                 unmatched_premium_new=fun.cumminus(unmatched_premium,redeem_asset_focus$amount))%>%
        mutate(.,amount_change=unmatched_premium-unmatched_premium_new)
      
      #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
      redeem_frozen_temp<-data.frame(redeemID=redeem_asset_focus$redeemID,
                                     amount=extra_invest_focus$amount_change,
                                     type=type,
                                     create_time=end)
      redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
      
      #更新待匹配赎回资产的待冲抵状态
      redeem_asset_focus<-mutate(redeem_asset_focus,amount=amount-sum(extra_invest_focus$amount_change))
      #更新额外投资队列的状态
      extra_invest_now<-left_join(extra_invest_now,select(extra_invest_focus,id,unmatched_premium_new),
                                  by=c("id"="id"))%>%
        mutate(.,unmatched_premium=unmatched_premium_new)%>%
        select(.,-unmatched_premium_new)%>%
        filter(.,unmatched_premium>0)
    }
    #如果待冲抵赎回资产额度仍有剩余，则从资金队列里继续进行冲抵
    if(redeem_asset_focus$amount>0){
      
      cash_flow<-switch(type+1,
                        current_now,
                        tPlus_now)
      cash_flow_focus<-filter(cash_flow,userid==redeem_asset_focus$userid)
      
      #可冲抵的资金队列按照标签降序（紧急度降序）排列
      if(nrow(cash_flow_focus)>0){
        cash_flow_focus<-arrange(cash_flow_focus,-remark)%>%
          mutate(.,unmatched_premium_new=fun.cumminus(unmatched_premium,redeem_asset_focus$amount))%>%
          mutate(.,amount_change=unmatched_premium-unmatched_premium_new)
        
        #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
        redeem_frozen_temp<-data.frame(redeemID=redeem_asset_focus$redeemID,
                                       amount=cash_flow_focus$amount_change,
                                       type=type,
                                       create_time=end)
        redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
        
        
        redeem_asset_focus<-mutate(redeem_asset_focus,amount=amount-sum(cash_flow_focus$amount_change))
        
        cash_flow<-left_join(cash_flow,select(cash_flow_focus,id,unmatched_premium_new),by=c("userid"="userid","remark"="remark"))%>%
          mutate(.,unmatched_premium=unmatched_premium_new)%>%
          select(.,-unmatched_premium_new)%>%
          filter(.,unmatched_premium>0)
      }else{next}
    }
      
      redeem_asset<-left_join(redeem_asset,select(redeem_asset_focus,id,amount_new=amount),by=c("id"="id"))%>%
        mutate(.,amount=amount_new)%>%
        select(.,-amount_new)
  }
}

if(nrow(redeem_seq)>0){
  
}






redeem_status<-redeem_asset[,.(remain_redeem=sum(unmatched_amount)),by=redeemID]
redeem_record[redeem_status,on=c("id"="redeemID"),":="(status=ifelse(i.remain_redeem==0,2,1),update_time=start)]
redeem_asset<-redeem_asset[unmatched_amount>0,]














