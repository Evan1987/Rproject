match_time_flow_evaluation<-function(time_seq,
                                     index,
                                     in_asset,
                                     out_asset,
                                     invest,
                                     redeem,
                                     redeem_record,
                                     redeem_frozen_now,
                                     redeem_asset,
                                     redeem_status,
                                     extra_invest_now,
                                     asset_now,
                                     match_status_now,
                                     regular_now,
                                     tPlus_now,
                                     current_now,
                                     n=150)
{
#--时间片端点
start=time_seq[index]
end=time_seq[index+1]

#--当前时间片内入库的资产信息
in_asset_seq<-in_asset[create_time>=start&create_time<end,]
#--当前时间片内出库的资产信息
out_asset_seq<-out_asset[create_time>=start&create_time<end,]
#--当前时间片内发起的投资序列
invest_seq<-invest[create_time>=start&create_time<end,]%>%.[,remark:=as.integer(0)]
#--当前时间片内发起的赎回，加权重，加赎回序号，加赎回成功状态判定。status：1.未完成，2.已完成。
redeem_seq<-redeem[create_time>=start&create_time<end,]%T>%
  setorder(.,create_time)%>%
  .[,":="(id=.I+nrow(redeem_record),
          status=1,
          update_time=create_time)]
#--赎回总记录更新
redeem_record<-rbind(redeem_record,redeem_seq)
#--赎回状态更新
redeem_status<-rbind(redeem_status,data.table(redeemID=redeem_seq$id,remain_redeem=redeem_seq$amount))
############### 1.资金资产序列整理 ##################
# 1.1 出库资产的资金处理
#--产出：资产序列去掉出库资产，资金序列增加出库解绑资金
if(nrow(out_asset_seq)>0){
  #在资产队列里删除这些资产的信息
  asset_now<-asset_now[!(id%in%out_asset_seq$id),]
  #在资金队列里添加与出库资产匹配的资金流
  #--总出库资金流
  out_money_flow<-match_status_now[asset_id%in%out_asset_seq$id,]%>%
    .[out_asset_seq[,c("id","end_time")],on=c("asset_id"="id"),nomatch=NA,mult="all"]%>%
    .[,":="(remark=1)]
  #--各类型出库资金流与现有资金流合并
  #----合并过程中，以用户为key，不区分资金来源（未给因出库导致的资金序列加标记），记录时间取最小的
  for(i in 0:2){
    temp<-out_money_flow[type==i,c("userid","amount","end_time","remark")]%>%
      setnames(.,c("amount","end_time"),c("unmatched_premium","log_time"))
    
    switch(i+1,
           current_now<-rbind(current_now,temp)%>%
             .[,.(unmatched_premium=sum(unmatched_premium),
                  log_time=min(log_time)),by=.(userid,remark)],
           tPlus_now<-rbind(tPlus_now,temp)%>%
             .[,.(unmatched_premium=sum(unmatched_premium),
                  log_time=min(log_time)),by=.(userid,remark)],
           regular_now<-rbind(regular_now,temp)%>%
             .[,.(unmatched_premium=sum(unmatched_premium),
                  log_time=min(log_time)),by=.(userid,remark)]
           )
  }
  #在匹配信息中删除这些资产信息
  match_status_now<-match_status_now[!(asset_id%in%out_asset_seq$id),]  
  
}
#输出成功！
print(paste(index,"[",start,"-",end,")",":out_asset OK!"))

# 1.2 正常投资处理（包含额外投资）
#--产出：资金序列增加相应投资资金
#--投资进入相应的资金序列，与原有序列按用户key合并
#--更新额外投资的队列，只保留冻结的（isFrozen==1）
unfreeze_invest_seq<-extra_invest_now[isFrozen==0,]%>%
  .[,c("userid","create_time","unmatched_premium","type")]%>%
  setnames(.,"unmatched_premium","amount")%>%
  .[,remark:=2]

invest_seq<-rbind(invest_seq,unfreeze_invest_seq)
extra_invest_now<-extra_invest_now[isFrozen==1,]

for(k in 0:2){
  tmp<-invest_seq[type==k,c("userid","create_time","amount","remark")]%>%
    {
      setnames(.,c("create_time","amount"),c("log_time","unmatched_premium"))
      switch(k+1,
            rbind(current_now,.),
            rbind(tPlus_now,.),
            rbind(regular_now,.)
      )
    }%>%
    .[,.(unmatched_premium=sum(unmatched_premium),log_time=min(log_time)),by=.(userid,remark)]
  name<-switch(k+1,"current","tPlus","regular")
  eval(parse(text = paste0(name,"_now<-tmp")))
}
#输出成功！
print(paste(index,"[",start,"-",end,")",":invest & unfreezed extra invest OK!"))

# 1.3 赎回处理（超级复杂！）
#--产出：对应类型的未匹配资金减少，产生赎回资产，产生额外解绑投资资金

# 1.3.1 历史残留赎回记录的资金冲抵
if(nrow(redeem_asset)>0){
  #历史赎回记录进行冲抵时，按额度顺序排列，进行遍历
  redeem_asset<-setorder(redeem_asset,userid,unmatched_amount)
  for(m in 1:nrow(redeem_asset)){
    redeem_asset_focus<-redeem_asset[m,]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    type_focus<-redeem_asset_focus$type
    
    #对于剩余可匹人数仅剩1人，且资产赎回额等于该用户在该资产的持有总额时，需要验证是否可以冲抵完，如若不能，则不可冲抵
    if(redeem_asset_focus$avail_num==1&redeem_asset_focus$unmatched_amount==redeem_asset_focus$full_amount)
    {
      check_amount<-sum(extra_invest_now[isFrozen==1&userid==redeem_asset_focus$userid&type==type_focus,]$unmatched_premium)+
        {
          cash_flow<-switch(type_focus+1,
                            current_now,
                            tPlus_now)
          sum(cash_flow[userid==redeem_asset_focus$userid,]$unmatched_premium)
        }
      if(round(check_amount-redeem_asset_focus$unmatched_amount,2)<0){next}
    }
    
    #优先选取冻结在extra_invest队列里的资金
    extra_invest_focus<-extra_invest_now[isFrozen==1&userid==redeem_asset_focus$userid&type==type_focus,
                                         c("id","unmatched_premium")]%>%
      setorder(.,unmatched_premium)

    if(nrow(extra_invest_focus)>0){
      extra_invest_focus[,unmatched_premium_new:=fun.cumminus(unmatched_premium,redeem_asset_focus$unmatched_amount)]%>%
        .[,amount_change:=unmatched_premium-unmatched_premium_new]

      #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
      redeem_frozen_temp<-data.table(redeemID=redeem_asset_focus$redeemID,
                                     amount=extra_invest_focus$amount_change,
                                     type=type_focus,
                                     create_time=start)
      
      redeem_status<-redeem_status[redeem_frozen_temp[,.(amount=sum(amount)),by=redeemID],
                                   on=c("redeemID"="redeemID"),
                                   remain_redeem:=round(remain_redeem-amount,2)]
      
      redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
      
      #更新待匹配赎回资产的待冲抵状态
      redeem_asset_focus[,unmatched_amount:=round(unmatched_amount-sum(extra_invest_focus$amount_change),2)]

      #更新额外投资队列的状态
      extra_invest_now<-extra_invest_now[extra_invest_focus[,c("id","unmatched_premium_new")],
                                         on=c("id"="id"),
                                         unmatched_premium:=round(unmatched_premium_new,2)]%>%
        .[unmatched_premium>0,]
    }
    
    #如果待冲抵赎回资产额度仍有剩余，则从资金队列里继续进行冲抵
    if(redeem_asset_focus$unmatched_amount>0){
      
      cash_flow<-switch(type_focus+1,
                        current_now,
                        tPlus_now)
      cash_flow_focus<-cash_flow[userid==redeem_asset_focus$userid,]
      
      #可冲抵的资金队列按照标签降序（紧急度降序）排列
      if(nrow(cash_flow_focus)>0){
        cash_flow_focus<-setorder(cash_flow_focus,-remark)%>%
          .[,unmatched_premium_new:=fun.cumminus(unmatched_premium,redeem_asset_focus$unmatched_amount)]%>%
          .[,amount_change:=unmatched_premium-unmatched_premium_new]

        #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
        redeem_frozen_temp<-data.table(redeemID=redeem_asset_focus$redeemID,
                                       amount=cash_flow_focus$amount_change,
                                       type=type_focus,
                                       create_time=start)
        
        redeem_status<-redeem_status[redeem_frozen_temp[,.(amount=sum(amount)),by=redeemID],
                                     on=c("redeemID"="redeemID"),
                                     remain_redeem:=round(remain_redeem-amount,2)]
        
        redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
        
        redeem_asset_focus[,unmatched_amount:=round(unmatched_amount-sum(cash_flow_focus$amount_change),2)]

        cash_flow<-cash_flow[cash_flow_focus[,c("userid","remark","unmatched_premium_new")],
                             on=c("userid"="userid","remark"="remark"),
                             unmatched_premium:=round(unmatched_premium_new,2)]%>%
          .[unmatched_premium>0,]
        
        #更新资金队列
        name<-switch(type_focus+1,"current","tPlus","regular")
        eval(parse(text = paste0(name,"_now<-cash_flow")))
      }else{next}
    }
    redeem_asset[id==redeem_asset_focus$id,
                 unmatched_amount:=redeem_asset_focus$unmatched_amount]
  }
  redeem_asset<-redeem_asset[unmatched_amount>0,]
  #更新赎回记录
  redeem_record<-redeem_record[redeem_status,
                               on=c("id"="redeemID"),
                               status:=ifelse(i.remain_redeem==0,2,1)]
  #更新冻结的额外投资记录（解冻操作）
  extra_invest_now<-extra_invest_now[redeem_record[,c("id","status")],on=c("redeemID"="id"),isFrozen:=ifelse(status==2,0,1)]
  #更新冻结的冲抵资金记录（解冻操作）
  redeem_frozen_now<-redeem_frozen_now[redeemID%in%redeem_record[status==1,]$id,]
  #输出成功！
  print(paste(index,"[",start,"-",end,")",":the relief of redeem_asset is OK!"))
}

# 1.3.2 本时间段的新赎回请求的处理：资金冲抵、得出赎回资产
if(nrow(redeem_seq)>0){
  #本时间段的赎回请求进行冲抵时，按额度顺序排列，进行遍历
  redeem_seq<-setorder(redeem_seq,userid,amount)
  for(m in 1:nrow(redeem_seq)){
    redeem_seq_focus<-redeem_seq[m,]
    type_focus<-redeem_seq_focus$type
    
    #优先选取冻结在extra_invest队列里的资金
    extra_invest_focus<-extra_invest_now[isFrozen==1&
                                           userid==redeem_seq_focus$userid&
                                           type==type_focus,][,c("id","unmatched_premium")]%>%
      setorder(.,unmatched_premium)

    if(nrow(extra_invest_focus)>0){
      extra_invest_focus[,unmatched_premium_new:=fun.cumminus(unmatched_premium,redeem_seq_focus$amount)]%>%
        .[,amount_change:=unmatched_premium-unmatched_premium_new]

      #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
      redeem_frozen_temp<-data.table(redeemID=redeem_seq_focus$id,
                                     amount=extra_invest_focus$amount_change,
                                     type=type_focus,
                                     create_time=start)
      
      redeem_status<-redeem_status[redeem_frozen_temp[,.(amount=sum(amount)),by=redeemID],
                                   on=c("redeemID"="redeemID"),
                                   remain_redeem:=round(remain_redeem-amount,2)]
      
      redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
      
      #更新待匹配赎回资产的待冲抵状态
      redeem_seq_focus[,amount:=round(amount-sum(extra_invest_focus$amount_change),2)]

      #更新额外投资队列的状态
      extra_invest_now<-extra_invest_now[extra_invest_focus[,c("id","unmatched_premium_new")],
                                         on=c("id"="id"),
                                         unmatched_premium:=round(unmatched_premium_new,2)]%>%
        .[unmatched_premium>0,]
    }
    
    #如果待冲抵赎回资金额度仍有剩余，则从资金队列里继续进行冲抵
    if(redeem_seq_focus$amount>0){
      
      cash_flow<-switch(type_focus+1,
                        current_now,
                        tPlus_now)
      cash_flow_focus<-cash_flow[userid==redeem_seq_focus$userid,]
      
      #可冲抵的资金队列按照标签降序（紧急度降序）排列
      if(nrow(cash_flow_focus)>0){
        cash_flow_focus<-setorder(cash_flow_focus,-remark)%>%
          .[,unmatched_premium_new:=fun.cumminus(unmatched_premium,redeem_seq_focus$amount)]%>%
          .[,amount_change:=unmatched_premium-unmatched_premium_new]
   
        #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
        redeem_frozen_temp<-data.table(redeemID=redeem_seq_focus$id,
                                       amount=cash_flow_focus$amount_change,
                                       type=type_focus,
                                       create_time=start)
        
        redeem_status<-redeem_status[redeem_frozen_temp[,.(amount=sum(amount)),by=redeemID],
                                     on=c("redeemID"="redeemID"),
                                     remain_redeem:=round(remain_redeem-amount,2)]
        
        redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
        
        redeem_seq_focus[,amount:=round(amount-sum(cash_flow_focus$amount_change),2)]
        cash_flow<-cash_flow[cash_flow_focus[,c("userid","remark","unmatched_premium_new")],
                             on=c("userid"="userid","remark"="remark"),
                             unmatched_premium:=round(unmatched_premium_new,2)]%>%
          .[unmatched_premium>0,]
        #更新资金队列
        name<-switch(type_focus+1,"current","tPlus","regular")
        eval(parse(text = paste0(name,"_now<-cash_flow")))
      }
      
      #如果还有待赎回资金，则需要从匹配资产里赎回  
      if(redeem_seq_focus$amount>0){
        
        asset_focus<-match_status_now[userid==redeem_seq_focus$userid&type==type_focus,][,-"id"]%>%
          asset_now[,c("id","unmatched_amount","avail_num","deadline")][.,on=c("id"="asset_id"),nomatch=0,mult="all"]

        #--调用函数fun.redeem_select_asset函数决定赎回的资产及其对应的相应信息
        redeem_output<-fun.redeem_select_asset(redeem_seq_focus$amount,asset_focus)
        #----输出赎回的资产信息
        redeem_asset_temp<-redeem_output$extra_asset%>%
          .[,":="(redeemID=redeem_seq_focus$id,
                  userid=redeem_seq_focus$userid,
                  type=type_focus,
                  create_time=redeem_seq_focus$create_time)]%>%
                  {
                    asset_focus2<-match_status_now[userid==redeem_seq_focus$userid,]%>%
                      .[,.(full_amount=sum(amount)),by=asset_id]
                    .[asset_focus2,on=c("id"="asset_id"),full_amount:=i.full_amount]
                  }
        #----累加到赎回资产信息表
        redeem_asset<-rbind(redeem_asset,redeem_asset_temp)
        #--如果赎回涉及了额外解绑资金，则需要输出相应信息
        if(redeem_output$extra_label){
          #----输出涉及的额外解绑资金
          extra_invest_temp<-redeem_output$extra_invest%>%.[,":="(redeemID=redeem_seq_focus$id,
                                                                  userid=redeem_seq_focus$userid,
                                                                  create_time=redeem_seq_focus$create_time,
                                                                  type=type_focus,
                                                                  isFrozen=1,
                                                                  id=.I+nrow(extra_invest_now))]%>%
            setnames(.,"amount","unmatched_premium")
          #----累加到额外解绑资金序列
          extra_invest_now<-rbind(extra_invest_now,extra_invest_temp)
          
          #----赎回状态表中remain_redeem需要增加
          redeem_status<-redeem_status[redeemID==redeem_seq_focus$id,
                                       remain_redeem:=remain_redeem+sum(extra_invest_temp$unmatched_premium)]
        }
      }else{next}
    }else{next}
  }
  #更新赎回记录
  redeem_record<-redeem_record[redeem_status,
                               on=c("id"="redeemID"),
                               status:=ifelse(i.remain_redeem==0,2,1)]
  
  #更新冻结的额外投资记录（解冻操作）
  extra_invest_now<-extra_invest_now[redeem_record[,c("id","status")],on=c("redeemID"="id"),isFrozen:=ifelse(status==2,0,1)]
  
  #更新冻结的冲抵资金记录（解冻操作）
  redeem_frozen_now<-redeem_frozen_now[redeemID%in%redeem_record[status==1,]$id,]
}
#输出成功！
print(paste(index,"[",start,"-",end,")",": redeem OK!"))


# 1.4 赎回资产处理（redeem_asset+match_status_now=>asset_now）
#--产出：更新资产状态表，以便产生待匹配资产队列
#--虚拟化处理，如果赎回立即成功，则资产情况是什么样的

if(nrow(redeem_asset)>0){
  #----虚拟在匹配记录中作出变化，并求出虚拟变化后资产情况如何
  asset_redeem_dummy<-copy(match_status_now)%>%
    .[redeem_asset[,c("userid","type","id","unmatched_amount","create_time")],
      on=c("userid"="userid","type"="type","asset_id"="id"),
      amount:=amount-ifelse(is.na(unmatched_amount),0,unmatched_amount)]%>%
    .[amount>0,]%>%
    .[,.(matched_amount=sum(amount),
         num=uniqueN(userid)),
      by=asset_id]%>%
    .[redeem_asset[,.(redeem_time=min(create_time)),by=id],on=c("asset_id"="id"),redeem_time:=i.redeem_time]
  
  #----将这种虚拟影响传递给当前资产状态表，以产出待匹配资产队列
  asset_now<-asset_now[asset_redeem_dummy,
                       on=c("id"="asset_id"),
                       ":="(isredeemed=ifelse(is.na(i.redeem_time),0,1),
                            redeem_time=i.redeem_time,
                            unmatched_amount=amount-ifelse(is.na(matched_amount),0,matched_amount),
                            avail_num=n-ifelse(is.na(num),0,num))]

}
#输出成功！
print(paste(index,"[",start,"-",end,")",": redeem asset summarise OK!"))

# 1.5 入库资产处理
#--产出：资产队列增加入库资产
if(nrow(in_asset_seq)>0){
  asset_now<-rbind(asset_now,in_asset_seq)
  #输出成功！
  print(paste(index,"[",start,"-",end,")",":asset OK!"))
}


############### 2.资产匹配 ##################
# 2.1 资金队列
regular_seq<-copy(regular_now)
tPlus_seq<-copy(tPlus_now)
current_seq<-copy(current_now)

# 2.2 资产匹配情况，待循环更新写入
match_record<-copy(match_status_now)

# 2.3 当前赎回状态（涉及单赎回ID对应多笔资产）
redeem_asset_snap<-copy(redeem_asset)

if(nrow(asset_now[unmatched_amount>0,])>0){
  # 2.4 资产队列，并加权重排序
  asset_data<-copy(asset_now)%>%.[unmatched_amount>0,]%>%
    .[,weight:=fun.asset_weight(unmatched_amount = unmatched_amount,
                                redeem_time = redeem_time,
                                now_time = end,
                                deadline = deadline,
                                issettled = issettled),by=.I]%T>%
    setorder(.,-weight)
  #--资产序列在过程中不断变化，因此创建一个初始副本（asset_data）  
  asset_list<-copy(asset_data)
  
  #输出匹配开始！
  print(paste(index,"[",start,"-",end,")",":match loop start! total_num is",nrow(asset_list)))
  
  # 2.5 资产匹配主循环
  for(i in 1:nrow(asset_data)){
    #--匹配参数初始值
    r_regular=ceiling(nrow(regular_seq)/sum(filter(asset_list,unmatched_amount>0)$avail_num)/0.05)*0.05
    r_tPlus=1
    
    #--wait_asset直接从asset_data选取，因此顺序不会发生变化
    wait_asset<-asset_data[i,]
    
    #--形成资金队列，并按权重排列
    regular_seq<-regular_seq[unmatched_premium>0,]
    tPlus_seq<-tPlus_seq[unmatched_premium>0,]
    current_seq<-current_seq[unmatched_premium>0,]

    if(nrow(regular_seq)>0){
      regular_seq[,weight:=fun.invest(unmatched_premium = unmatched_premium,
                                      log_time = log_time,
                                      now_time = end,
                                      remark = remark),by=.I]%T>%
        setorder(.,-weight)
    }
    
    if(nrow(tPlus_seq)>0){
      tPlus_seq[,weight:=fun.invest(unmatched_premium = unmatched_premium,
                                    log_time = log_time,
                                    now_time = end,
                                    remark = remark),by=.I]%T>%
        setorder(.,-weight)
    }
    
    if(nrow(current_seq)>0){
      current_seq[,weight:=fun.invest(unmatched_premium = unmatched_premium,
                                      log_time = log_time,
                                      now_time = end,
                                      remark = remark),by=.I]%T>%
        setorder(.,-weight)
    }
    #--可行解是否存在下界的判定
    m_regular<-min(floor(r_regular*wait_asset$avail_num),nrow(regular_seq))
    m_tPlus<-min(floor((wait_asset$avail_num-m_regular)*r_tPlus),nrow(tPlus_seq))
    m_current<-min(wait_asset$avail_num-m_regular-m_tPlus,nrow(current_seq))
    
    judge_const<-(
      regular_seq$unmatched_premium%>%
        head(.,m_regular)%>%
        sum(.))+
      (
        tPlus_seq$unmatched_premium%>%
          head(.,m_tPlus)%>%
          sum(.))+
      (
        current_seq$unmatched_premium%>%
          head(.,m_current)%>%
          sum(.)
      )
    
    #--存在可行解则继续，否则跳过此资产
    if(judge_const<wait_asset$unmatched_amount){
      next
    }
    
    #--调用函数：match_temp2进行匹配信息的输出
    #----匹配顺序：定期=>T+N=>活期
    temp<-match_temp2(unmatched_amount = wait_asset$unmatched_amount,
                      avail_num = wait_asset$avail_num,
                      regular_seq = regular_seq,
                      tPlus_seq = tPlus_seq,
                      current_seq = current_seq,
                      r_regular = r_regular,
                      r_tPlus = r_tPlus)%>%
      .[,":="(asset_id=wait_asset$id,id=id+nrow(match_record),log_time=end)]
    
    #如果该资产属于赎回资产（与赎回相关），则更新赎回记录中的状态，并更新资产匹配表的状态
    if(wait_asset$isredeemed){
      #得到本次资产成功匹配带来的匹配影响
      redeem_status<-redeem_status[redeem_asset[id==wait_asset$id,],
                                   on=c("redeemID"="redeemID"),
                                   remain_redeem:=round(remain_redeem-i.unmatched_amount,2)]
      
      redeem_result<-redeem_asset[id==wait_asset$id,c("id","redeemID","unmatched_amount")][
        redeem_record[,c("id","type","userid")],on=c("redeemID"="id"),nomatch=0,mult="all"]

      #更新资产匹配状态明细
      match_record<-match_record[redeem_result,
                                 on=c("asset_id"="id","userid"="userid","type"="type"),
                                 ":="(amount=round(amount-i.unmatched_amount,2),log_time=end)]%>%
        .[amount>0,]

      #赎回资产列表去掉这个资产的信息
      redeem_asset<-redeem_asset[id!=wait_asset$id,]
    }
    # 如果该资产之前因为未匹配而没有打款，则此时更新，表示可以打款了
    if(!wait_asset$issettled){
      wait_asset[,":="(issettled=1,
                       settled_time=end)]
    }
    
    #增加资产匹配信息内容，并做聚合
    match_record<-rbind(match_record,xx<-temp[,-"remark"])%>%
      .[,.(id=max(id),amount=sum(amount),log_time=max(log_time)),by=.(asset_id,userid,type)]

    #更新资产序列
    wait_asset[,":="(unmatched_amount=round(unmatched_amount-sum(temp$amount),2),
                     avail_num=n-uniqueN(match_record[asset_id==wait_asset$id,]$userid))]
    
    asset_list[id==wait_asset$id,
               ":="(unmatched_amount=wait_asset$unmatched_amount,
                    avail_num=wait_asset$avail_num,
                    issettled=wait_asset$issettled,
                    settled_time=wait_asset$settled_time)]
    
    #更新资金序列
    if(nrow(temp[type==2,])>0){
      regular_seq<-regular_seq[temp[type==2,c("userid","amount","remark")],
                               on=c("userid"="userid","remark"="remark"),
                               unmatched_premium:=round(unmatched_premium-amount,2)]
    }
    if(nrow(temp[type==1,])>0){
      tPlus_seq<-tPlus_seq[temp[type==1,c("userid","amount","remark")],
                               on=c("userid"="userid","remark"="remark"),
                               unmatched_premium:=round(unmatched_premium-amount,2)]
    }
    if(nrow(temp[type==0,])>0){
      current_seq<-current_seq[temp[type==0,c("userid","amount","remark")],
                               on=c("userid"="userid","remark"="remark"),
                               unmatched_premium:=round(unmatched_premium-amount,2)]
    }
  }
  #更新赎回记录
  redeem_record<-redeem_record[redeem_status,
                               on=c("id"="redeemID"),
                               status_new:=ifelse(i.remain_redeem==0,2,1)]%>%
    .[!is.na(status_new),":="(update_time=ifelse(status_new>status,end,update_time),status=status_new)]%>%
    .[,-"status_new"]

  #更新冻结的额外投资记录（解冻操作）
  extra_invest_now<-extra_invest_now[redeem_record[,c("id","status")],
                                     on=c("redeemID"="id"),
                                     isFrozen:=ifelse(status==2,0,1)]

  #更新冻结的冲抵资金记录（解冻操作）
  redeem_frozen_now<-redeem_frozen_now[redeemID%in%redeem_record[status==1,]$id,]

  
  #更新当前资金序列状态，以传递给下个时间片
  regular_now<-copy(regular_seq)%>%.[,-"weight"]
  tPlus_now<-copy(tPlus_seq)%>%.[,-"weight"]
  current_now<-copy(current_seq)%>%.[,-"weight"]
  
  redeem_status<-redeem_status[remain_redeem>0,]
  
  #更新资产匹配状态，以传递给下个时间片
  match_status_now<-match_record[amount>0,]
  #更新资产状态，以传递给下个时间片
  
  asset_now<-asset_now[asset_list[,c("id","unmatched_amount","avail_num","issettled","settled_time")],
                       on=c("id"="id"),
                       ":="(unmatched_amount=i.unmatched_amount,
                            avail_num=i.avail_num,
                            issettled=i.issettled,
                            settled_time=i.settled_time)]%>%
    .[unmatched_amount==0,":="(redeem_time=as.POSIXct(NA),isredeemed=0)]

#检验资产匹配情况
  asset_status_summary<-{
    aa<-asset_now[,c("id","amount","unmatched_amount","avail_num","isredeemed")]
    bb<-match_status_now[,.(matched_amount=sum(amount),num=uniqueN(userid)),by=asset_id]
    aa[bb,on=c("id"="asset_id"),nomatch=NA,mult="all"]%>%.[,":="(total_num=num+avail_num,
                                                                 total_amount=unmatched_amount+matched_amount
    )]
  }
  
  cc<-asset_status_summary[isredeemed==0,]
  
  if(all.equal(cc$amount,
               cc$total_amount)&
     all.equal(cc$total_num,
               rep(n,nrow(cc))))
{cat(paste(index,"[",start,"-",end,")",":finished!\n------------------\n"))}
}

return(structure(list(redeem_record=redeem_record,
                      redeem_frozen_now=redeem_frozen_now,
                      redeem_asset=redeem_asset,
                      redeem_status=redeem_status,
                      extra_invest_now=extra_invest_now,
                      asset_now=asset_now,
                      match_status_now=match_status_now,
                      regular_now=regular_now,
                      tPlus_now=tPlus_now,
                      current_now=current_now,
                      asset_status_summary=asset_status_summary,
                      redeem_asset_snap=redeem_asset_snap
                      ),
                 class="match time flow eval"))
}








