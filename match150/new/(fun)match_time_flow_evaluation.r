match_time_flow_evaluation<-function(time_seq,
                                     index,
                                     in_asset,
                                     out_asset,
                                     invest,
                                     extra_invest_now,
                                     redeem,
                                     redeem_record,
                                     redeem_asset,
                                     redeem_frozen_now,
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
in_asset_seq<-filter(in_asset,create_time>=start&create_time<end)
#--当前时间片内出库的资产信息
out_asset_seq<-filter(out_asset,end_time>=start&end_time<end)
#--当前时间片内发起的投资序列
invest_seq<-filter(invest,create_time>=start&create_time<end)%>%mutate(.,remark=as.integer(NA))
#--当前时间片内发起的赎回，加权重，加赎回序号，加赎回成功状态判定。status：1.未完成，2.已完成。
redeem_seq<-filter(redeem,create_time>=start&create_time<end)%>%
  arrange(.,create_time)%>%
  rownames_to_column(.,"id")%>%
  mutate(.,id=as.integer(id)+nrow(redeem_record),
         status=1,update_time=create_time)%>%
  select(.,id,everything())
#--赎回总记录更新
redeem_record<-rbind(redeem_record,redeem_seq)


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
  select(.,userid,create_time,amount=unmatched_premium,type)%>%
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
  redeem_asset<-arrange(redeem_asset,userid,unmatched_amount)
  for(m in 1:nrow(redeem_asset)){
    redeem_asset_focus<-redeem_asset[m,]                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
    type_focus<-redeem_asset_focus$type
    
    #对于剩余可匹人数仅剩1人，且资产赎回额等于该用户在该资产的持有总额时，需要验证是否可以冲抵完，如若不能，则不可冲抵
    if(redeem_asset_focus$avail_num==1&redeem_asset_focus$unmatched_amount==redeem_asset_focus$full_amount){
      check_amount<-filter(extra_invest_now,
                           isFrozen==1,
                           userid==redeem_asset_focus$userid,
                           type==type_focus)$unmatched_premium%>%sum(.)+{
                             cash_flow<-switch(type_focus+1,
                                               current_now,
                                               tPlus_now)%>%
                               filter(.,userid==redeem_asset_focus$userid)$unmatched_premium%>%sum(.)
                           }
      if(check_amount<redeem_asset_focus$unmatched_amount){
        next
      }
    }
    #优先选取冻结在extra_invest队列里的资金
    extra_invest_focus<-filter(extra_invest_now,
                               isFrozen==1,
                               userid==redeem_asset_focus$userid,
                               type==type_focus)%>%
      select_(.,.dots=c("id","unmatched_premium"))%>%
      arrange(.,unmatched_premium)
    
    if(nrow(extra_invest_focus)>0){
      extra_invest_focus<-mutate(extra_invest_focus,
                                 unmatched_premium_new=fun.cumminus(unmatched_premium,redeem_asset_focus$unmatched_amount))%>%
        mutate(.,amount_change=unmatched_premium-unmatched_premium_new)
      
      #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
      redeem_frozen_temp<-data.frame(redeemID=redeem_asset_focus$redeemID,
                                     amount=extra_invest_focus$amount_change,
                                     type=type_focus,
                                     create_time=start)
      redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
      
      #更新待匹配赎回资产的待冲抵状态
      redeem_asset_focus<-mutate(redeem_asset_focus,unmatched_amount=unmatched_amount-sum(extra_invest_focus$amount_change))
      #更新额外投资队列的状态
      extra_invest_now<-left_join(extra_invest_now,select(extra_invest_focus,id,unmatched_premium_new),
                                  by=c("id"="id"))%>%
        mutate(.,unmatched_premium=unmatched_premium_new)%>%
        select(.,-unmatched_premium_new)%>%
        filter(.,unmatched_premium>0)
    }
    #如果待冲抵赎回资产额度仍有剩余，则从资金队列里继续进行冲抵
    if(redeem_asset_focus$unmatched_amount>0){
      
      cash_flow<-switch(type_focus+1,
                        current_now,
                        tPlus_now)
      cash_flow_focus<-filter(cash_flow,userid==redeem_asset_focus$userid)
      
      #可冲抵的资金队列按照标签降序（紧急度降序）排列
      if(nrow(cash_flow_focus)>0){
        cash_flow_focus<-arrange(cash_flow_focus,-remark)%>%
          mutate(.,unmatched_premium_new=fun.cumminus(unmatched_premium,redeem_asset_focus$unmatched_amount))%>%
          mutate(.,amount_change=unmatched_premium-unmatched_premium_new)
        
        #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
        redeem_frozen_temp<-data.frame(redeemID=redeem_asset_focus$redeemID,
                                       amount=cash_flow_focus$amount_change,
                                       type=type_focus,
                                       create_time=start)
        redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
        
        redeem_asset_focus<-mutate(redeem_asset_focus,unmatched_amount=unmatched_amount-sum(cash_flow_focus$amount_change))
        
        cash_flow<-left_join(cash_flow,select(cash_flow_focus,id,unmatched_premium_new),by=c("userid"="userid","remark"="remark"))%>%
          mutate(.,unmatched_premium=ifelse(is.na(unmatched_premium_new),unmatched_premium,unmatched_premium_new))%>%
          select(.,-unmatched_premium_new)%>%
          filter(.,unmatched_premium>0)
        
        #更新资金队列
        name<-switch(type_focus+1,"current","tPlus","regular")
        eval(parse(text = paste0(name,"_now<-cash_flow")))
      }else{next}
    }
    redeem_asset<-left_join(redeem_asset,select(redeem_asset_focus,id,unmatched_amount_new=unmatched_amount),by=c("id"="id"))%>%
        mutate(.,unmatched_amount=unmatched_amount_new)%>%
        select(.,-unmatched_amount_new)
  }
  redeem_asset<-filter(redeem_asset,unmatched_amount>0)
  #输出成功！
  print(paste(index,"[",start,"-",end,")",":the relief of redeem_asset is OK!"))
}

# 1.3.2 本时间段的新赎回请求的处理：资金冲抵、得出赎回资产
if(nrow(redeem_seq)>0){
  #本时间段的赎回请求进行冲抵时，按额度顺序排列，进行遍历
  redeem_seq<-arrange(redeem_seq,userid,amount)
  for(m in 1:nrow(redeem_seq)){
    redeem_seq_focus<-redeem_seq[m,]
    type_focus<-redeem_seq_focus$type
    
    #优先选取冻结在extra_invest队列里的资金
    extra_invest_focus<-filter(extra_invest_now,
                               isFrozen==1,
                               userid==redeem_seq_focus$userid,
                               type==type_focus)%>%
      select_(.,.dots=c("id","unmatched_premium"))%>%
      arrange(.,unmatched_premium)
    
    if(nrow(extra_invest_focus)>0){
      extra_invest_focus<-mutate(extra_invest_focus,
                                 unmatched_premium_new=fun.cumminus(unmatched_premium,redeem_seq_focus$amount))%>%
        mutate(.,amount_change=unmatched_premium-unmatched_premium_new)
      
      #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
      redeem_frozen_temp<-data.frame(redeemID=redeem_seq_focus$id,
                                     amount=extra_invest_focus$amount_change,
                                     type=type_focus,
                                     create_time=start)
      redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
      
      #更新待匹配赎回资产的待冲抵状态
      redeem_seq_focus<-mutate(redeem_seq_focus,amount=amount-sum(extra_invest_focus$amount_change))
      #更新额外投资队列的状态
      extra_invest_now<-left_join(extra_invest_now,select(extra_invest_focus,id,unmatched_premium_new),
                                  by=c("id"="id"))%>%
        mutate(.,unmatched_premium=unmatched_premium_new)%>%
        select(.,-unmatched_premium_new)%>%
        filter(.,unmatched_premium>0)
    }
    
    #如果待冲抵赎回资金额度仍有剩余，则从资金队列里继续进行冲抵
    if(redeem_seq_focus$amount>0){
      
      cash_flow<-switch(type_focus+1,
                        current_now,
                        tPlus_now)
      cash_flow_focus<-filter(cash_flow,userid==redeem_seq_focus$userid)
      
      #可冲抵的资金队列按照标签降序（紧急度降序）排列
      if(nrow(cash_flow_focus)>0){
        cash_flow_focus<-arrange(cash_flow_focus,-remark)%>%
          mutate(.,unmatched_premium_new=fun.cumminus(unmatched_premium,redeem_seq_focus$amount))%>%
          mutate(.,amount_change=unmatched_premium-unmatched_premium_new)
        
        #记录冲抵的资金，暂时冻结，等赎回成功时才能解冻
        redeem_frozen_temp<-data.frame(redeemID=redeem_seq_focus$id,
                                       amount=cash_flow_focus$amount_change,
                                       type=type_focus,
                                       create_time=start)
        redeem_frozen_now<-rbind(redeem_frozen_now,redeem_frozen_temp)
        
        redeem_seq_focus<-mutate(redeem_seq_focus,amount=amount-sum(cash_flow_focus$amount_change))
        
        cash_flow<-left_join(cash_flow,
                             select(cash_flow_focus,userid,remark,unmatched_premium_new),
                             by=c("userid"="userid","remark"="remark"))%>%
          mutate(.,unmatched_premium=ifelse(is.na(unmatched_premium_new),unmatched_premium,unmatched_premium_new))%>%
          select(.,-unmatched_premium_new)%>%
          filter(.,unmatched_premium>0)
        
        #更新资金队列
        name<-switch(type_focus+1,"current","tPlus","regular")
        eval(parse(text = paste0(name,"_now<-cash_flow")))
      }
      
      #如果还有待赎回资金，则需要从匹配资产里赎回  
      if(redeem_seq_focus$amount>0){
        asset_focus<-filter(match_status_now,
                            userid==redeem_seq_focus$userid,
                            type==type_focus)%>%
          select(.,-id)%>%
          left_join(.,select_(asset_now,.dots=c("id","unmatched_amount","avail_num","deadline")),by=c("asset_id"="id"))
          
        #--调用函数fun.redeem_select_asset函数决定赎回的资产及其对应的相应信息
        redeem_output<-fun.redeem_select_asset(redeem_seq_focus$amount,asset_focus)
        #----输出赎回的资产信息
        redeem_asset_temp<-redeem_output$extra_asset%>%
          tbl_df(.)%>%
          mutate(.,
                 redeemID=redeem_seq_focus$'id',
                 userid=redeem_seq_focus$'userid',
                 type=type_focus,
                 create_time=redeem_seq_focus$'create_time')%>%
          {
            asset_focus2<-filter(match_status_now,userid==redeem_seq_focus$userid)%>%
              group_by(.,asset_id)%>%
              summarise(.,full_amount=sum(amount))
              left_join(.,asset_focus2,by=c("asset_id"="asset_id"))
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
                                                                  isFrozen=1)]%>%
            tbl_df(.)%>%
            rownames_to_column(.,"id")%>%
            mutate(.,id=as.numeric(id)+nrow(extra_invest))
          #----累加到额外解绑资金序列
          extra_invest<-rbind(extra_invest,extra_invest_temp)
        }
      }else{next}
    }else{next}
  }
}
#更新赎回状态
redeem_status<-group_by(redeem_asset,redeemID)%>%
  summarise(.,remain_redeem=sum(unmatched_amount))
#更新赎回记录
redeem_record<-left_join(redeem_record,redeem_status,by=c("id"="redeemID"))%>%
  mutate(.,status=ifelse(is.na(remain_redeem),2,ifelse(remain_redeem==0,2,1)))%>%
  select(.,-remain_redeem)

#更新冻结的额外投资记录（解冻操作）
extra_invest_now<-left_join(extra_invest_now,select(redeem_record,id,status),by=c("redeemID"="id"))%>%
  mutate(.,isFrozen=ifelse(status==2,0,1))%>%
  select(.,-status)

#更新冻结的冲抵资金记录（解冻操作）
redeem_frozen_now<-left_join(redeem_frozen_now,select(redeem_record,id,status),by=c("redeemID"="id"))%>%
  filter(.,status==1)%>%
  select(.,-status)
#输出成功！
print(paste(index,"[",start,"-",end,")",": redeem OK!"))


# 1.4 赎回资产处理（redeem_asset+match_status_now=>asset_now）
#--产出：更新资产状态表，以便产生待匹配资产队列
#--虚拟化处理，如果赎回立即成功，则资产情况是什么样的
if(nrow(redeem_asset)>0){
  #----虚拟在匹配记录中作出变化，并求出虚拟变化后资产情况如何
  asset_redeem_dummy<-copy(match_status_now)%>%
    left_join(.,
              select_(redeem_asset,.dots=c("userid","type","id","unmatched_amount")),
              by=c("userid"="userid","type"="type","asset_id"="id"))%>%
    mutate(.,amount=amount-unmatched_amount)%>%
    filter(.,amount>0)%>%
    group_by(.,asset_id)%>%
    summarise(.,matched_amount=sum(amount),num=n_distinct(userid))%>%
    left_join(.,group_by(redeem_asset,id)%>%summarise(.,redeem_time=min(create_time)),by=c("asset_id"="id"))%>%
    select_(.,.dots=c("asset_id","matched_amount","num","redeem_time"))

  #----将这种虚拟影响传递给当前资产状态表，以产出待匹配资产队列
  
  asset_now<-left_join(asset_now,asset_redeem_dummy,by=c("id"="asset_id"))%>%
    mutate(.,isredeemed=ifelse(is.na(redeem_time),0,1),
             unmatched_amount=amount-matched_amount,
             avail_num=n-ifelse(is.na(num),0,num))%>%
    select(.,-matched_amount,-num)
}
#输出成功！
print(paste(index,"[",start,"-",end,")",": redeem asset summarise OK!"))

# 1.5 入库资产处理
#--产出：资产队列增加入库资产
if(nrow(in_asset_seq)>0){
  asset_now<-rbind(asset_now,in_asset_seq)
}
#输出成功！
print(paste(index,"[",start,"-",end,")",":asset OK!"))


############### 2.资产匹配 ##################
# 2.1 资金队列
regular_seq<-copy(regular_now)
tPlus_seq<-copy(tPlus_now)
current_seq<-copy(current_now)

# 2.2 资产匹配情况，待循环更新写入
match_record<-copy(match_status_now)

# 2.3 当前赎回状态（涉及单赎回ID对应多笔资产）
redeem_asset_snap<-copy(redeem_asset)

if(nrow(filter(asset_now,unmatched_amount>0))>0){
  # 2.4 资产队列，并加权重排序
  asset_data<-copy(asset_now)%>%
    filter(.,unmatched_amount>0)%>%
    mutate(.,weight=fun.asset_weight(unmatched_amount = unmatched_amount,
                                redeem_time = redeem_time,
                                now_time = end,
                                deadline = deadline,
                                issettled = issettled)
          )%>%
    arrange(.,-weight)
  #--资产序列在过程中不断变化，因此创建一个初始副本（asset_data）  
  asset_list<-copy(asset_data)
  
  #输出匹配开始！
  print(paste(index,"[",start,"-",end,")",":match loop start! total_num is",nrow(asset_list)))
  
  # 2.5 资产匹配主循环
  for(i in 1:nrow(asset_data)){
    #--匹配参数初始值
    r_regular=nrow(regular_seq)/sum(filter(asset_list,unmatched_amount>0)$avail_num)%>%ceiling(./0.05)*0.05
    r_tPlus=1
    
    #--wait_asset直接从asset_data选取，因此顺序不会发生变化
    wait_asset<-asset_data[i,]
    
    #--形成资金队列，并按权重排列
    regular_seq<-filter(regular_seq,unmatched_premium>0)
    tPlus_seq<-filter(tPlus_seq,unmatched_premium>0)
    current_seq<-filter(current_seq,unmatched_premium>0)

    if(nrow(regular_seq)>0){
      regular_seq<-mutate(.,weight=fun.invest(unmatched_premium = unmatched_premium,
                                              log_time = log_time,
                                              now_time = end,
                                              remark = remark))%>%
        arrange(.,-weight)
    }
    
    if(nrow(tPlus_seq)>0){
      tPlus_seq<-mutate(.,weight=fun.invest(unmatched_premium = unmatched_premium,
                                              log_time = log_time,
                                              now_time = end,
                                              remark = remark))%>%
        arrange(.,-weight)
    }
    
    if(nrow(current_seq)>0){
      current_seq<-mutate(.,weight=fun.invest(unmatched_premium = unmatched_premium,
                                              log_time = log_time,
                                              now_time = end,
                                              remark = remark))%>%
        arrange(.,-weight)
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
      mutate(.,asset_id=wait_asset$id,id=id+nrow(match_record),log_time=end)
    
    #如果该资产属于赎回资产（与赎回相关），则更新赎回记录中的状态，并更新资产匹配表的状态
    if(wait_asset$isredeemed){
      
      #得到本次资产成功匹配带来的匹配影响
      redeem_result<-filter(redeem_asset,id==wait_asset$id)%>%
        select_(.,.dots=c("id","redeemID","unmatched_amount"))%>%
        left_join(.,select_(redeem_record,.dots=c("id","type","userid")),by=c("redeemID"="id"))%>%
        select(.,-redeemID)
      
      #更新资产匹配状态明细
      match_record<-left_join(match_record,redeem_result,by=c("asset_id"="id","userid"="userid","type"="type"))%>%
        mutate(.,amount=amount-unmatched_amount,log_time=end)%>%
        select(.,unmatched_amount)%>%
        filter(.,amount>0)
      
      #赎回资产列表去掉这个资产的信息
      redeem_asset<-redeem_asset[id!=wait_asset$id,]
      #更新赎回状态表
      redeem_status<-group_by(redeem_asset,redeemID)%>%
        summarise(.,remain_redeem=sum(unmatched_amount))
    }
    # 如果该资产之前因为未匹配而没有打款，则此时更新，表示可以打款了
    if(!wait_asset$issettled){
      wait_asset<-mutate(wait_asset,issettled=1,settled_time=end)
    }
    
    #增加资产匹配信息内容，并做聚合
    match_record<-rbind(match_record,temp)%>%
      group_by(.,asset_id,userid,type)%>%
      summarise(.,id=max(id),amount=sum(amount),log_time=max(log_time))
    
    
    #更新资产序列
    wait_asset<-mutate(wait_asset,
                       unmatched_amount=unmatched_amount-sum(temp$amount),
                       avail_num=n-uniqueN(filter(match_record,asset_id==wait_asset$id)$userid))%>%
      select(.,
             id,
             unmatched_amount_new=unmatched_amount,
             avail_num_new=avail_num,
             issettled_new=issettled,
             settled_time_new=settled_time)

    
    asset_list<-left_join(asset_list,wait_asset,by=c("id"="id"))%>%
      mutate(.,
             unmatched_amount=unmatched_amount_new,
             avail_num=avail_num_new,
             issettled=issettled_new,
             settled_time=settled_time_new)%>%
      select(.,-unmatched_amount_new,-avail_num_new,-issettled_new,-settled_time)
    
    #更新资金序列
    if(nrow(filter(temp,type==2))>0){
      regular_seq<-left_join(regular_seq,
                             filter(temp,type==2)%>%select(.,userid,amount),
                             by=c("userid"="userid"))%>%
        mutate(.,unmatched_premium=unmatched_premium-amount)%>%
        select(.,-amount)
    }
    if(nrow(filter(temp,type==1))>0){
      tPlus_seq<-left_join(tPlus_seq,
                             filter(temp,type==1)%>%select(.,userid,amount),
                             by=c("userid"="userid"))%>%
        mutate(.,unmatched_premium=unmatched_premium-amount)%>%
        select(.,-amount)
    }
    if(nrow(filter(temp,type==0))>0){
      current_seq<-left_join(current_seq,
                             filter(temp,type==0)%>%select(.,userid,amount),
                             by=c("userid"="userid"))%>%
        mutate(.,unmatched_premium=unmatched_premium-amount)%>%
        select(.,-amount)
    }
  }
  #更新赎回记录
  redeem_record<-left_join(redeem_record,redeem_status,by=c("id"="redeemID"))%>%
    mutate(.,status_new=ifelse(is.na(remain_redeem),status,ifelse(remain_redeem==0,2,1)))%>%
    mutate(.,
           update_time=ifelse(status_new>status,end,update_time),
           staus=status_new)%>%
    select(.,-status_new,-remain_redeem)
  
  #更新冻结的额外投资记录（解冻操作）
  extra_invest_now<-left_join(extra_invest_now,select(redeem_record,id,status),by=c("redeemID"="id"))%>%
    mutate(.,isFrozen=ifelse(status==2,0,1))%>%
    select(.,-status)
  
  #更新冻结的冲抵资金记录（解冻操作）
  redeem_frozen_now<-left_join(redeem_frozen_now,select(redeem_record,id,status),by=c("redeemID"="id"))%>%
    filter(.,status==1)%>%
    select(.,-status)
  
  #更新当前资金序列状态，以传递给下个时间片
  regular_now<-copy(regular_seq)%>%
    select(.,-weight)
  tPlus_now<-copy(tPlus_seq)%>%
    select(.,-weight)
  current_now<-copy(current_seq)%>%
    select(.,-weight)
  
  #更新资产匹配状态，以传递给下个时间片
  match_status_now<-filter(match_record,amount>0)
  #更新资产状态，以传递给下个时间片
  asset_now<-left_join(asset_now,
                       select(asset_list,
                              id,
                              unmatched_amount.y=unmatched_amount,
                              avail_num.y=avail_num,
                              issettled.y=issettled,
                              settled_time.y=settled_time),
                       by=c("id"="id"))%>%
    mutate(.,
           unmatched_amount=unmatched_amount.y,
           avail_num=avail_num.y,
           issettled=issettled.y,
           settled_time=settled_time.y)%>%
    select(.,-contains(".y"))%>%
    mutate(.,redeem_time=ifelse(unmatched_amount==0,NA,redeem_time),isredeemed=ifelse(unmatched_amount==0,0,isredeemed))


#检验资产匹配情况
asset_status_summary<-{
  aa<-select_(asset_now,.dots=c("id","amount","unmatched_amount","avail_num","isredeemed"))
  bb<-group_by(match_status_now,asset_id)%>%
    summarise(.,matched_amount=sum(amount),num=n_distinct(userid))
  left_join(aa,bb,by=c("id"="asset_id"))%>%
    mutate(.,total_num=num+avail_num,total_amount=unmatched_amount+matched_amount)
}

cc<-filter(asset_status_summary,isredeemed==0)

if(all.equal(cc$amount,
             cc$total_amount)&
   all.equal(cc$total_num,
             rep(n,nrow(cc))))
{cat(paste(index,"[",start,"-",end,")",":finished!\n------------------\n"))}
}



return(structure(list(regular_now=regular_now,
                      tPlus_now=tPlus_now,
                      current_now=current_now,
                      match_status_now=match_status_now,
                      asset_now=asset_now,
                      asset_status_summary=asset_status_summary,
                      redeem_record=redeem_record,
                      redeem_asset=redeem_asset,
                      redeem_asset_snap=redeem_asset_snap,
                      redeem_status=redeem_status,
                      extra_invest_now=extra_invest_now,
                      redeem_frozen_now=redeem_frozen_now),
                 class="match time flow eval"))

}








