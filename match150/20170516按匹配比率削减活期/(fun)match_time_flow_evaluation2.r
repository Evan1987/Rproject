######函数输入######
#--time_seq：时间流的序列
#--index：表示第几个时间片，单时间片[time_seq[index],time_seq[index+1])
#--in_asset：总的入库资产信息流
#--out_asset：总的出库资产信息流
#--invest：总的投资信息流
#--redeem：总的赎回信息流
#--redeem_record：赎回处理记录汇总
#--redeem_log：赎回处理的资金信息（辅助redeem_record），基本没啥用(+﹏+)~晕
#--redeem_asset：赎回资产信息
#--asset_now：当前资产状态
#--match_status_now：当前资产匹配状态
#--regular_now，tPlus_now,current_now：当前定期、T+N、活期投资序列。
#--n：单资产最大匹配人数

#单一时间片的匹配全过程模拟函数
match_time_flow_evaluation<-function(time_seq,
                                    index,
                                    in_asset,
                                    out_asset,
                                    invest,
                                    redeem,
                                    redeem_record,
                                    redeem_log,
                                    redeem_asset,
                                    asset_now,
                                    match_status_now,
                                    regular_now,
                                    tPlus_now,
                                    current_now,
                                    target_ratio,
                                    n=150)
{ 

  ################ 0.信息汇总与整理 ##################

  # 重要参数的对照函数
  fun.type_name<-function(type){
    name<-switch(type+1,
                 "current",
                 "tPlus",
                 "regular")
    return(name)
  }
  #--时间片端点
  start=time_seq[index]
  end=time_seq[index+1]
  #--当前时间片内入库的资产信息
  in_asset_seq<-in_asset[create_time>=start&create_time<end,]
  #--当前时间片内出库的资产信息
  out_asset_seq<-out_asset[end_time>=start&end_time<end,]
  #--当前时间片内发起的投资序列
  invest_seq<-invest[create_time>=start&create_time<end,]%>%.[,":="(remark=NA)]
  #--当前时间片内发起的赎回，加权重，加赎回序号，加赎回成功状态判定。status：1.未完成，2.已完成。
  redeem_seq<-redeem[create_time>=start&create_time<end,]%T>%
    setorder(.,create_time)%>%
    .[,":="(id=.I+nrow(redeem_record),status=1,update_time=create_time)]
  #--赎回总记录更新
  redeem_record<-rbind(redeem_record,redeem_seq)
  #--当前剩余未完成的赎回队列
  redeem_now<-redeem_record[status==1,]
  
  
  
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
      .[,":="(remark="out_asset")]
    
    #--各类型出库资金流与现有资金流合并
	#----合并过程中，以用户为key，不区分资金来源（未给因出库导致的资金序列加标记），记录时间取最小的
    for(name in c("regular","tPlus","current")){
      temp<-out_money_flow[type==name,c("userid","amount","end_time","remark")]%>%
        setnames(.,c("amount","end_time"),c("unmatched_premium","log_time"))
      
      switch(name,
             "regular"=regular_now<-rbind(regular_now,temp)%>%
               .[,.(unmatched_premium=sum(unmatched_premium),
                    log_time=min(log_time)),by=userid]%>%
               .[,remark:=NA],
             "tPlus"=tPlus_now<-rbind(tPlus_now,temp)%>%
               .[,.(unmatched_premium=sum(unmatched_premium),
                    log_time=min(log_time)),by=userid]%>%
               .[,remark:=NA],
             "current"=current_now<-rbind(current_now,temp)%>%
               .[,.(unmatched_premium=sum(unmatched_premium),
                    log_time=min(log_time)),by=userid]%>%
               .[,remark:=NA]
      )
    }
    #在匹配信息中删除这些资产信息
    match_status_now<-match_status_now[!(asset_id%in%out_asset_seq$id),]
  }
  #输出成功！
  print(paste(index,"[",start,"-",end,")",":out_asset OK!"))
  
  # 1.2 正常投资处理
  #--产出：资金序列增加相应投资资金
  #--投资进入相应的资金序列，与原有序列按用户key合并
  for(k in 0:2){
    name<-switch(k+1,
                 "current",
                 "tPlus",
                 "regular")
    tmp<-invest_seq[type==k,c("userid","create_time","amount","remark")]%>%{
      setnames(.,c("create_time","amount"),c("log_time","unmatched_premium"))
      switch(k+1,
             rbind(current_now,.),
             rbind(tPlus_now,.),
             rbind(regular_now,.)
      )
    }
    if(nrow(tmp)>0){
      tmp<-tmp[,.(unmatched_premium=sum(unmatched_premium),log_time=min(log_time)),by=userid]%>%
        .[,remark:=NA]
    }
    eval(parse(text = paste0(name,"_now<-tmp")))
  }
  #输出成功！
  print(paste(index,"[",start,"-",end,")",":invest OK!"))
    
  # 1.3 赎回处理（超级复杂！）
  #--产出：对应类型的未匹配资金减少，产生赎回资产，产生额外解绑投资资金
  
  # 1.3.1 历史残留赎回记录的重新赎回确认
  #--产出：更新赎回资产中的待匹配资产信息，更新资金序列
  #--之前时间片的赎回资产问题再一次在资金队列里进行遍历
  if(nrow(redeem_asset)>0){
  	redeem_asset<-setorder(redeem_asset,redeemID,unmatched_amount)
  	for(m in 1:nrow(redeem_asset)){
  	  redeem_asset_focus<-redeem_asset[m,]
  	  type<-redeem_asset_focus$type
        cash_flow<-switch(type+1,
                          current_now,
                          tPlus_now)
  	  focus<-cash_flow[userid==redeem_asset_focus$userid,]
  	  if(nrow(focus)==0){
          next
        }else{
          rest_redeem_amount<-ifelse(round(focus$unmatched_premium-redeem_asset_focus$unmatched_amount,2)>=0,
                                     0,
                                     round(redeem_asset_focus$unmatched_amount-focus$unmatched_premium,2))
  		#--改变资金队列的相应记录
          cash_flow[userid==redeem_asset_focus$userid,
                    unmatched_premium:=round(unmatched_premium-(redeem_asset_focus$unmatched_amount-rest_redeem_amount),2)]
  		#--改变赎回资产信息的记录
  		redeem_asset[redeem_asset_focus,on=c("id"="id","redeemID"="redeemID"),unmatched_amount:=rest_redeem_amount]
  		
  		redeem_log[redeem_asset_focus,on=c("redeemID"="id"),":="(free_redeem_amount=free_redeem_amount+(i.unmatched_amount-rest_redeem_amount),
  																 matched_redeem_amount=matched_redeem_amount-(i.unmatched_amount-rest_redeem_amount))]
  		
        }  
      }
  	
  	redeem_status<-redeem_asset[,.(remain_redeem=sum(unmatched_amount)),by=redeemID]
  	redeem_record[redeem_status,on=c("id"="redeemID"),":="(status=ifelse(i.remain_redeem==0,2,1),update_time=start)]
  	redeem_asset<-redeem_asset[unmatched_amount>0,]
  }  
  
  #
  #--额外解绑投资资金初始化
  extra_invest<-data.table()
  if(nrow(redeem_seq)>0){
    #--从赎回队列开始遍历
    for(j in 1:nrow(redeem_seq)){
      redeem_focus<-redeem_seq[j,]
      
	  #确认赎回的类型、对应的资金队列
      type<-redeem_focus$type
      name<-switch(type+1,
                   "current","tPlus")
      cash_flow<-switch(type+1,
                        current_now,
                        tPlus_now)
      
      #查找该用户相应资金队列里的剩余金额（未匹配金额，因为前面都是按照用户key聚合，所以最多只会有一条记录）
      focus<-cash_flow[userid==redeem_focus$userid,]
      
	  #检验这部分资金是否足够赎回（rest_redeem_amount==0）
      if(nrow(focus)==0){
        rest_redeem_amount<-redeem_focus$amount
      }else{
        rest_redeem_amount<-ifelse(round(focus$unmatched_premium-redeem_focus$amount,2)>=0,
                                   0,
                                   round(redeem_focus$amount-focus$unmatched_premium,2))
      }
      
      #如果对应资金队列的金额足够，则只改变资金队列里的相应记录，同时改变赎回记录中的相应记录的status
      if(rest_redeem_amount==0){
		#--改变资金队列的相应记录
        cash_flow[userid==redeem_focus$userid,
                  unmatched_premium:=round(unmatched_premium-redeem_focus$amount,2)]
        #--改变赎回记录中的相应记录的状态，status=2，赎回成功时间标记为赎回发起时间
        redeem_record[id==redeem_focus$id,":="(status=2,update_time=create_time)]
        #--记录该笔赎回的资金信息
        redeem_log_temp<-data.table(redeemID=redeem_focus$id,
                                    free_redeem_amount=redeem_focus$amount,
                                    matched_redeem_amount=rest_redeem_amount)
      }else{#否则如果资金不够，则需从资产表中（已匹配资产）赎回
		    #--该用户相应资金队列的金额置0
        cash_flow[userid==redeem_focus$userid,unmatched_premium:=0]
        #--记录该笔赎回的资金信息
		    redeem_log_temp<-data.table(redeemID=redeem_focus$id,
                                    free_redeem_amount=round(redeem_focus$amount-rest_redeem_amount,2),
                                    matched_redeem_amount=rest_redeem_amount)
        
        #确定剩余赎回所对应的资产表
		    #--选定可赎回的资产范围及其资产信息
        asset_focus<-match_status_now[userid==redeem_focus$userid&type==name,
                                      c("userid","type","amount","asset_id")]%>%
          asset_now[,c("id","unmatched_amount","avail_num","deadline")][.,on=c("id"="asset_id"),nomatch=0,mult="all"]
        #--调用函数fun.redeem_select_asset函数决定赎回的资产及其对应的相应信息
        redeem_output<-fun.redeem_select_asset(rest_redeem_amount,asset_focus)
        #----输出赎回的资产信息
        redeem_asset_temp<-redeem_output$extra_asset%>%.[,":="(redeemID=redeem_focus$id,
                                                               userid=redeem_focus$userid,
                                                               type=redeem_focus$type,
                                                               create_time=redeem_focus$create_time)]
        #----累加到赎回资产信息表
        redeem_asset<-rbind(redeem_asset,redeem_asset_temp)
        #--如果赎回涉及了额外解绑资金，则需要输出相应信息
        if(redeem_output$extra_label){
		  #----输出涉及的额外解绑资金
          extra_invest_temp<-redeem_output$extra_invest%>%.[,":="(userid=redeem_focus$userid,
                                                                  create_time=redeem_focus$create_time,
                                                                  amount=amount,
                                                                  type=type,
                                                                  remark="extra")]
          #----累加到额外解绑资金序列
          extra_invest<-rbind(extra_invest,extra_invest_temp)
        }
      }
      #累加赎回资金信息表
      redeem_log<-rbind(redeem_log,redeem_log_temp)
	  #更新资金队列
      if(type==1){
        tPlus_now<-cash_flow
      }else{
        current_now<-cash_flow
      }
    }
  }
  #输出成功！
  print(paste(index,"[",start,"-",end,")",":redeem OK!"))
  
  #用额外解绑资金再次更新资金队列，更新方法与之前一样，仍以用户为key
  if(nrow(extra_invest)>0){
    for(k in 0:1){
      name<-switch(k+1,
                   "current",
                   "tPlus",
                   "regular")
      tmp<-extra_invest[type==k,c("userid","create_time","amount","remark")]%>%{
        setnames(.,c("create_time","amount"),c("log_time","unmatched_premium"))
        switch(k+1,
               rbind(current_now,.),
               rbind(tPlus_now,.),
               rbind(regular_now,.)
        )
      }%>%.[,.(unmatched_premium=sum(unmatched_premium),
               log_time=min(log_time)),
            by=.(userid)]%>%
			.[,remark:=NA]
      eval(parse(text = paste0(name,"_now<-tmp")))
    }
	#输出成功
	print(paste(index,"[",start,"-",end,")",":extra_invest OK!"))
  }
  
  # 1.4 赎回资产处理（redeem_asset+match_status_now=>asset_now）
  #--产出：更新资产状态表，以便产生待匹配资产队列
  #--虚拟化处理，如果赎回立即成功，则资产情况是什么样的
  if(nrow(redeem_asset)>0){
    #----虚拟在匹配记录中作出变化
    match_record_dummy<-copy(match_status_now)%>%
      .[redeem_asset[,type_name:=sapply(FUN = fun.type_name,type)],
        on=c("userid"="userid","type"="type_name","asset_id"="id"),
        ":="(amount=round(amount-unmatched_amount,2))]%>%
      .[amount>0,]
    #----虚拟变化后资产情况如何
    asset_redeem_dummy<-match_record_dummy[,.(matched_amount=sum(amount),num=uniqueN(userid)),by=asset_id][
      redeem_asset[,.(redeem_time=min(create_time)),by=id],
      on=c("asset_id"="id"),redeem_time:=i.redeem_time]
    #----将这种虚拟影响传递给当前资产状态表，以产出待匹配资产队列
    asset_now<-asset_now[asset_redeem_dummy,
                         on=c("id"="asset_id"),
                         ":="(isredeemed=ifelse(is.na(i.redeem_time),0,1),
                              redeem_time=i.redeem_time,
                              unmatched_amount=(amount-ifelse(is.na(i.matched_amount),0,i.matched_amount))%>%round(.,2),
                              avail_num=n-ifelse(is.na(i.num),0,i.num)
                         )]%>%
      .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]
  }
  # 1.5 入库资产处理
  #--产出：资产队列增加入库资产
  if(nrow(in_asset_seq)>0){
    asset_now<-rbind(asset_now,in_asset_seq)
  }
  #输出成功！
  print(paste(index,"[",start,"-",end,")",":asset OK!"))


  ############### 2.资产匹配 ##################
  # 2.1 资金队列
  user_list<-{
    var=c("userid","type","unmatched_premium")
    a<-copy(regular_now)[,type:="regular"]%>%.[.SD,.SDcols=var]
    b<-copy(tPlus_now)[,type:="tPlus"]%>%.[,.SD,.SDcols=var]
    d<-copy(current_now)[,type:="current"]%>%.[,.SD,.SDcols=var]
    e<-match_status_now[,c("userid","type","amount")]%T>%setnames(.,"amount","unmatched_premium")
    f<-match_status_now[type=="current",c("userid","amount")]%>%
      .[,.(matched_amount=sum(amount)),by="userid"]
    xx<-rbindlist(list(a,b,d,e))%>%
      .[,.(unmatched_premium=sum(unmatched_premium)),by=.(userid,type)]%>%
      dcast(.,userid~type,value.var="unmatched_premium",fill=0)%>%
      .[,premium:=rowSums(.SD,dim=1),.SDcols=names(.)%>%.[which(.%in%c("regular","tPlus","current"))]]%>%
      .[,all_match_premium:=rowSums(.SD,dim=1),.SDcols=names(.)%>%.[which(.%in%c("regular","tPlus"))]]%>%
      .[,ratio:=(all_match_premium/premium)%>%round(.,4)]%>%
      .[,avail_current_total:=(ifelse(ratio>=target_ratio,0,target_ratio-ratio)*premium/10)%>%floor(.)*10]%>%
      .[f,on="userid",matched_current:=i.matched_amount]%>%
      .[,matched_current:=replace(matched_current,is.na(matched_current),0)]%>%
      .[,avail_current:=avail_current_total-matched_current]%>%
      .[,c("userid","avail_current")]
    xx
  }
  regular_seq<-copy(regular_now)
  tPlus_seq<-copy(tPlus_now)
  
  current_seq<-copy(current_now)%T>%
    setorder(.,userid,remark,na.last=T)%>%
    .[,cum_amount:=cumsum(unmatched_premium),by=userid]%>%
    .[user_list,on=c("userid"="userid"),test:=i.avail_current-cum_amount]%>%
    .[,unmatched_premium_new:=ifelse(test>=0,unmatched_premium,unmatched_premium+test)]%>%
    .[,unmatched_premium_new:=ifelse(unmatched_premium_new>=0,unmatched_premium_new,0)]%>%
    .[,current_minus:=(unmatched_premium-unmatched_premium_new)%>%round(.,2)]
  
  current_minus<-current_seq[current_minus>0,c("userid","current_minus")]%>%
    .[,.(current_minus=sum(current_minus)),by=userid]%T>%
    setnames(.,"current_minus","unmatched_premium")%>%
    .[,log_time:=end]%>%
    .[,remark:=NA]
  
  current_seq<-current_seq[,c("userid","unmatched_premium_new","log_time","remark")]%T>%
    setnames(.,"unmatched_premium_new","unmatched_premium")
  
  # 2.2 资产匹配情况，待循环更新写入
  match_record<-copy(match_status_now)
  
  # 2.3 当前赎回状态（涉及单赎回ID对应多笔资产）
  redeem_status<-redeem_asset[,.(remain_redeem=sum(unmatched_amount)),by=redeemID] 
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
    
    # 2.5 匹配参数初始值
    r_regular=ceiling((nrow(regular_seq)/sum(asset_list[unmatched_amount>0,]$avail_num))/0.05)*0.05
    r_tPlus=1
    
	#输出匹配开始！
    print(paste(index,"[",start,"-",end,")",":match loop start! total_num is",nrow(asset_list)))
    
    # 2.6 资产匹配主循环
    for(i in 1:nrow(asset_list)){
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
        .[,asset_id:=wait_asset$id]%>%
        .[,id:=id+nrow(match_record)]%>%
        .[,log_time:=end]
		
      #如果该资产属于赎回资产（与赎回相关），则更新赎回记录中的状态，并更新资产匹配表的状态
      if(wait_asset$isredeemed){
		#更新赎回记录，相应赎回ID的赎回总额度应减去该资产下属于该赎回ID的金额
		redeem_status[redeem_asset[id==wait_asset$id,],on=c("redeemID"="redeemID"),remain_redeem:=remain_redeem-i.unmatched_amount]		
        
		#redeem_asset上该资产对应用户相应产品的额度在match_record上进行更新
        redeem_result<-redeem_asset[id==wait_asset$id,c("id","redeemID","unmatched_amount")][
          redeem_record[,c("id","type","userid")],on=c("redeemID"="id"),nomatch=0,mult="all"]%>%
          .[,type_name:=sapply(FUN = fun.type_name,type)]
        
        match_record<-match_record[redeem_result,
                                   on=c("asset_id"="id","userid"="userid","type"="type_name"),
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
      
      match_record<-rbind(match_record,temp)
      
      #更新资产序列
      wait_asset[,":="(unmatched_amount=round(unmatched_amount-sum(temp$amount),2),
                       avail_num=n-uniqueN(match_record[asset_id==wait_asset$id,]$userid))]%>%
        .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]
      
      asset_list[id==wait_asset$id,
                 ":="(unmatched_amount=round(wait_asset$unmatched_amount,2),
                      avail_num=wait_asset$avail_num,
                      avg_avail_amount=wait_asset$avg_avail_amount,
                      issettled=wait_asset$issettled,
                      settled_time=wait_asset$settled_time)]
      
      #更新资金序列
      if(nrow(temp[type=="regular"])>0){
        regular_seq[temp[type=="regular",],
                    on=c("userid"="userid"),
                    unmatched_premium:=round(unmatched_premium-i.amount,2)]
      }
      if(nrow(temp[type=="tPlus"])>0){
        tPlus_seq[temp[type=="tPlus",],
                  on=c("userid"="userid"),
                  unmatched_premium:=round(unmatched_premium-i.amount,2)]
      }
      if(nrow(temp[type=="current"])>0){
        current_seq[temp[type=="current",],
                    on=c("userid"="userid"),
                    unmatched_premium:=round(unmatched_premium-i.amount,2)]
      }
    }##匹配结束
	
	#更新赎回记录，如果该笔赎回在匹配过程中，赎回资产都得到了匹配（remain_redeem==0），则该赎回可以赎回（2），否则不可赎回（1）
	redeem_record[redeem_status,on=c("id"="redeemID"),":="(status=ifelse(i.remain_redeem==0,2,1),update_time=end)]
    
	#更新当前资金序列状态，以传递给下个时间片
	regular_now<-regular_seq%>%.[,weight:=NULL]
  tPlus_now<-tPlus_seq%>%.[,weight:=NULL]
  current_now<-{
    current_seq[,weight:=NULL]
    rbind(current_seq,current_minus)
    }%>%
    .[,.(unmatched_premium=sum(unmatched_premium),log_time=min(log_time)),by=.(userid,remark)]
    
	#更新资产匹配状态，以传递给下个时间片
    match_status_now<-match_record[amount>0,]%>%
      .[,.(amount=round(sum(amount),2),log_time=max(log_time),id=max(id)),by=.(userid,type,asset_id)]
    #更新赎回资产状态，以传递给下个时间片
	redeem_asset<-redeem_asset[,type_name:=NULL]
    #更新资产状态，以传递给下个时间片
    asset_now<-asset_now[asset_list[,c("id","unmatched_amount","avail_num","avg_avail_amount","issettled","settled_time")],
                         on=c("id"="id"),
                         ":="(unmatched_amount=i.unmatched_amount,
                              avail_num=i.avail_num,
                              avg_avail_amount=i.avg_avail_amount,
                              issettled=i.issettled,
                              settled_time=i.settled_time)]%>%
      .[unmatched_amount==0,":="(redeem_time=NA,
                                 isredeemed=0)]
  }
  
  
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
  
  return(structure(list(regular_now=regular_now,
                        tPlus_now=tPlus_now,
                        current_now=current_now,
                        match_status_now=match_status_now,
                        asset_now=asset_now,
                        asset_status_summary=asset_status_summary,
                        redeem_record=redeem_record,
                        redeem_log=redeem_log,
                        redeem_asset=redeem_asset,
						redeem_asset_snap=redeem_asset_snap,
						redeem_status=redeem_status),
                   class="match time flow eval"))
  
}

