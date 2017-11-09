fun.wise_slim<-function(user_list,
                        by,
                        target_amount = list(amount=5e+7,tol = 5e+6),
                        target_asset =list(amount=9e+6,num=200)){
  library(data.table)
  library(magrittr)
  source('~/rstudio/match150/calculate/(fun)fun.wise_slim_by_amount.r', encoding = 'UTF-8',echo=TRUE)
  source('~/rstudio/match150/calculate/(fun)fun.wise_slim_by_asset.r', encoding = 'UTF-8', echo=TRUE)
  
  if(missing(by)){
    if(!missing(target_amount)){
      by = "amount"
    }else if(!missing(target_asset)){
      by = "asset"
    }
  }
  asset_conn<-MySQL_conn_select(connect_name = "asset_product")
  cash_conn<-MySQL_conn_select(connect_name = "cash_product")
  
  
  # ********************************* 数据收集 *****************************************
  # 当前用户存量信息
  z0 <- "SELECT 	user_id as userid,
  (un_match_amount+match_amount+asset_out_amount) as premium
  FROM user_account_info
  WHERE (match_amount + un_match_amount + asset_out_amount) > 0"
  res <- dbSendQuery(cash_conn, z4)
  user_premium_data <- dbFetch(res, n = -1) %>% as.data.table(.)
  dbClearResult(res)
  dbDisconnect(cash_conn)
  
  if(by=="amount"){
    # 当前未匹配用户资金
    z1<-"select user_id as userid,unmatched_amount
    from ast_money_account where yn=0 and unmatched_amount>0 and account_type=1"
    res<-dbSendQuery(asset_conn,z1)
    user_match_data<-dbFetch(res,n=-1)%>%as.data.table(.)
    # names in user_match_data :userid,unmatched_amount_regular,unmatched_amount_tPlus,unmatched_amount_current
    dbClearResult(res)
    dbDisconnect(asset_conn)
    
    user_list<-user_match_data[user_premium_data,on="userid",premium:=i.premium]%>%
      .[,exp_match_amount:=premium-unmatched_amount]
  }
  
  if(by=="asset"){
    # 当前一、二队列长度
    z0 <- "select	count(if(account_type=3,true,null)) as first_seq_length,
		count(if(account_type=2,true,null)) as second_seq_length
    from ast_money_account where yn=0 and unmatched_amount>0"
    res<-dbSendQuery(asset_conn,z0)
    seq_length<-dbFetch(res,n=-1)
    first_seq_length = seq_length$first_seq_length
    second_seq_length = seq_length$second_seq_length
    
    # 需求的一、二队列样本
    z1 <- "select	unmatched_amount
    from ast_money_account where yn = 0 and unmatched_amount>0 and 
    account_type=3 order by unmatched_amount desc limit 200"
    res<-dbSendQuery(asset_conn,z1)
    first_seq<-dbFetch(res,n=-1)%>%.$unmatched_amount%>%sort(.,decreasing=T)
    
    z2 <- "select	unmatched_amount
    from ast_money_account where yn = 0 and unmatched_amount>0 and 
    account_type=2 order by unmatched_amount desc limit 200"
    res<-dbSendQuery(asset_conn,z1)
    second_seq<-dbFetch(res,n=-1)%>%.$unmatched_amount%>%sort(.,decreasing=T)
    
    # 当前待匹资产的总可匹配人数
    z3<-"SELECT sum(avail_num) as num from ast_matching_asset_group WHERE unmatched_amount > 0 AND yn = 0"
    res<-dbSendQuery(asset_conn,z2)
    remainTotalNum<-dbFetch(res,n=-1)%>%.$num
    dbClearResult(res)
    
    # 当前第三队列样本
    z4 <- paste("select	a.*
                from
                (
                select	user_id as userid,
                unmatched_amount,
                target_amount
                from ast_money_account where yn = 0 and account_type=1 and unmatched_amount>0
                )a
                left join
                (
                select	user_id as userid,
                from ast_money_account where yn = 0 and account_type=1 order by target_amount desc limit 200
                )b on a.userid=b.userid
                where b.userid is not null or a.unmatched_amount>=",ceiling(target_asset$amount/target_asset$num)
                )
    res<-dbSendQuery(asset_conn,z3)
    user_match_data<-dbFetch(res,n=-1)%>%as.data.table(.)
    dbClearResult(res)
    dbDisconnect(asset_conn)
    user_list<-user_match_data[user_premium_data,on="userid",premium:=i.premium]%>%
      .[,exp_match_amount:=premium-unmatched_amount]
  }
  
  
  
  
  result<-switch(by,
                 "amount" = fun.wise_slim_by_amount(user_list = user_list,
                                                    tol = target_amount$tol,
                                                    target_cash_amount = target_amount$amount),
                 "asset" = fun.wise_slim_by_asset(user_list = user_list,
                                                  remainTotalNum = remainTotalNum,
                                                  first_seq = first_seq,
                                                  second_seq = second_seq,
                                                  target_asset = target_asset
                                                  ))
  return(result)
}