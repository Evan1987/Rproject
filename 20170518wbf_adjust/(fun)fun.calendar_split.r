fun.calendar_split<-function(add_ord_final,start_day,calendar_limit){
  library(lubridate)
  
  add_ord_summary<-add_ord_final[,.(amount=sum(amount),num=.N),by=.(user_id,trade_type)]%>%
    dcast(.,user_id~trade_type,value.var=c("amount","num"))%>%
    {
      vars =names(.)%>%.[which(str_detect(.,"invest")|str_detect(.,"redeem"))]
      .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
    }
  
  total_amount<-sum(add_ord_summary$amount_invest)+sum(add_ord_summary$amount_redeem)
  target_day = start_day
  split_result<-data.table()
  while(total_amount>0){
    index = format(target_day,"%w")%>%as.numeric(.)
    target_limit<-calendar_limit[id==index,]
    exp_invest_amount = target_limit$invest_limit
    exp_redeem_amount = target_limit$redeem_limit
    
    ## 无压力选择投资
    target_invest_data<-add_ord_final[trade_type=="invest",]
    
    invest_select<-{
      model<-list(obj=target_invest_data$amount,
                  A=matrix(target_invest_data$amount,nrow = 1),
                  sense=c("<="),
                  rhs=c(exp_invest_amount),
                  vtype="B",
                  modelsense="max")
      result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
      if(result$status=="OPTIMAL"){
        x<-result$x
        tmp<-target_invest_data[which(x==1),]
      }else{
        tmp<-target_invest_data[0]
      }
      tmp
    }
    # 投资结果汇总
    invest_select_summary<-invest_select[,.(amount=sum(amount),num=.N),by=user_id]
    add_ord_summary[invest_select_summary,
                    on="user_id",
                    ":="(amount_invest=round(amount_invest-i.amount,2),
                         num_invest=num_invest-i.num)]
    
    # 根据剩余投资情况，决定每个用户的赎回订单
    target_redeem_data<-{
      redeem_avail_list<-add_ord_summary[amount_redeem>amount_invest,]%>%
        .[,avail_amount:=round(amount_redeem-amount_invest,2)]%>%
        .[,c("user_id","avail_amount")]
      
      add_ord_final[trade_type=="redeem",][redeem_avail_list,on="user_id"]%T>%
        setorder(.,user_id,amount)%>%
        .[,cum_amount:=cumsum(amount),by=user_id]%>%
        .[cum_amount<=avail_amount,]%>%
        .[,-c("avail_amount","cum_amount")]
    }
    
    # 选择赎回订单
    redeem_select<-{
      model<-list(obj=target_redeem_data$amount,
                  A=matrix(target_redeem_data$amount,nrow = 1),
                  sense=c("<="),
                  rhs=c(exp_redeem_amount),
                  vtype="B",
                  modelsense="max")
      result<-gurobi(model = model,params = list(OutputFlag=0,timeLimit = 10))
      if(result$status=="OPTIMAL"){
        x<-result$x
        tmp<-target_redeem_data[which(x==1),]
      }else{
        tmp<-target_redeem_data[0]
      }
      tmp
    }
    
    # 赎回结果汇总
    redeem_select_summary<-redeem_select[,.(amount=sum(amount),num=.N),by=user_id]
    add_ord_summary[redeem_select_summary,
                    on="user_id",
                    ":="(amount_invest=round(amount_invest-i.amount,2),
                         num_invest=num_invest-i.num)]
    
    result_temp<-rbind(invest_select,redeem_select)%>%.[,dt:=target_day]
    split_result<-rbind(split_result,result_temp)
    add_ord_final<-add_ord_final[!ord_no%in%c(invest_select$ord_no,redeem_select$ord_no),]
    total_amount<-sum(add_ord_summary$amount_invest)+sum(add_ord_summary$amount_redeem)
    
    target_day = target_day+1
  }
  
  return(split_result)
}