redeem_cycle<-function(i_invest,i_redeem,jstep=dim(i_redeem)[1]){
  library(data.table)
  library(magrittr)
  library(sqldf)
  
  #### Create action shedule for the special user. from invest1st_day to the check_date("2016-12-31")
  i_timeline<-seq.Date(from = min(as.Date(i_invest$date)),to=as.Date("2016-12-31"),by="day")%>%
    data.table(log_day=.)
 
  i_z<-"select	a.log_day,
		b.invest_amount,
  c.redeem_amount
  from i_timeline a
  left join i_invest b on a.log_day=b.date
  left join i_redeem c on a.log_day=c.date"
  
  # the i_log(schedule output), delete the date with no action and transfer the "NA" value to zero for the convenience of following calc.
  i_log<-as.data.table((sqldf(i_z)))%>%
    .[!(is.na(invest_amount)&is.na(redeem_amount)),]%>%
    as.data.frame(.)
  i_log[is.na(i_log)]<-0
  i_log<-as.data.table(i_log)
  
  # "result_log" is the ultimate function output, it is started from the raw i_log.
  result_log<-copy(i_log)
  
  for(j in 1:jstep){
  
    j_redeem_day<-i_redeem$date[j]
  
  # temp1 is the ones may be related to this redeem (log_day<=redeem_day), "id" is for convenience to find the redeem influence row boundary.
    temp1<-i_log[log_day<=j_redeem_day,]%>%
      .[,":="(invest_sum=cumsum(invest_amount),
              day_span=as.numeric(difftime(j_redeem_day,log_day,units = "day")),
              id=seq_len(dim(.)[1]))]

  # temp2 has no relationship with this redeem (log_day>redeem_day).
    temp2<-i_log[log_day>j_redeem_day,]
  # in case of the redeem_amount>total invest_amount(due to the interest income), do this treatment.  
    j_redeem_amount<-min(i_redeem$redeem_amount[j],max(temp1$invest_sum))
  
  # Get the boundry between redeemed and unredeemed invest,temp1_1 is totally redeemed(invest_sum<=j_redeem_amount).
    temp1_1<-temp1[invest_sum<=j_redeem_amount,]%>%
      .[,tt:=invest_amount*day_span]
  # The cycle_day contribution from temp1_1  
	redeem_cycle_days_1<-ifelse(nrow(temp1_1)==0,0,sum(temp1_1$tt))
  # The total redeem source contributed from temp1_1 part
	total_redeem<-ifelse(nrow(temp1_1)==0,0,max(temp1_1$invest_sum))

  # the invest has all been redeemed (invest_sum==j_redeem_amount | nrow(temp1_1)==nrow(temp1)).temp1_2 is null.
    if(nrow(temp1_1)==nrow(temp1)){
      redeem_cycle_days_2<-0
      temp1_2<-data.table()
    }
  # the invest(id==id_boundary) is partly redeemed. The invest_amount of temp1_2 has to change for the next loop. 
    else{
      id_boundary<-nrow(temp1_1)+1
      redeem_cycle_days_2<-temp1[id==id_boundary,]$day_span*(j_redeem_amount-total_redeem)
      temp1_2<-temp1[id>=id_boundary,]%>%
        .[id==id_boundary,invest_amount:=invest_sum-j_redeem_amount]%>%
        .[,1:3]
    }
              
    # Get the Cycle days. temp1_2 and temp2 is the initial condition for the next redeem calc loop.
    redeem_cycle_days<-(redeem_cycle_days_1+redeem_cycle_days_2)/ifelse(j_redeem_amount==0,0.01,j_redeem_amount)
    
    result_log<-result_log[log_day==j_redeem_day,cycle_days:=redeem_cycle_days]
    i_log<-rbind(temp1_2,temp2)
    
  }
  return(result_log)
}