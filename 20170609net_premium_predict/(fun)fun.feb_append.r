fun.feb_append<-function(add_num,start_id,end_id,start_value,end_value,dayOfMonth){
  data.table(id=seq(from=start_id,to=end_id,length.out = add_num+2),
             dayOfMonth=seq(from=dayOfMonth,length.out = add_num+2,by=1),
             value=seq(from=start_value,to=end_value,length.out = add_num+2))%>%
    .[id>start_id & id<end_id,]
}