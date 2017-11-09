library(data.table)
library(magrittr)
library(readr)
# 不爽度衡量函数，与挖掘过程保持一直
fun.tol<-function(general_rate,jlc_rate,yuebao_rate,zhenrongbao_rate,premium_cur){
  x=general_rate-jlc_rate
  y=jlc_rate-yuebao_rate
  z=jlc_rate-zhenrongbao_rate
  result1=(exp(-x)-1)*10
  result2=(exp(-(y-3))-1)*20
  result3=(exp(-z*2)-1)*15
  
  return(max(0,result1)+max(0,result2)+max(0,result3)+ifelse(premium_cur>=100,0,0.01))
}

# 给定不爽度，以及坚持天数阈值，可以给出受影响的用户存量及数量
fun.select_people<-function(tol,hold_days_threhold=30,total_data=result_data){
  #总体用户数量和存量
  origin_summary<-total_data[,.(total_num=.N,premium=sum(max))]
  
  #坚持时间低于坚持天数阈值的人群，被认为是受影响人群
  tmp<-copy(total_data)%>%as.data.table(.)%>%
    .[,hold_days_estimate:=ceiling(interface/(tol+0.01))]%>%
    .[hold_days_estimate<hold_days_threhold,]%>%
    .[,c("userid","max","min")]
  #估计受影响人群的总人数和总存量（下降的存量）
  result<-tmp[,.(num_ratio=.N/origin_summary$total_num,
                 premium_change_ratio=(sum(max)-sum(min))/origin_summary$premium)]%>%
    .[,":="(num_ratio=round(num_ratio,2),premium_change_ratio=round(premium_change_ratio,2))]
}

path<-"F:/Project/20170406用户数量和存量额度的利率弹性/"
result_data<-fread(paste0(path,"result/result_true.csv"))

# 创建不同的jlc利率并认为用户实际利率与jlc利率相等，得出不同利率下的影响用户范围和存量范围
simu_data<-data.table(jlc_rate=seq(5,6.5,0.1))%>%
  .[,id:=.I]%>%.[,c("id","jlc_rate")]%>%
  .[,tol:=fun.tol(jlc_rate,jlc_rate,yuebao_rate = 3.5,zhenrongbao_rate = 5.5,premium_cur=100)%>%round(.,2),by=id]%>%
  .[,fun.select_people(tol),by=.(id,jlc_rate,tol)]
