
# 根据现有投资序列，分解虚拟出三种产品形式（定期、T+N、活期）的投资序列
# type:0-活期，1-T+N，2-定期
invest_simu<-function(rawdata,P_regular=0.15,P_tPlus=0.6,unit_amount=c(1000,1000)){
  library(data.table)
  library(magrittr)
  library(lubridate)
  invest_rawdata<-copy(rawdata)%>%
    .[,create_time:=as.POSIXct(create_time)]%>%
    .[,type:=0]%>%
    .[amount>=1000,type:=rbinom(n=nrow(.[amount>=1000,]),size = 1,prob = P_regular)%>%
        ifelse(.,2,0)]%>%
    .[amount>=1000&type==0,type:=rbinom(n=nrow(.[amount>=1000&type==0,]),size=1,prob=P_tPlus/(1-P_regular))%>%
        ifelse(.,1,0)]%>%
    .[type%in%c(1,2),amount:=round(amount/1000)*1000]%>%
    .[,amount:=round(amount,2)]
  
}


  










  

