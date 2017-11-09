library(data.table)
library(magrittr)
library(stringr)
library(RMySQL)
library()
my_conn<-dbConnect(MySQL(),
                   host = "127.0.0.1",
                   port = 3306,
                   user="root",
                   password='871226',
                   dbname="yinker")


z0<-"select date,hour,(invest_amount-redeem_amount) as net_change from jlc_sum_by_hour where date>='2017-04-01'"
res<-dbSendQuery(my_conn,z0)
mydata<-dbFetch(res,n=-1)%>%as.data.table(.)%>%
  .[,date:=as.Date(date)]%>%
  .[,weekday:=format(date,"%w")%>%as.numeric(.)]
dbClearResult(res) 
dbDisconnect(my_conn)

path = "F:/Project/20170315资产匹配穿透150人调研/20170609资金量流入流出统计调研/"
mydata_summary<-mydata[,.(net_change=mean(net_change,na.rm = T)),by=.(weekday,hour)]%T>%
  setorder(.,weekday,hour)%>%
  .[,cum_net_change:=cumsum(net_change),by=weekday]

for(i in 0:6){
  x<-switch(i+1,
            "Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  target_data<-mydata_summary[weekday==i,]
  png(filename = paste0(path,x,".png"),width = 800,height = 600)
  plot(target_data$hour,
       target_data$cum_net_change,
       type = "l",xlab = "hour",ylab = "net_change",ylim = c(-5e+7,3e+7),
       main = paste(x,"cash net flow by hour"),col="red")
  lines(x=target_data$hour,y=target_data$net_change)
  dev.off()
}
