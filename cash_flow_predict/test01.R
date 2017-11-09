library(data.table)
library(timeSeries)
library(forecast)
source('~/rstudio/(fun)mysqlconn.R')
conn<-mysqlconn("mysql_settings.csv")
z0<-"select date,sum(amount) as amount from invest_record group by date"
investdata<-sqlQuery(conn,z0)
# STL by week
ts1<-ts(investdata$amount,frequency = 7)
stl1<-stl(ts1,s.window = "period")
stl_df<-as.data.table(stl1$time.series)
stl_df[,total:=seasonal+trend+remainder]
all.equal(stl_df$total,investdata$amount)#TRUE
remainder1<-stl1$time.series[,"remainder"]
plot(stl1)

#STL by month
ts2<-ts(investdata$amount,frequency = 30)
stl2<-stl(ts2,s.window = "period")
plot(stl2)

fit1<-hw(investdata$amount,seasonal = "multiplicative")
