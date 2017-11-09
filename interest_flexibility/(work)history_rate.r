library(data.table)
library(magrittr)
library(sqldf)
library(dplyr)
path<-"F:/Project/20170406用户数量和存量额度的利率弹性/"
timeSpan<-seq.Date(as.Date("2016-05-01"),as.Date("2017-04-05"),by="day")%>%
  data.frame(log_date=.)

jlc_history_rate<-fread(paste0(path,"jlc_history_rate.csv"))%>%
  tbl_df(.)%>%
  mutate(.,log_date=as.Date(log_date))

yuebao_history_rate<-fread(paste0(path,"yuebao_history_rate.csv"))%>%
  tbl_df(.)%>%
  mutate(.,log_date=as.Date(log_date))%>%
  rename(.,rate=yuebao_rate)

zhenrongbao_history_rate<-fread(paste0(path,"zhenrongbao_history_rate.csv"))%>%
  tbl_df(.)%>%
  mutate(.,log_date=as.Date(log_date))%>%
  select(.,log_date,rate=cur_rate)

z0<-"select	a.log_date as log_date,
		b.rate as jlc_rate,
		c.rate as yuebao_rate,
		d.rate as zhenrongbao_rate
from timeSpan a
left join jlc_history_rate b on a.log_date=b.log_date
left join yuebao_history_rate c on a.log_date=c.log_date
left join zhenrongbao_history_rate d on a.log_date=d.log_date"

history_rate<-sqldf(z0)%>%tbl_df(.)
fwrite(history_rate,paste0(path,"history_rate.csv"))
