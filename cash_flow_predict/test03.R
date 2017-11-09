library(data.table)
library(magrittr)
library(sqldf)
library(plyr)
library(tcltk)
invest_by_type<-fread("F:\\Project\\20170220投资额度预测\\invest_by_type.csv")%>%
  .[,date:=as.Date(date)]%T>%
  setkeyv(.,c("date","type"))

calendar<-fread("F:\\Project\\20170220投资额度预测\\calendar.csv")%>%
  .[,":="(date=as.Date(date),id=as.numeric(row.names(.)))]%T>%
  setkey(.,date)

holidays<-calendar[isHoliday==1,]
z1<-"select 	a.feast as feast,
a.id as start_id,
min(b.id) as end_id
from holidays a
join holidays b on b.id>=a.id
left join holidays c on a.id-1=c.id
left join holidays d on b.id+1=d.id
where c.id is null and d.id is null
group by a.feast,a.id"

holidays_summary<-as.data.table(sqldf(z1))
holidays_summary<-arrange(holidays_summary,feast,start_id)
rows<-nrow(holidays_summary)

#节后标签
for(i in 1:rows){
  end_id<-holidays_summary$end_id[i]
  feast<-holidays_summary$feast[i]
  calendar[id<=end_id+ceiling(feast/2) & id>end_id,Back_Label:=paste0(feast,"b",id-end_id)]
}
#节前标签
for(i in 1:rows){
  start_id<-holidays_summary$start_id[i]
  feast<-holidays_summary$feast[i]
  calendar[id>=start_id-feast & id<start_id,Before_Label:=paste0(feast,"f",start_id-id)]
}
#节中标签
for(i in 1:rows){
  start_id<-holidays_summary$start_id[i]
  end_id<-holidays_summary$end_id[i]
  feast<-holidays_summary$feast[i]
  calendar[id>=start_id & id<=end_id,In_Label:=paste0(feast,"i",id-start_id+1)]
}

write.csv(calendar,"F:\\Project\\20170220投资额度预测\\calendar.csv",row.names = F)

types<-data.table(type=unique(invest_by_type$type))%T>%
  setkey(.,type)

dateline<-rep(seq.Date(from=as.Date("2016-02-01"),to=as.Date("2017-02-20"),by="day"),each=9)
date_type<-data.table(date=dateline,type=types$type)

users_by_type<-fread("F:\\Project\\20170220投资额度预测\\users_by_type.csv")%>%
  .[,date:=as.Date(date)]

z0<-"select	b.*,
		a.type,
c.type_num,
d.num,
d.amount,
d.invest_num_ratio as invest_num_ratio,
d.amount_ratio,
d.amount_per_num		
from date_type a
left join calendar b on a.date=b.date
left join users_by_type c on a.date=c.date and a.type=c.type
left join invest_by_type d on a.date=d.date and a.type=d.type"

aa<-as.data.table(sqldf(z0))
aa[is.na(aa)]<-0
aa[,type_num_ratio:=ifelse(type_num==0,0,round(num/type_num,4))]

write.csv(aa,"F:\\Project\\20170220投资额度预测\\invest_by_type.csv",row.names = F)

