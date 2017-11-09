library(data.table)
library(plyr)
library(sqldf)
library(ggplot2)
source('~/rstudio/(fun)colname_treat.R')

dateline<-seq(from=as.Date("2016-10-01"),to=as.Date("2016-12-31"),by="days")
dateline<-data.frame(check_date=dateline)

premium_data<-fread("F:\\Project\\20161220运营新分析模式\\（20160901-20161231）存量数据.csv")
premium_data[,log_day:=as.Date(as.character(log_day),format="%Y%m%d")]

invest_data<-fread("F:\\Project\\20161220运营新分析模式\\（20160901-20161231）投资数据.csv")
invest_data<-colname_treat(invest_data)
invest_data<-arrange(invest_data,date,userid)
invest_data[,":="(date=as.Date(date),
                  reg_time=as.POSIXct(reg_time),
                  invest1st_time=as.POSIXct(invest1st_time))]
invest_data[,days_span:=as.numeric(date-as.Date(invest1st_time))]



redeem_data<-fread("F:\\Project\\20161220运营新分析模式\\（20160901-20161231）赎回数据.csv")
redeem_data<-colname_treat(redeem_data)
redeem_data<-redeem_data[invest1st_time!='null',]
redeem_data<-arrange(redeem_data,date,userid)
redeem_data[,":="(date=as.Date(date),
                  reg_time=as.POSIXct(reg_time),
                  invest1st_time=as.POSIXct(invest1st_time))]
redeem_data[,days_span:=as.numeric(date-as.Date(invest1st_time))]			  




rush_data<-fread("F:\\Project\\20161220运营新分析模式\\（20160901-20161231）刷子行为.csv")
rush_data$check_date<-as.Date(rush_data$check_date)


#生成每日刷子用户的检查日志
z_rush<-"select a.check_date,
b.userid,
count(*) as rush_num
from dateline a
left join rush_data b on b.check_date<=a.check_date-1 and b.check_date>=a.check_date-30
group by  a.check_date,b.userid"

rush_check<-as.data.table(sqldf(z_rush))
rush_check<-rush_check[rush_num>=3,]

#生成每日最近30日平均存量的检查日志
#z_premium<-"select	a.check_date,
#b.userid,
#sum(b.premium_final)/30 as avg_premium
#from dateline a
#left join premium_data b on b.log_day<=a.check_date-1 and b.log_day>=a.check_date-30
#group by a.check_date,b.userid"

#premium_check<-as.data.table(sqldf(z_premium))
#premium_check[,check_date:=as.Date(check_date)]

a1<-fread("F:/Project/20161220运营新分析模式/premium_check01.csv")
a2<-fread("F:/Project/20161220运营新分析模式/premium_check02.csv")
a3<-fread("F:/Project/20161220运营新分析模式/premium_check03.csv")
premium_check<-rbind(a3,rbind(a1,a2))


#重整投资表，增加用户分层
z_invest_01<-"select	a.check_date,
b.userid,
b.reg_time,
b.invest1st_time,
case when d.userid is not null then 'rush'
when b.days_span = 0 then 'new_comer'
when b.days_span<=30 then 'fresh_user'
when c.avg_premium<=100 or c.avg_premium is null then 'E'
when c.avg_premium<=2000 then 'D'
when c.avg_premium<=7000 then 'C'
when c.avg_premium<=40000 then 'B'
when c.avg_premium<=180000 then 'A'
when c.avg_premium>180000 then 'S' 
end as `type`,
b.invest_amount		
from dateline a
left join invest_data b on b.date=a.check_date
left join premium_check c on c.userid=b.userid and c.check_date=a.check_date
left join rush_check d on d.check_date=c.check_date and d.userid=b.userid"

invest_data_all<-as.data.table(sqldf(z_invest_01))

#投资行为汇总
z_invest_02<-"select  check_date,
`type`,
count(*) as num,
sum(invest_amount) as invest_amount_total
from invest_data_all
group by check_date,`type`"

invest_summary<-as.data.table(sqldf(z_invest_02))

#重整赎回表，增加用户分层
z_redeem_01<-"select	a.check_date,
b.userid,
b.reg_time,
b.invest1st_time,
case  when d.userid is not null then 'rush'
when b.days_span = 0 then 'new_comer'
when b.days_span<=30 then 'fresh_user'
when c.avg_premium<=100 or c.avg_premium is null then 'E'
when c.avg_premium<=2000 then 'D'
when c.avg_premium<=7000 then 'C'
when c.avg_premium<=40000 then 'B'
when c.avg_premium<=180000 then 'A'
when c.avg_premium>180000 then 'S' 
end as `type`,
b.redeem_amount		
from dateline a
left join redeem_data b on b.date=a.check_date
left join premium_check c on c.userid=b.userid and c.check_date=a.check_date
left join rush_check d on d.check_date=c.check_date and d.userid=b.userid"

redeem_data_all<-as.data.table(sqldf(z_redeem_01))


#赎回行为汇总
z_redeem_02<-"select  check_date,
`type`,
count(*) as num,
sum(redeem_amount) as redeem_amount_total
from redeem_data_all
group by check_date,`type`"

redeem_summary<-as.data.table(sqldf(z_redeem_02))



#重整存量表，增加用户分层
z_premium_01<-"select	a.check_date,
b.userid,
case  when f.userid is not null then 'rush'
when d.days_span=0 or e.days_span=0 then 'new_comer'
when d.days_span<=30 or e.days_span<=30 then 'fresh_user'
when c.avg_premium<=100 or c.avg_premium is null then 'E'
when c.avg_premium<=2000 then 'D'
when c.avg_premium<=7000 then 'C'
when c.avg_premium<=40000 then 'B'
when c.avg_premium<=180000 then 'A'
when c.avg_premium>180000 then 'S'
end as `type`,
b.premium_final
from dateline a
left join premium_data b on b.log_day=a.check_date
left join premium_check c on c.userid=b.userid and c.check_date=a.check_date
left join invest_data d on a.check_date=d.date and b.userid=d.userid
left join redeem_data e on a.check_date=e.date and b.userid=e.userid
left join rush_check f on f.check_date=a.check_date and f.userid=b.userid"

premium_data_all<-as.data.table(sqldf(z_premium_01))

#存量汇总
z_premium_02<-"select	check_date,
`type`,
count(*) as num,
sum(premium_final) as premium_amount_total
from premium_data_all
group by check_date,`type`"

premium_summary<-as.data.table(sqldf(z_premium_02))

types<-unique(premium_summary$type)
for (i in 1:length(types)) {
  a<-types[i]
  qq<-qplot(data=premium_summary[type==a,],x=check_date,y=premium_amount_total,geom="line",colour="red",main=a,ylab="premium")
  path<-paste0("F:/temp/",a,".png")
  ggsave(filename = path,plot = qq,width = 8,height = 6)
}


# Find the userid contributed to the loss of premium on Dec
a1<-premium_data_all[check_date==as.Date("2016-11-30"),]
a2<-premium_data_all[check_date==as.Date("2016-12-31"),]
zz<-"select	a.check_date,
		a.userid as userid,
a.type as start_type,
a.premium_final as start_premium,
b.check_date,
b.type as end_type,
b.premium_final as end_premium,
round((b.premium_final-a.premium_final)/a.premium_final,2) as decrease_level
from a1 a
left join a2 b on a.userid=b.userid
where b.premium_final<a.premium_final"

loss_users<-as.data.table(sqldf(zz))
loss_users[,decrease_amount:=start_premium-end_premium]
main_loss_users<-loss_users[start_type=="S"|start_type=="A",]

user_focused<-main_loss_users[decrease_level<=-0.4,]
user_focused<-arrange(user_focused,-decrease_amount,-start_premium)
user_focused<-as.data.frame(user_focused)
user_for_investigated<-user_focused[1:1000,]
user_for_investigated<-arrange(user_for_investigated,userid)
