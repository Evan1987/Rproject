library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(sqldf)
source('~/rstudio/leave_venture_model/(fun)fun.log_filter_with_period.r',encoding = 'UTF-8')
source('~/rstudio/leave_venture_model/(fun)fun.model_output.r', encoding = 'UTF-8')
path<-"F:/Project/20170419用户流失风险预警模型/invest1st_to_leave/rawdata/"
last_date=as.Date("2017-04-24")
days_threhold=20
user_life_cycle<-read_csv(paste0(path,"user_life_cycle.csv"))%>%as.data.table(.)
user_info<-read_csv(paste0(path,"user_info.csv"))%>%as.data.table(.)
user_app_summary<-read_csv(paste0(path,"user_app_summary.csv"))%>%as.data.table(.)

######## 1.筛选freshman时期的各项原始记录
#####app_log
user_app_log<-read_csv(paste0(path,"user_app_log.csv"))%>%as.data.table(.)%T>%
  setorder(.,userid,part_log_day)%>%
  .[,index:=seq_along(part_log_day),by=userid]%>%
  .[user_info[,c("userid","invest1st_time")],on="userid"]%>%
  .[,timediff:=difftime(part_log_day,as.Date(invest1st_time),units = "days")%>%as.numeric(.)%>%round(.)]%>%
  fun.log_filter_with_period(.,
                             period="freshman",
                             time_col="part_log_day",
                             life_cycle_info=user_life_cycle,
                             last_date=last_date)
  
#####invest_log
invest_log<-read_csv(paste0(path,"invest_info.csv"))%>%as.data.table(.)%T>%
  setorder(.,userid,create_time)%>%
  .[user_info[,c("userid","invest1st_time")],on="userid"]%>%
  .[,timediff:=difftime(log_day,as.Date(invest1st_time),units = "days")%>%as.numeric(.)%>%round(.)]%>%
  fun.log_filter_with_period(.,
                             period="freshman",
                             time_col="log_day",
                             life_cycle_info=user_life_cycle,
                             last_date=last_date)
#####redeem_log
redeem_log<-read_csv(paste0(path,"redeem_info.csv"))%>%as.data.table(.)%T>%
  setorder(.,userid,create_time)%>%
  .[user_info[,c("userid","invest1st_time")],on="userid"]%>%
  .[,timediff:=difftime(log_day,as.Date(invest1st_time),units = "days")%>%as.numeric(.)%>%round(.)]%>%
  fun.log_filter_with_period(.,
                             period="freshman",
                             time_col="log_day",
                             life_cycle_info=user_life_cycle,
                             last_date=last_date)
#####regular_log
regular_log<-read_csv(paste0(path,"regular_info.csv"))%>%as.data.table(.)%T>%
  setorder(.,userid,create_time)%>%
  .[user_info[,c("userid","invest1st_time")],on="userid"]%>%
  .[,timediff:=difftime(log_day,as.Date(invest1st_time),units = "days")%>%as.numeric(.)%>%round(.)]%>%
  fun.log_filter_with_period(.,
                             period="freshman",
                             time_col="log_day",
                             life_cycle_info=user_life_cycle,
                             last_date=last_date)
####premium_log
user_premium_log<-fread(paste0(path,"user_premium_log.csv"))%>%
  .[,log_day:=as.Date(log_day)]%>%
  setorder(.,userid,log_day)%>%
  fun.log_filter_with_period(.,
                             period="freshman",
                             time_col="log_day",
                             life_cycle_info=user_life_cycle,
                             last_date=last_date)
# user_premium_log<-fread(paste0(path,"user_premium_log.csv"))%>%
#   .[,log_day:=as.Date(log_day)]%>%
#   setorder(.,userid,log_day)%>%
#   fun.log_filter_with_period(.,
#                              period="freshman",
#                              time_col="log_day",
#                              life_cycle_info=user_life_cycle,
#                              last_date=last_date)%>%
#   {
#     xx<-user_premium_log[userid%in%leave_user_list,]
#     yy<-leave_user[,c("userid","last_day_before_leave")]
#     log_append<-xx[yy,on="userid",nomatch=0]%>%
#       .[,.SD[CJ(log_day=seq.Date(max(log_day)+1,first(last_day_before_leave)+7,by="days")),
#              on="log_day",roll=T],
#         by="userid"]%>%
#       .[,-"last_day_before_leave"]%>%
#       .[,names(.)[6:ncol(.)]:=0]
# 
#     rbind(.,log_append)
#   }%>%
#   setkey(.,userid,log_day)

# ##构建大额赎回之后的信息表
# big_redeem_log<-user_premium_log[premium_change_ratio<=-0.8&premium_change<=-100,]
# big_redeem_invest_info<-invest_log[big_redeem_log[,c("userid","log_day")],on="userid"]%>%
#   .[between(difftime(log_day,i.log_day,units = "days"),1,7),]%>%
#   .[,.(amount=sum(amount)),by=.(userid,i.log_day)]%>%
#   .[,eval_date:=i.log_day+7]%>%
#   .[,-"i.log_day"]
# big_redeem_app_info<-user_app_log[big_redeem_log[,c("userid","log_day")],on="userid"]%>%
#   .[between(difftime(part_log_day,log_day,units = "days"),1,7),]%>%
#   .[,.(num=.N),by=.(userid,log_day)]%>%
#   .[,eval_date:=log_day+7]%>%
#   .[,-"log_day"]

# #用户移动7天的平均访问次数
# user_app_cumsum<-{
#   xx<-user_app_log[,c("userid","part_log_day")]
#   yy<-xx[,.SD[CJ(part_log_day=seq.Date(min(part_log_day),
#                                        max(part_log_day),
#                                        by = "days")),
#               on="part_log_day"]
#          ,by=userid]%T>%{names(.)<-c("userid","start")}%>%
#     .[,end:=start+6]
#   result<-xx[yy,on=.(userid=userid,part_log_day>=start,part_log_day<=end),
#              nomatch=0,allow.cartesian=T]%>%
#     .[,.(num=.N),by=.(userid,part_log_day.1)]%>%
#     {names(.)<-c("userid","log_day","visit_num")}
# }

  
# mydata<-user_premium_log[,c("userid","log_day","premium_final")]%>%
#   .[user_info[,c("userid","invest1st_time","last_day_before_leave")],on="userid"]%>%
#   big_redeem_invest_info[.,on=c("userid"="userid","eval_date"="log_day"),nomatch=NA]%>%
#   big_redeem_app_info[.,on=c("userid"="userid","eval_date"="eval_date"),nomatch=NA]%>%
#   user_app_cumsum[.,on=c("userid"="userid","log_day"="eval_date"),nomatch=NA]


#####cash_log:模拟存量表
cash_log<-rbind(invest_log[,c("userid","amount","create_time","log_day")][,label:=1],
                redeem_log[,c("userid","amount","create_time","log_day")][,":="(label=2,amount=-amount)])%>%
  setorder(.,userid,create_time,label)%>%
  .[user_info[,c("userid","invest1st_time")],on="userid"]%>%
  .[,timediff:=difftime(log_day,as.Date(invest1st_time),units = "days")%>%as.numeric(.)%>%round(.)]%>%
  .[,premium_final:=cumsum(amount),by=userid]%>%
  .[,premium_final:=ifelse(premium_final<0,0,premium_final)]%>%
  .[,premium_begin:=dplyr::lag(premium_final,default = 0),by=userid]%>%
  .[,redeem_ratio:=ifelse(label==2,round(amount/premium_begin,2),0)]%>%
  .[,redeem_ratio:=ifelse(is.infinite(-redeem_ratio)|abs(redeem_ratio)>1,-1,redeem_ratio)]


####### 2.各项原始记录的进阶处理
#####invest_log按照日期聚合后，加投资次序
invest_summary<-invest_log[,.(amount=sum(amount)),by=.(userid,log_day)]%>%setkey(.,userid,log_day)%>%
  .[,index:=seq_along(log_day),by=userid]

invest_summary2<-dcast(invest_summary[index<=3,],userid~index,value.var = "amount",fill = 0)%T>%
{names(.)<-c("userid","invest_1","invest_2","invest_3")}

invest_summary2_2<-dcast(invest_summary[index<=3,],userid~index,value.var = "log_day",fill = last_date)%T>%
{names(.)<-c("userid","invest_1_day","invest_2_day","invest_3_day")}%>%
  .[,":="(invest_2nd_days=difftime(invest_2_day,invest_1_day,units = "days")%>%as.numeric(.),
          invest_3rd_days=difftime(invest_3_day,invest_1_day,units = "days")%>%as.numeric(.))]

invest_summary3<-invest_summary[,.(invest_num=.N,invest_days=uniqueN(log_day)),by=userid]

##### regular_info定期汇总
regular_summary<-regular_log[,.(amount=sum(amount)),by=userid]

#####premium_log的最大存量和最小存量
premium_summary<-cash_log[,.(min_premium=min(premium_final),max_premium=max(premium_final)),by=userid]

#####
premium_summary2<-user_premium_log[user_info[,c("userid","invest1st_time")],
                                   on=.(userid=userid,log_day-as.Date(invest1st_time)=6),nomatch=NA]
#####最大赎回比例
redeem_summary<-cash_log[,.(max_redeem_ratio=min(redeem_ratio)),by=userid]


######### 3.选定freshman时期流失的用户和非流失用户，以及训练集和测试集覆盖的用户群
total_leave_user<-user_info[isleave==1&leave_period=="freshman",]
##观察用户流失日到首投日（freshman起始日）的时长
##50%-7,5%0-80%-31
quantile(total_leave_user[,xx:=difftime(last_day_before_leave,as.Date(invest1st_time),units = "days")]$xx,
         probs = seq(0,1,0.1))
#这些用户若不是已流失用户，则不可按照未流失用户进行训练。需要从样本信息中筛除。
total_stay_user_list<-user_info[user_life_cycle[,c("userid","freshman_end_day")],on="userid"]%>%
  .[!is.na(freshman_end_day)&(isleave==0|last_day_before_leave>freshman_end_day),]%>%
  .$userid
#################

# 选定第一周内流失的用户和未流失的用户
leave_user<-total_leave_user[difftime(last_day_before_leave,as.Date(invest1st_time),units = "days")<=37,]
leave_user_list<-leave_user$userid
stay_user_list<-c(total_stay_user_list,
                  total_leave_user[difftime(last_day_before_leave,
                                            as.Date(invest1st_time),
                                            units = "days")>37,]$userid)%>%unique(.)
# 输出用户总表
user_list<-{
  a<-data.table(userid=leave_user_list,isleave=1)
  b<-data.table(userid=stay_user_list,isleave=0)
  rbind(a,b)
}
#提取训练集用户
trainning_list<-{
  a<-sample(leave_user_list,ceiling(0.7*length(leave_user_list)))
  b<-sample(stay_user_list,ceiling(0.7*length(stay_user_list)))
  c(a,b)
}
#提取测试集用户
testing_list<-user_list$userid[which(!user_list$userid%in%trainning_list)]

####### 4.决策数决策维度汇总
z0<-"select	a.*,
		b.invest_1,
c.*,
d.*,
e.*,
f.*
from user_list a
left join invest_summary2 b on a.userid=b.userid
left join 
(select	userid,
sum(timediff<=7) as visit_num_7
from user_app_log
group by userid
)c on a.userid=c.userid
left join
(select userid,
sum((timediff<=7)*(label==1)) as invest_num_7,
sum((timediff<=7)*(label==2)) as redeem_num_7
from cash_log
group by userid
)d on a.userid=d.userid
left join
(select	userid,
min(premium_final) as min_premium_7,
max(premium_final) as max_premium_7,
min(redeem_ratio) as max_redeem_ratio_7
from cash_log
where timediff<=7
group by userid
)e on a.userid=e.userid
left join
(select userid,
sum(timediff<=7) as regular_num_7
from regular_log
group by userid
)f on a.userid=f.userid"

mydata<-sqldf(z0)%>%as.data.table(.)%>%
  .[,.SD,.SDcols=-which(names(.)=="userid")[-1]]%>%
  {
    vars<-names(.)[3:ncol(.)]
    .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
  }%>%
  {
    char_vars<-names(.)[which(sapply(.,is.character))]%>%.[-which(.=="userid")]
    if(length(char_vars)>0){.[,(char_vars):=lapply(.SD,as.numeric),.SDcols=char_vars]}
    else(.)
  }


value_cols<-names(mydata)[3:ncol(mydata)]

error_cost<-matrix(c(0,1,1,0),nrow = 2)
parms<-list(split="information",loss=error_cost)

result<-fun.model_output(mydata,
                         trainning_list,
                         testing_list,
                         label_col = "isleave",
                         parms = parms,
                         value_col = value_cols,prune = T)

result$pred_performance

testing_result<-result$testing_result
testing_result[userid%in%total_leave_user$userid,isfinallyleave:=1]%>%
  .[,isfinallyleave:=replace(isfinallyleave,is.na(isfinallyleave),0)]
tmp<-prediction(testing_result$predict_result,testing_result$isfinallyleave)
performance(tmp,"fpr","tpr")

