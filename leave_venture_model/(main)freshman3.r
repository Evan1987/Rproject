library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(sqldf)
source('~/rstudio/leave_venture_model/(fun)fun.log_filter_with_period.r',encoding = 'UTF-8')
source('~/rstudio/leave_venture_model/(fun)fun.model_output.r', encoding = 'UTF-8')
source('~/rstudio/leave_venture_model/(fun)fun.manual_forrest.r', encoding = 'UTF-8')
path<-"F:/Project/20170419用户流失风险预警模型/invest1st_to_leave/rawdata/"
last_date=as.Date("2017-04-24")
days_threhold=20
xdays=3
user_info<-read_csv(paste0(path,"user_info.csv"))%>%as.data.table(.)
user_life_cycle<-read_csv(paste0(path,"user_life_cycle.csv"))%>%as.data.table(.)%>%
  .[user_info[,c("userid","last_day_before_leave")],on="userid",nomatch=NA]
###################################### 1.筛选freshman时期的各项原始记录 #################################
#####app_log
user_app_log<-read_csv(paste0(path,"user_app_log.csv"))%>%as.data.table(.)%T>%
  setorder(.,userid,part_log_day)%>%
  .[,index:=seq_along(part_log_day),by=userid]%>%
  .[user_info[,c("userid","invest1st_time")],on="userid"]%>%
  .[,timediff:=difftime(part_log_day,as.Date(invest1st_time),units = "days")%>%as.numeric(.)%>%round(.)]%>%
  fun.log_filter_with_period(.,
                             period="freshman",
                             time_col="part_log_day",
                             life_cycle_info=copy(user_life_cycle)%>%.[,freshman_end_day:=freshman_end_day+xdays],
                             last_date=last_date)%T>%
  setorder(.,userid,-part_log_day)%>%
  .[,rev_diffdate:=c(0,diff.Date(part_log_day))%>%abs(.),by=userid]%T>%
  setorder(.,userid,part_log_day)

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

##################################### 2.进阶数据整合 #######################################

premium_log<-cash_log[,.(premium_final=last(premium_final),timediff=last(timediff)),by=.(userid,log_day)]%T>%
  setkey(.,userid,log_day)%>%
  .[,premium_begin:=dplyr::lag(premium_final,default = 0),by=userid]%>%
  .[,premium_change:=premium_final-premium_begin]%>%
  .[,premium_change_ratio:=premium_change/(premium_begin+0.01)%>%round(.,2)]

##用户首投后（含）第7日存量
premium_log_7<-{
  tmp<-premium_log[timediff<=6,.(log_day=max(log_day)),by=userid]%>%setkey(.,userid,log_day)
  premium_log[tmp,on=c("userid"="userid","log_day"="log_day"),nomatch=NA][,c("userid","log_day","premium_final")]
}
##用户首投后（含）第14日存量
premium_log_14<-{
  tmp<-premium_log[timediff<=13,.(log_day=max(log_day)),by=userid]%>%setkey(.,userid,log_day)
  premium_log[tmp,on=c("userid"="userid","log_day"="log_day"),nomatch=NA][,c("userid","log_day","premium_final")]
}

#####大额赎回信息
big_redeem_info<-premium_log[premium_change<=-100&premium_final<=100&premium_change_ratio<=-0.75,]
invest_summary<-invest_log[,.(amount=sum(amount)),by=.(userid,log_day)]
#补充用户最后访问日后的未访问天数
user_app_summary<-user_app_log[,.(last_visit_day=max(part_log_day)),by=userid]
user_app_summary[user_life_cycle[,c("userid","last_day_before_leave","freshman_end_day")],
                 on=c("userid"="userid"),
                 expect_wait_days:=ifelse(is.na(last_day_before_leave),
                                          ifelse(is.na(freshman_end_day),
                                                 difftime(last_date,last_visit_day,units = "days"),
                                                 difftime(freshman_end_day,last_visit_day,units = "days")+xdays+1),
                                          difftime(last_day_before_leave,last_visit_day,units = "days")+xdays+1)]

big_redeem_app_info<-user_app_log[,c("userid","part_log_day","rev_diffdate")]%>%
  .[userid%in%unique(big_redeem_info$userid),]%>%
  .[big_redeem_info,on=c("userid"="userid","part_log_day"="log_day"),redeem:=1]%>%
  .[invest_summary,on=c("userid"="userid","part_log_day"="log_day"),invest:=1]%>%
  .[user_app_summary,
    on=c("userid"="userid","part_log_day"="last_visit_day"),
    expect_wait_days:=i.expect_wait_days]%>%
  .[,(c("redeem","invest","expect_wait_days")):=lapply(.SD,function(x) replace(x,is.na(x),0)),
    .SDcols=c("redeem","invest","expect_wait_days")]%>%
  .[,invest:=ifelse(redeem==1,0,invest)]

valid_big_redeem<-big_redeem_app_info[,redeem_cum:=cumsum(redeem),by=userid]%>%
  .[redeem_cum>0,]%>%
  .[,invest_cum:=cumsum(invest),by=.(userid,redeem_cum)]%>%
  .[invest_cum==0,]%>%
  .[rev_diffdate+expect_wait_days>=xdays+1,]
  
big_redeem_leave_judge<-copy(valid_big_redeem)%>%
  .[,leave_judge_day:=part_log_day+xdays+1]%>%
  .[,c("userid","part_log_day","leave_judge_day")]%T>%
  setorder(.,userid,leave_judge_day)

big_redeem_leave_user<-unique(big_redeem_leave_judge[,c("userid")])

##################################### 3.用户选择 #######################################
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
leave_user<-total_leave_user
leave_user_list<-leave_user$userid
stay_user_list<-total_stay_user_list
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
##################################### 4.决策构建 #####################################
z0<-"select	a.*,
		b.*,
c.*,
d.premium_final as premium_final_7,
e.premium_final as premium_final_14,
case when f.userid is null then 0 else 1 end as ishaving_big_redeem
from user_list a
left join 
(select	userid,
sum(timediff<=6) as visit_num_7,
sum(timediff<=13) as visit_num_14
from user_app_log
group by userid
)b on a.userid=b.userid
left join
(select userid,
sum((timediff<=6)*(label==1)) as invest_num_7,
sum((timediff<=6)*(label==2)) as redeem_num_7,
min(case when timediff<=6 then premium_final else null end) as min_premium_7,
max(case when timediff<=6 then premium_final else null end) as max_premium_7,
avg(case when timediff<=6 then premium_final else null end) as mean_premium_7,
sum((timediff<=13)*(label==1)) as invest_num_14,
sum((timediff<=13)*(label==2)) as redeem_num_14,
min(case when timediff<=13 then premium_final else null end) as min_premium_14,
max(case when timediff<=13 then premium_final else null end) as max_premium_14,
min(case when timediff<=13 and timediff>=7 then premium_final else null end) as min_premium_7_14,
max(case when timediff<=13 and timediff>=7 then premium_final else null end) as max_premium_7_14,
avg(case when timediff<=13 and timediff>=7 then premium_final else null end) as mean_premium_7_14
from cash_log
group by userid
)c on a.userid=c.userid
left join premium_log_7 d on a.userid=d.userid
left join premium_log_14 e on a.userid=e.userid
left join big_redeem_leave_user f on a.userid = f.userid"

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

value_col_list=list(names(mydata)%>%.[which(str_detect(.,"7")&!str_detect(.,"14"))],
                    names(mydata)%>%.[which(str_detect(.,"14"))],
                    "ishaving_big_redeem")

error_cost<-matrix(c(0,1,6,0),nrow = 2)
parms<-list(split="gini",loss=error_cost)

result<-fun.manual_forest(df=mydata,
                          trainning_list = trainning_list,
                          testing_list = testing_list,
                          label_col = "isleave",
                          value_col_list = value_col_list,
                          parms = parms,prune = T)

result$pred_performance


