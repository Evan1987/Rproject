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


######### 1.选定freshman时期流失的用户和非流失用户，以及训练集和测试集覆盖的用户群
leave_user<-user_info[isleave==1&leave_period=="freshman",]

#这些用户若不是已流失用户，则不可按照未流失用户进行训练。需要从样本信息中筛除。
leave_user_list<-leave_user$userid
stay_user_list<-user_info[user_life_cycle[,c("userid","freshman_end_day")],on="userid"]%>%
  .[!is.na(freshman_end_day)&(isleave==0|last_day_before_leave>freshman_end_day),]%>%
  .$userid
stay_user<-user_info[userid%in%stay_user_list,]

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


######## 2.筛选freshman时期的各项原始记录
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
#####premium_log
user_premium_log<-fread(paste0(path,"user_premium_log.csv"))%>%
  .[,log_day:=as.Date(log_day)]%>%
  setorder(.,userid,log_day)%>%
  fun.log_filter_with_period(.,
                             period="freshman",
                             time_col="log_day",
                             life_cycle_info=user_life_cycle,
                             last_date=last_date)%>%
                             {
                               xx<-.[userid%in%leave_user_list,]
                               yy<-leave_user[,c("userid","last_day_before_leave")]
                               log_append<-xx[yy,on="userid",nomatch=0]%>%
                                 .[,.SD[CJ(log_day=seq.Date(max(log_day)+1,first(last_day_before_leave)+7,by="days")),
                                        on="log_day",roll=T],
                                   by="userid"]%>%
                                 .[,-"last_day_before_leave"]%>%
                                 .[,names(.)[6:ncol(.)]:=0]
                               
                               rbind(.,log_append)
                             }%>%
  setkey(.,userid,log_day)

##构建大额赎回之后的信息表
big_redeem_log<-user_premium_log[premium_change_ratio<=-0.8&premium_change<=-100,]
big_redeem_invest_info<-invest_log[big_redeem_log[,c("userid","log_day")],on="userid"]%>%
  .[between(difftime(log_day,i.log_day,units = "days"),1,7),]%>%
  .[,.(amount=sum(amount)),by=.(userid,i.log_day)]%>%
  .[,eval_date:=i.log_day+7]%>%
  .[,-"i.log_day"]
big_redeem_app_info<-user_app_log[big_redeem_log[,c("userid","log_day")],on="userid"]%>%
  .[between(difftime(part_log_day,log_day,units = "days"),1,7),]%>%
  .[,.(num=.N),by=.(userid,log_day)]%>%
  .[,eval_date:=log_day+7]%>%
  .[,-"log_day"]

#用户移动7天的平均访问次数
user_app_cumsum<-{
  xx<-user_app_log[,c("userid","part_log_day")]
  yy<-xx[,.SD[CJ(part_log_day=seq.Date(min(part_log_day),
                                       max(part_log_day),
                                       by = "days")),
              on="part_log_day"]
         ,by=userid]%T>%{names(.)<-c("userid","start")}%>%
    .[,end:=start+6]
  result<-xx[yy,on=.(userid=userid,part_log_day>=start,part_log_day<=end),
             nomatch=0,allow.cartesian=T]%>%
    .[,.(num=.N),by=.(userid,part_log_day.1)]%T>%
    {names(.)<-c("userid","log_day","visit_num")}
}


mydata<-user_premium_log[,c("userid","log_day","premium_final")]%>%
  .[user_info[,c("userid","invest1st_time","last_day_before_leave")],on="userid"]%>%
  big_redeem_log[,c("userid","log_day","premium_change_ratio")][.,on=c("userid"="userid","log_day"="log_day"),nomatch=NA]%>%
  big_redeem_invest_info[.,on=c("userid"="userid","eval_date"="log_day"),nomatch=NA]%>%
  big_redeem_app_info[.,on=c("userid"="userid","eval_date"="eval_date"),nomatch=NA]%>%
  user_app_cumsum[.,on=c("userid"="userid","log_day"="eval_date"),nomatch=NA]%>%
  .[userid%in%user_list$userid&log_day>=as.Date(invest1st_time)+6,]%>%
  .[,isleave:=ifelse(userid%in%leave_user_list&
                      between(difftime(log_day,last_day_before_leave,units = "days"),0,7),1,0)]%>%
  .[,-c("last_day_before_leave","invest1st_time")]%>%
  {.[,3:ncol(.)][is.na(.[,3:ncol(.)])]<-0}

# mydata[,3:ncol(mydata)][is.na(mydata[,3:ncol(mydata)])]<-0







result<-fun.model_output(mydata,
                         trainning_list,
                         testing_list,
                         label_col = "isleave",
                         value_col = names(mydata)[3:7])



testing_result<-result$testing_result
















