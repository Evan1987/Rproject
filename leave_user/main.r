library(data.table)
library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
source('~/rstudio/leave_user/(fun)fun.elbow_point.r', encoding = 'UTF-8', echo=TRUE)
path="F:/Project/20170314离场用户调研/"
rawdata<-fread(paste0(path,"leave_user_mining.csv"))

workdata<-tbl_df(rawdata)%>%
  rename_(.,.dots=setNames(names(.),names(.)%>%str_sub(.,start=3)))%>%
  mutate(.,last_day_before_leave=as.Date(last_day_before_leave),
           leave_mean_premium=round(leave_mean_premium),
           after_mean_premium=round(after_mean_premium),
           after_invest_amount=round(after_invest_amount),
           after_redeem_amount=round(after_redeem_amount),
           after_visit_num=ifelse(after_visit_num=="NULL","0",after_visit_num)%>%as.numeric(.),
           remark=ifelse(after_invest_amount>0,0,1)%>%factor(.))

#选出疑似回访用户
falsedata<-filter(workdata,remark==0)
#p<-ggplot(data=workdata,aes(x=leave_mean_premium,y=leave_mean_regular_premium))+geom_point(aes(color=remark))

#选出回访用户中在流失判定时没有定期存量的用户
falsedata_2<-mutate(falsedata,label=ifelse(leave_mean_regular_premium>0,1,0))%>%
  filter(.,label==0)

#group_by(falsedata_2,label)%>%summarise(.,num=n())


#探索流失中增加活期存量判定对召回率的影响，L曲线
breaks=seq(100,10000,100)

result<-data.frame()
for(i in breaks){
  total_num<-filter(workdata,leave_mean_premium<=i)%>%nrow(.)
  back_num<-filter(falsedata_2,leave_mean_premium<=i)%>%nrow(.)
  temp<-data.frame(breaks=i,back_num=back_num,total_num=total_num)
  result<-rbind(result,temp)
}

result<-tbl_df(result)%>%
  mutate(.,callback=round(back_num/total_num,4))

elbow_point<-fun.elbow_point(x=result$breaks,y=result$callback,doplot = T)









