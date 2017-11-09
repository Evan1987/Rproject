#通用
library(data.table)
library(varhandle)
library(sqldf)
setwd("D:\\Project\\20161027用户活动标签\\csv\\01基础数据")
files<-dir()
files<-as.data.frame(files,stringsAsFactors=F)
#选取活动20151013数据,并对数值格式进行处理。
mydata<-fread(files[2,1],encoding = "UTF-8",stringsAsFactors = F)
name<-colnames(mydata)

for (i in 2:7) {
  mydata[,name[i]:=as.numeric(get(name[i]))]
  mydata[is.na(eval(parse(text=name[i]))),name[i]:=0]
}
###活动时长参数，为了计算活动期投资频次
activity_span_days<-as.numeric(difftime("2015-10-13","2015-10-13"))+1
###注册时长列，为了计算之前投资频次
mydata[,span_days:=as.numeric(difftime("2015-10-13",reg_day,units="days"))]
###投资时长列，为了计算之前的赎回周期
mydata[,invest_span_days:=as.numeric(difftime("2015-10-13",invest1st_day,units="days"))]
###活动期投资频次
exp_factor_F<-expression(invest_freq/activity_span_days)
###之前的投资频次，加平滑
exp_factor_F_formal<-expression((invest_freq_formal+1)/(span_days+2))
###活动期单笔投资额
exp_factor_MF<-expression(invest_total/invest_freq)
###之前的单笔投资额
exp_factor_MF_formal<-expression(ifelse(invest_freq_formal==0,100,invest_total_formal/invest_freq_formal))
###活动期的资金赎回周期，最长为30天
exp_factor_RF<-expression(ifelse(retain_days>30,30,retain_days))
###之前的资金赎回周期，加平滑
exp_factor_RF_formal<-expression((invest_span_days+15)/(redeem_freq_formal+1))

#指标化
mydata[,":="(factor_F=eval(exp_factor_F),
             factor_F_formal=eval(exp_factor_F_formal),
             factor_MF=eval(exp_factor_MF),
             factor_MF_formal=eval(exp_factor_MF_formal),
             factor_RF=eval(exp_factor_RF),
             factor_RF_formal=eval(exp_factor_RF_formal))]

#任务备份
task<-copy(mydata)
#去掉多余的列
task[,c(2:7,9:11):=NULL]
#指标向量化
task[,":="(F_value=round(factor_F/factor_F_formal,2),
           MF_value=round(factor_MF/factor_MF_formal,2),
           RF_value=round(factor_RF/factor_RF_formal,2))]
#去掉多余列
task[,(3:8):=NULL]
write.csv(task,"D:\\Project\\20161027用户活动标签\\csv\\02参与向量\\Result20151013.csv",row.names = F)


