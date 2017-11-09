###通用
library(data.table)
library(varhandle)
library(sqldf)
setwd("D:\\Project\\20161027用户活动标签\\csv\\01基础数据")
files<-dir()
files<-as.data.frame(files,stringsAsFactors=F)

##选取活动20150827数据,并对数值格式进行处理。
mydata<-fread(files[1,1],encoding = "UTF-8",stringsAsFactors = F)
name<-colnames(mydata)
for (i in 3:6) {
  mydata[,name[i]:=as.numeric(get(name[i]))]
  mydata[is.na(eval(parse(text=name[i]))),name[i]:=0]
}
##增加计算维度列，设置计算公式
activity_span_days<-as.numeric(difftime("2015-08-27","2015-08-27"))+1
mydata[,span_days:=as.numeric(difftime("2015-08-27",reg_day,units="days"))]
exp_factor_F<-expression(invest_freq/activity_span_days)
exp_factor_F_formal<-expression((invest_freq_formal+1)/(span_days+2))
exp_factor_MF<-expression(invest_total/invest_freq)
exp_factor_MF_formal<-expression(ifelse(invest_freq_formal==0,100,invest_total_formal/invest_freq_formal))

mydata[,":="(factor_F=eval(exp_factor_F),
             factor_F_formal=eval(exp_factor_F_formal))]
mydata[,":="(factor_MF=eval(exp_factor_MF),
             factor_MF_formal=eval(exp_factor_MF_formal))]
##去掉多余分析列，并做备份
task<-copy(mydata)
task[,c(1,3:6,8,9):=NULL]
task[,":="(F_value=round(factor_F/factor_F_formal,2),
           MF_value=round(factor_MF/factor_MF_formal,2))]
task[,(3:6):=NULL]
write.csv(task,"D:\\Project\\20161027用户活动标签\\csv\\02参与向量\\Result20150827.csv",row.names = F)