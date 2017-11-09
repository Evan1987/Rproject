#通用
library(data.table)
library(varhandle)
library(sqldf)
setwd("D:\\Project\\20161027用户活动标签\\csv\\01基础数据")
files<-dir()
files<-as.data.frame(files,stringsAsFactors=F)
#选取活动数据,并对数值格式进行处理。
mydata<-fread(files[12,1],encoding = "UTF-8",stringsAsFactors = F)
name<-colnames(mydata)

for (i in 2:5) {
  mydata[,name[i]:=as.numeric(get(name[i]))]
  mydata[is.na(eval(parse(text=name[i]))),name[i]:=0]
}

###活动时长参数，为了计算活动期定期投资频次
activity_span_days<-as.numeric(difftime("2016-06-12","2016-06-07"))+1
###投资时长列，为了计算之前定期投资频次
mydata[,span_days:=as.numeric(difftime("2016-06-07",max(reg_day,"2016-03-07"),units="days"))]
###活动期投资频次
exp_factor_F<-expression(regular_freq/activity_span_days)
###之前的投资频次，加平滑
exp_factor_F_formal<-expression((regular_freq_formal+1)/(span_days+2))
###活动期单笔定期投资额
exp_factor_MF<-expression(regular_total/regular_freq)
###之前的单笔投资额
exp_factor_MF_formal<-expression(ifelse(regular_freq_formal==0,1000,
                                        regular_total_formal/regular_freq_formal))

#指标化
mydata[,":="(factor_F=eval(exp_factor_F),
             factor_F_formal=eval(exp_factor_F_formal),
             factor_MF=eval(exp_factor_MF),
             factor_MF_formal=eval(exp_factor_MF_formal))]
#任务备份
task<-copy(mydata)
#去掉多余的列
task[,c(2:5,7:9):=NULL]
#指标向量化
task[,":="(F_value=round(factor_F/factor_F_formal,2),
           MF_value=round(factor_MF/factor_MF_formal,2))]
#去掉多余列
task[,(3:6):=NULL]
write.csv(task,"D:\\Project\\20161027用户活动标签\\csv\\02参与向量\\Result20160607.csv",row.names = F)