#通用
library(data.table)
library(varhandle)
library(sqldf)
setwd("D:\\Project\\20161027用户活动标签\\csv\\01基础数据")
files<-dir()
files<-as.data.frame(files,stringsAsFactors=F)
#选取活动数据,并对数值格式进行处理。
mydata<-fread(files[15,1],encoding = "UTF-8",stringsAsFactors = F)
name<-colnames(mydata)

for (i in 2:13) {
  mydata[,name[i]:=as.numeric(get(name[i]))]
  mydata[is.na(eval(parse(text=name[i]))),name[i]:=0]
}

###活动时长参数，为了计算活动期定期投资频次
activity_span_days<-as.numeric(difftime("2016-07-22","2016-07-13"))+1
###投资时长列，为了计算之前定期投资频次
mydata[,span_days:=as.numeric(difftime("2016-07-12",max(reg_day,"2016-01-01"),units="days"))]
###活动期投资频次
exp_factor_F3<-expression(regular3_freq/activity_span_days)
exp_factor_F6<-expression(regular6_freq/activity_span_days)
exp_factor_F12<-expression(regular12_freq/activity_span_days)
###之前的投资频次，加平滑
exp_factor_F3_formal<-expression((regular3_freq_formal+1)/(span_days+2))
exp_factor_F6_formal<-expression((regular6_freq_formal+1)/(span_days+2))
exp_factor_F12_formal<-expression((regular12_freq_formal+1)/(span_days+2))
###活动期单笔定期投资额
exp_factor_MF3<-expression(ifelse(regular3_freq==0,0,regular3_total/regular3_freq))
exp_factor_MF6<-expression(ifelse(regular6_freq==0,0,regular6_total/regular6_freq))
exp_factor_MF12<-expression(ifelse(regular12_freq==0,0,regular12_total/regular12_freq))
###之前的单笔投资额
exp_factor_MF3_formal<-expression(ifelse(regular3_freq_formal==0,1000,
                                        regular3_total_formal/regular3_freq_formal))
exp_factor_MF6_formal<-expression(ifelse(regular6_freq_formal==0,1000,
                                         regular6_total_formal/regular6_freq_formal))
exp_factor_MF12_formal<-expression(ifelse(regular12_freq_formal==0,1000,
                                         regular12_total_formal/regular12_freq_formal))

#指标化
mydata[,":="(factor_F3=eval(exp_factor_F3),
             factor_F6=eval(exp_factor_F6),
             factor_F12=eval(exp_factor_F12),
             factor_F3_formal=eval(exp_factor_F3_formal),
             factor_F6_formal=eval(exp_factor_F6_formal),
             factor_F12_formal=eval(exp_factor_F12_formal),
             factor_MF3=eval(exp_factor_MF3),
             factor_MF6=eval(exp_factor_MF6),
             factor_MF12=eval(exp_factor_MF12),
             factor_MF3_formal=eval(exp_factor_MF3_formal),
             factor_MF6_formal=eval(exp_factor_MF6_formal),
             factor_MF12_formal=eval(exp_factor_MF12_formal)
             )]
#任务备份
task<-copy(mydata)
#去掉多余的列
task[,c(2:13,15:17):=NULL]
#指标向量化
task[,":="(F_value=round((1*factor_F3/factor_F3_formal+
                         2*factor_F6/factor_F6_formal+
                         4*factor_F12/factor_F12_formal)/7,2),
           MF_value=round((1*factor_MF3/factor_MF3_formal+
                          2*factor_MF6/factor_MF6_formal+
                          4*factor_MF12/factor_MF12_formal)/7,2))]
#去掉多余列
task[,(3:14):=NULL]
write.csv(task,"D:\\Project\\20161027用户活动标签\\csv\\02参与向量\\Result20160713.csv",row.names = F)