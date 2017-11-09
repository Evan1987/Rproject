
#只针对type_num项做预测，其中涉及两个数据源：
#①trainSet:抽取格式及字段请参考hive保存sql——invest_by_type
#②calendar：参考项目目录下的calendar.csv
cash_flow_type_num_train<-function(
                          user_type=c("S","A","B","C","D","brush","freshman","newcomer"),
                          trainTimeSpan=c("2016-09-01","2017-02-27"),
                          trainSetPath,
                          calendarPath)
{
  library(data.table)
  library(magrittr)
  library(sqldf)
  library(tcltk)
  library(plyr)
  library(lubridate)
  library(stringr)
  
  if(missing(user_type)){
    stop("The user_type for training must be specified!")
  }
  if(any(user_type!="freshman")){
    if(length(trainTimeSpan)!=2){
      stop("The timeSpan for training must be specified and have 2 elements.")
    }
    else{
      #对于freshman来说，无需利用训练得到，而是利用newcomer和全部原始数据集公式计算可得
      #因此对于freshman，设置trainTimeSpan没有太多意义，而此处设置的2017-02-27应是trainSet的最大时间
      trainTimeSpan<-c("2016-02-01","2017-02-27")
    }
  }
  
  # *******************************0. 构建原始训练集*************************************
  # 用户数据集
  calendar<-fread(calendarPath)%>%
    .[,date:=as.Date(date)]%>%
    .[between(date,as.Date(trainTimeSpan[1]),as.Date(trainTimeSpan[2])),]
  
  type<-str_c(user_type,collapse="")
  date_type<-seq.Date(from=as.Date(trainTimeSpan[1]),to=as.Date(trainTimeSpan[2]),by="day")%>%
    data.table(date=.,type=type)
  
  trainSet<-fread(trainSetPath)
  temp<-trainSet[type %in% user_type,]%>%
    .[,.(num=sum(num),amount=sum(amount),type_num=sum(type_num)),by=date]%>%
    .[,":="(date=as.Date(date),
            type=type,
            type_num_ratio=num/type_num,
            amount_per_num=amount/num)]%T>%
    setkeyv(.,c("date","type"))%>%
    .[date_type,on=c("date"="date","type"="type"),nomatch=NA,mult="all"]
  
  temp[is.na(temp)]<-0
  temp<-temp[calendar,on=c("date"="date"),nomatch=NA,mult="all"]

  # *******************************根据不同用户类，做预测或输出预测相关数据****************
  
  # 对于S-D人群采用linear估计，输出在coef里，同时输出预测原点forecast_initial
  if(all(user_type %in% c("S","A","B","C","D"))){
    lr_summary<-lm(type_num~date,temp)%>%
      summary(.)
    coef<-lr_summary$coefficients%>%
      data.frame(.)%>%
      data.frame(coef=row.names(.),.)
    colnames(coef)<-c("coef","value","sd","t_value","p_value")
    row.names(coef)<-NULL
    
    remark<-"linear"
    pdl<-data.frame()
    forecast_initial<-temp[date==as.Date(trainTimeSpan[2]),]
  }
  # 对于newcomer和brush用户，由于随机性太大，所以之后预测时用有放回抽样来代替，输出抽样权重pdl
  else if(all(user_type %in% c("newcomer","brush"))){
    high_value<-quantile(temp$type_num,0.95)
    low_value<-quantile(temp$type_num,0.05)
    temp_trimmed<-temp[between(type_num,low_value,high_value),]
    num_density<-density(temp_trimmed$type_num)
    unit<-mean(diff(num_density$x,1))
    
    remark<-"random"
    pdl<-data.frame(x=num_density$x,y=num_density$y*unit)
    coef<-data.frame()
    forecast_initial<-NULL
  }
  # 对于freshman，可以结合之前的newcomer和预测原点的值来计算得出
  else if(all(user_type=="freshman")){
    
    # 由于之后推算freshman人数时，需要用到之前30天内的newcomer人数，所以训练集的时间跨度不能小于30
    if(difftime(as.Date(trainTimeSpan[2]),as.Date(trainTimeSpan[1]),units = "days")<30){
      stop("For freshman type_num forecasting, no less than 30 training timespan is required!")
    }
    # 用上面的方法输出newcomer的pdl
    
    newcomer_temp<-trainSet[type=="newcomer",]%>%
      .[,.(num=sum(num),amount=sum(amount),type_num=sum(type_num)),by=date]%>%
      .[,":="(date=as.Date(date),
              type=type,
              type_num_ratio=num/type_num,
              amount_per_num=amount/num)]%T>%
      setkeyv(.,c("date","type"))%>%
      .[date_type,on=c("date"="date","type"="type"),nomatch=NA,mult="all"]
    newcomer_temp[is.na(newcomer_temp)]<-0
    newcomer_temp<-newcomer_temp[calendar,on=c("date"="date"),nomatch=NA,mult="all"]%>%
      .[,type:="newcomer"]
    
    high_value<-quantile(newcomer_temp$type_num,0.95)
    low_value<-quantile(newcomer_temp$type_num,0.05)
    newcomer_temp_trimmed<-newcomer_temp[between(type_num,low_value,high_value),]
    num_density<-density(newcomer_temp_trimmed$type_num)
    unit<-mean(diff(num_density$x,1))
    
    remark<-"no forecast"
    pdl<-data.frame(x=num_density$x,y=num_density$y*unit)
    coef<-data.frame()
    forecast_initial<-rbind(temp[date==as.Date(trainTimeSpan[2]),],newcomer_temp)
  }
  else{
    stop("The user_type is invalid!")
  }
  forecast_result<-structure(list(user_type=type,
                                  remark=remark,
                                  coef=coef,
                                  pdl=pdl,
                                  forecast_initial=forecast_initial),
                             class="cash_flow_type_num_train")
  
  return(forecast_result)
}