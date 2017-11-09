
#只针对type_num_ratio和amount_per_num两项做预测，其中涉及两个数据源：
#①trainSet:抽取格式及字段请参考hive保存sql——invest_by_type
#②calendar：参考项目目录下的calendar.csv
cash_flow_type_train<-function(item=c("type_num_ratio","amount_per_num","amount"),
                               user_type=c("S","A","B","C","D","brush","freshman","newcomer"),
                               trainTimeSpan=c("2016-02-01","2017-02-27"),
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
  if(missing(item)){
    stop("The item for training must be specified!")
  }
  if(missing(user_type)){
    stop("The user_type for training must be specified!")
  }
  if(length(trainTimeSpan)!=2){
    stop("The timeSpan for training must be specified and have 2 elements.")
  }
  # *******************************0. 构建原始训练集*************************************
  calendar<-fread(calendarPath)%>%
    .[,date:=as.Date(date)]%>%
    .[between(date,as.Date(trainTimeSpan[1]),as.Date(trainTimeSpan[2])),]
  
  type<-str_c(user_type,collapse="")
  date_type<-seq.Date(from=as.Date(trainTimeSpan[1]),to=as.Date(trainTimeSpan[2]),by="day")%>%
    data.table(date=.,type=type)
  
  # 用户数据集
  trainSet<-fread(trainSetPath)%>%
    .[,":="(num=as.numeric(num),
            amount=as.numeric(amount),
            type_num=as.numeric(type_num))]
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
 

  corr<-cor(kk<-data.table(id=temp$id,
                           amount=temp$amount,
                           num=temp$num,
                           amount_per_num=temp$amount_per_num,
                           type_num=temp$type_num,
                           type_num_ratio=temp$type_num_ratio))
  # lr_type_num<-lm(type_num~id,temp)
  # *******************************1. 7日周期规律*************************************
  eval(parse(text=paste0("ts_week<-ts(temp$",item,",frequency=7)")))
  eval(parse(text=paste0("variables<-temp$",item)))
  stl_week<-stl(ts_week,s.window = "period")
  
  # 1.1 抽取7日周期规律数值
  weekly_seasonal<-data.table(dayOfWeek=temp$dayOfWeek,week_seasonal=as.numeric(stl_week$time.series[,"seasonal"]))%>%
    head(.,7)%T>%
    setkey(.,"dayOfWeek")
  # 1.2 更新原始数据集，将其拆分成7日周期+非7日周期
  stl_week_detail<-data.table(calendar,total=variables)%T>%
    setkey(.,"dayOfWeek")%>%
    .[weekly_seasonal,nomatch=NA,mult="all"]%>%
    .[,un_week_seasonal_remain:=total-week_seasonal]%>%
    arrange(.,date)
  
  # ********************************2. 30日周期规律************************************
  # 2.1 月份数据对齐，处理有31天的月份，将31号和30号的结果平均化赋给30号
  month_last_id<-stl_week_detail[day(date)==31,]$id
  feb_last_id <-
    stl_week_detail[month(date) == 2 &
                      day(date) >= 28,][((year(date) %% 400 == 0 |
                                            (year(date) %% 100 > 0 &
                                               year(date) %% 4 == 0)) &
                                           day(date) == 29) | year(date) %% 4 > 0,]$id
  stl_week_detail_new<-copy(stl_week_detail)
  
  for(i in month_last_id){
    month_last_unseansonal_remain<-stl_week_detail_new[id==i,]$un_week_seasonal_remain
    stl_week_detail_new[id==i-1,un_week_seasonal_remain:=(un_week_seasonal_remain+month_last_unseansonal_remain)/2]
  }
  
  # 2.2 处理2月份，将不足30日的部分线性插值补充，输出插值的数据bind到原始数据集
  feb_append<-data.table()
  for(i in feb_last_id){
    last_day<-day(stl_week_detail[id==i,]$date)
    datediff<-30-last_day
    march_first_unseasonal_remain<-stl_week_detail[id==i+1,]$un_week_seasonal_remain
    feb_last_unseasonal_remain<-stl_week_detail[id==i,]$un_week_seasonal_remain
    remain_diff<-march_first_unseasonal_remain-feb_last_unseasonal_remain
    appendtemp<-data.table()
    j=1
    while(j<=datediff){
      append<-data.table(id=i+j/(datediff+1),
                         date=stl_week_detail[id==i,]$date,
                         dayOfWeek=NA,
                         dayOfMonth=last_day+j,
                         isHoliday=NA,
                         Feast=NA,
                         Description=NA,
                         Back_Label=NA,
                         Before_Label=NA,
                         In_Label=NA,
                         total=NA,
                         week_seasonal=NA,
                         un_week_seasonal_remain=feb_last_unseasonal_remain+j/(datediff+1)*remain_diff)
      appendtemp<-rbind(appendtemp,append)
      j=j+1
    }
    feb_append<-rbind(feb_append,appendtemp)
  }
  stl_week_detail_new<-rbind(stl_week_detail_new,feb_append)
  
  # 2.3 去掉不齐的31日数据
  stl_week_detail_new<-stl_week_detail_new[!(id %in% month_last_id),]%>%
    arrange(.,id)
  
  # 2.4 30日周期规律输出，补充31号的猜测数据（这里采用的是与30日相同数据）
  ts_month<-ts(stl_week_detail_new$un_week_seasonal_remain,frequency = 30)
  monthly_seasonal<-
    tryCatch({
    stl_month<-stl(ts_month,s.window = "period")
    m<-data.table(dayOfMonth=stl_week_detail_new$dayOfMonth,
                                 month_seasonal=as.numeric(stl_month$time.series[,"seasonal"]))%>%
    head(.,30)},
    error=function(e)
      {
        m<-data.table(dayOfMonth=1:30,month_seasonal=0)
        return(m)
           })
  value<-monthly_seasonal[dayOfMonth==30,]$month_seasonal
  append_row_31<-data.table(dayOfMonth=31,month_seasonal=value)
  monthly_seasonal<-rbind(monthly_seasonal,append_row_31)%T>%
    setkey(.,dayOfMonth)
  
  # 2.5 更新原始数据集，将其拆分为7日周期+30日周期+非周期
  stl_week_month_detail<-copy(stl_week_detail)%T>%
    setkey(.,"dayOfMonth")%>%
    .[monthly_seasonal,nomatch=NA,mult="all"]%>%
    .[,no_seasonal_part:=un_week_seasonal_remain-month_seasonal]%>%
    .[,un_week_seasonal_remain:=NULL]%>%
    arrange(.,date)
  
  # ********************************3. 非周期部分线性趋势 ************************************
  # 3.1 去掉极大干扰项（长假日前后及异常点），然后简单拟合出趋势
  xx<-stl_week_month_detail[!(str_sub(In_Label,1,1) %in% c(3,7)|str_sub(Back_Label,1,1) %in% c(3,7)|str_sub(Before_Label,1,1) %in% c(3,7)),]
  high_value<-quantile(xx$no_seasonal_part,0.95)%>%
    as.numeric(.)
  low_value<-quantile(xx$no_seasonal_part,0.05)%>%
    as.numeric(.)
  xx_trimmed<-xx[no_seasonal_part>=low_value&no_seasonal_part<=high_value,]
  lr<-lm(no_seasonal_part~id,xx_trimmed)
  fitlist<-summary(lr)
  coef<-fitlist$coefficients%>%
    data.frame(.)%>%
    data.frame(coef=row.names(.),.)
  row.names(coef)<-NULL
  colnames(coef)<-c("coef","value","sd","t_value","p_value")
  
  # 3.2 获得线性趋势的截距和斜率，并更新原始数据集，将其拆分成7日周期+30日周期+线性趋势+线性残差
  intercept<-coef$value[1]
  slope<-coef$value[2]
  stl_week_month_linear_detail<-copy(stl_week_month_detail)%>%
    .[,linear_trend:=intercept+slope*id]%>%
    .[,linear_residuals:=no_seasonal_part-linear_trend]%>%
    .[,no_seasonal_part:=NULL]
  
  #********************************4. 线性残差的节日影响处理 ************************************
  # 4.1 节日影响训练集
  festivalTrain<-stl_week_month_linear_detail[!is.na(Back_Label)|!is.na(Before_Label)|!is.na(In_Label),]
  # 4.2 七日影响
  # 4.2.1 七日节前影响估计
  feast7_Before_Train<-festivalTrain[str_sub(Before_Label,1,2)=="7f",][
    ,c("Before_Label","date","linear_residuals")]
  #plot(str_sub(feast7_Before_Train$Before_Label,3,3),feast7_Before_Train$linear_residuals)
  colnames(feast7_Before_Train)<-c("label","date","value")
  # 4.2.2 七日节后影响估计
  feast7_Back_Train<-festivalTrain[str_sub(Back_Label,1,2)=="7b",][
    ,c("Back_Label","date","linear_residuals")]
  #plot(str_sub(feast7_Back_Train$Back_Label,3,3),feast7_Back_Train$linear_residuals)
  colnames(feast7_Back_Train)<-c("label","date","value")
  # 4.2.3 七日节中影响估计
  feast7_In_Train<-festivalTrain[str_sub(In_Label,1,2)=="7i"&month(date)==10,][
    ,c("In_Label","date","linear_residuals")]
  #plot(str_sub(feast7_In_Train$In_Label,3,3),feast7_In_Train$linear_residuals)
  colnames(feast7_In_Train)<-c("label","date","value")
  
  # 4.3 三日影响
  # 4.3.1 三日节前影响估计
  feast3_Before_Train<-festivalTrain[str_sub(Before_Label,1,2)=="3f"&
                                       (is.na(In_Label)|str_sub(In_Label,1,1)!=7)&
                                       (is.na(Back_Label)|str_sub(Back_Label,1,1)!=7),][
                                         ,c("Before_Label","date","linear_residuals")
                                       ]
  #plot(str_sub(feast3_Before_Train$Before_Label,3,3),feast3_Before_Train$linear_residuals)
  colnames(feast3_Before_Train)<-c("label","date","value")
  
  # 4.3.2 三日节后影响估计
  feast3_Back_Train<-festivalTrain[str_sub(Back_Label,1,2)=="3b"&
                                     (is.na(In_Label)|str_sub(In_Label,1,1)!=7)&
                                     (is.na(Before_Label)|str_sub(Before_Label,1,1)!=7),][
                                       ,c("Back_Label","date","linear_residuals")
                                     ]
  #plot(str_sub(feast3_Back_Train$Back_Label,3,3),feast3_Back_Train$linear_residuals)
  colnames(feast3_Back_Train)<-c("label","date","value")
  # 4.3.3 三日节中影响估计
  feast3_In_Train<-festivalTrain[str_sub(In_Label,1,2)=="3i"&
                                   (is.na(Back_Label)|str_sub(Back_Label,1,1)!=7)&
                                   (is.na(Before_Label)|str_sub(Before_Label,1,1)!=7),][
                                     ,c("In_Label","date","linear_residuals")
                                   ]
  #plot(str_sub(feast3_In_Train$In_Label,3,3),feast3_In_Train$linear_residuals)
  colnames(feast3_In_Train)<-c("label","date","value")
  
  # 4.4 二日影响,由于二日较为普遍，样本多，所以在平均时采用异常值trim
  # 4.4.1 二日节前影响估计
  feast2_Before_Train<-festivalTrain[str_sub(Before_Label,1,2)=="2f"&
                                       is.na(In_Label)&
                                       is.na(Back_Label),][
                                         ,c("Before_Label","date","linear_residuals")
                                       ]
  #plot(str_sub(feast2_Before_Train$Before_Label,3,3),feast2_Before_Train$linear_residuals)
  colnames(feast2_Before_Train)<-c("label","date","value")
  
  # 4.4.2 二日节后影响估计
  feast2_Back_Train<-festivalTrain[str_sub(Back_Label,1,2)=="2b"&
                                     is.na(In_Label)&
                                     is.na(Before_Label),][
                                       ,c("Back_Label","date","linear_residuals")
                                     ]
  #plot(str_sub(feast2_Back_Train$Back_Label,3,3),feast2_Back_Train$linear_residuals)
  colnames(feast2_Back_Train)<-c("label","date","value")
  # 4.4.3 二日节中影响估计
  feast2_In_Train<-festivalTrain[str_sub(In_Label,1,2)=="2i"&
                                   is.na(Back_Label)&
                                   is.na(Before_Label),][
                                     ,c("In_Label","date","linear_residuals")
                                   ]
  #plot(str_sub(feast2_In_Train$In_Label,3,3),feast2_In_Train$linear_residuals)
  colnames(feast2_In_Train)<-c("label","date","value")
  
  
  # 4.5 节日影响训练结果汇总
  festivalTrain<-rbindlist(list(
    feast2_Back_Train,
    feast2_Before_Train,
    feast2_In_Train,
    feast3_Back_Train,
    feast3_Before_Train,
    feast3_In_Train,
    feast7_Back_Train,
    feast7_Before_Train,
    feast7_In_Train))
  
  festivalInfluenceFun<-function(inputdate,festivalLabel){
    if(is.na(festivalLabel)|length(festivalLabel)==0){
      result=0
    }
    else{
      if(str_sub(festivalLabel,1,1)==2){
        result<-festivalTrain[label==festivalLabel&date<=inputdate,]$value%>%
          tail(.,10)%>%
          mean(.,trim=0.05)
        if(is.nan(result)){
          result<-festivalTrain[label==festivalLabel,]$value%>%
            tail(.,10)%>%
            mean(.,trim=0.05)
        }
      }
      else{
        result<-festivalTrain[label==festivalLabel&date<=inputdate,]$value%>%
          mean(.)
        if(is.nan(result)){
          result<-festivalTrain[label==festivalLabel,]$value%>%
            mean(.)
        }
      }
    }
    result
  }

  Before_influenceResult<-mapply(festivalInfluenceFun,
                                 stl_week_month_linear_detail$date,
                                 stl_week_month_linear_detail$Before_Label
                                 )
  Back_InfluenceResult<-mapply(festivalInfluenceFun,
                               stl_week_month_linear_detail$date,
                               stl_week_month_linear_detail$Back_Label
                               )
  In_influenceResult<-mapply(festivalInfluenceFun,
                             stl_week_month_linear_detail$date,
                             stl_week_month_linear_detail$In_Label
                             )
  
  
  festivalInfluence<-Before_influenceResult+Back_InfluenceResult+In_influenceResult
  festivalInfluence[is.nan(festivalInfluence)]<-0
  
  stl_week_month_linear_unfeast_detail<-data.table(stl_week_month_linear_detail,festivalInfluence)%>%
    .[,residuals:=linear_residuals-festivalInfluence]%>%
    .[,linear_residuals:=NULL]%>%
    setorder(.,date)
  
  
  # #********************************5. 投资体验金发放影响 ************************************
  
  
  estimate_result<-structure(list(weekly_seasonal = weekly_seasonal,
                                  monthly_seasonal = monthly_seasonal,
                                  trend = coef,
                                  festivalInfluence = festivalTrain,
                                  residuals = stl_week_month_linear_unfeast_detail$residuals,
                                  trainingSet = stl_week_month_linear_unfeast_detail,
                                  corr=corr),
                             class = "cash_flow_type_train")
  
  # #********************************6. 残差的利率影响处理 ************************************
  # #利率差异，由于没有利率相关规律，所以以下代码没有运行。
  # history_rate<-fread("F:\\Project\\20170220投资额度预测\\history_rate.csv")%>%
  #   .[,date:=as.Date(date)]%>%
  #   .[date>=as.Date("2016-02-01")&date<=as.Date("2017-02-20"),]%T>%
  #   setkey(.,date)
  # res_high_value<-quantile(stl_week_month_linear_unfeast_detail$residuals,0.95)%>%
  #   as.numeric(.)
  # res_low_value<-quantile(stl_week_month_linear_unfeast_detail$residuals,0.05)%>%
  #   as.numeric(.)
  # yy<-data.table(rate_diff=history_rate$ratediff_30ave,res=stl_week_month_linear_unfeast_detail$residuals)
  # yy_trimmed<-yy[between(yy$res,res_low_value,res_high_value),]
  
  return(estimate_result)
}