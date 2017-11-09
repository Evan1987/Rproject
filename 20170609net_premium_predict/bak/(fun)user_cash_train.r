
user_cash_train<-function(trainSetTotal,
                            calendar,
                            user_types,
                            trainTimeSpan=c("2016-02-01","2017-06-01"),
                            value.var = "net_premium"){
  library(data.table)
  library(magrittr)
  library(stringr)
  
  source('~/rstudio/20170609net_premium_predict/(fun)fun.feb_append.r', echo=TRUE)
  
  trainSet = trainSetTotal[user_type%in%user_types,]%T>%
    setnames(.,value.var,"value")%>%
    .[,.(value=sum(value)),by=log_day]%>%
    {
      trainTimeSpan<-as.Date(trainTimeSpan)
      .[calendar[between(log_day,trainTimeSpan[1],trainTimeSpan[2]),],on="log_day"]
    }%>%
    {
      vars = c("value")
      .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
    }%>%
    {
      types<-str_c(user_types,collapse="")
      .[,user_type:=types]
    }
  # *******************************1. 7日周期规律*************************************
  # 1.1 抽取7日周期规律数值
  weekly_seasonal<-{
    ts_week<-ts(trainSet$value,frequency = 7)
    stl_week<-stl(ts_week,s.window = "period")
    data.table(dayOfWeek=trainSet$dayOfWeek,
               week_seasonal=as.numeric(stl_week$time.series[,"seasonal"]))%>%
      head(.,7)%T>%
      setkey(.,"dayOfWeek")
  }
  # 1.2 更新原始数据集，将其拆分成7日周期+非7日周期
  stl_week_detail<-copy(trainSet)%>%
    .[weekly_seasonal,
      on="dayOfWeek",
      ":="(weekly_seasonal=i.week_seasonal,
           un_week_seasonal_remain=round(value-i.week_seasonal,4))]%T>%
    setkey(.,log_day)
  
  # *******************************2. 30日周期规律*************************************
  # 2.1 月份数据对齐，处理有31天的月份，将31号和30号的结果平均化赋给30号
  stl_week_detail_new<-copy(stl_week_detail)%>%
  {
    month_last_id<-stl_week_detail[dayOfMonth==31,]$id
    month_last_mean<-stl_week_detail[id%in%c(month_last_id,month_last_id-1),]%>%
      .[,.(id=min(id),mean_value=mean(un_week_seasonal_remain)),by=month(log_day)]
    
    .[month_last_mean,on="id",un_week_seasonal_remain:=i.mean_value]
  }
  # 2.2 处理2月份，将不足30日的部分线性插值补充，输出插值的数据bind到原始数据集并去掉不齐的31日数据
  if(nrow(stl_week_detail[month(log_day)==2,])>0){
    stl_week_detail_new<-{
      feb_last_id <-stl_week_detail[month(log_day)==2,][,.(last_id=last(id)),by=year(log_day)]$last_id
      
      month_feb_mean<-stl_week_detail[id%in%c(feb_last_id,feb_last_id+1),
                                      c("log_day","id","dayOfMonth","un_week_seasonal_remain")]%T>%
        setnames(.,c("un_week_seasonal_remain","id"),c("start_value","start_id"))%>%
        .[,end_value:=dplyr::lead(start_value,default = NA),by=year(log_day)]%>%
        .[!is.na(end_value),]%>%
        .[,":="(end_id=start_id+1,add_num=30-dayOfMonth)]
      
      feb_append<-data.table()
      for(i in 1:nrow(month_feb_mean)){
        temp<-month_feb_mean[i,]
        tmp<-fun.feb_append(add_num = temp$add_num,
                            start_id = temp$start_id,
                            end_id = temp$end_id,
                            start_value = temp$start_value,
                            end_value = temp$end_value,
                            dayOfMonth = temp$dayOfMonth)
        feb_append<-rbind(feb_append,tmp)
      }
      setnames(feb_append,"value","un_week_seasonal_remain")
      rbindlist(list(stl_week_detail_new,feb_append),fill = T)%>%.[dayOfMonth<=30,]%>%setorder(.,id)
    }
  }
  
  # 2.3 30日周期规律输出，补充31号的猜测数据（这里采用的是与30日相同数据）
  monthly_seasonal<-{
    ts_month<-ts(stl_week_detail_new$un_week_seasonal_remain,frequency = 30)
    monthly_seasonal_temp<-
      tryCatch({
        stl_month<-stl(ts_month,s.window = "period")
        m<-data.table(dayOfMonth=stl_week_detail_new$dayOfMonth,
                      month_seasonal=as.numeric(stl_month$time.series[,"seasonal"]))%>%
          .[,.(month_seasonal=first(month_seasonal)),by=dayOfMonth]
      },
      error=function(e)
      {
        m<-data.table(dayOfMonth=1:30,month_seasonal=0)
        return(m)
      })
    
    a<-monthly_seasonal_temp[dayOfMonth==30,]$month_seasonal
    append_row_31<-data.table(dayOfMonth=31,month_seasonal=a)
    rbind(monthly_seasonal_temp,append_row_31)%T>%
      setkey(.,dayOfMonth)
  }
  
  # 2.4 更新原始数据集，将其拆分为7日周期+30日周期+非周期
  stl_week_month_detail<-copy(stl_week_detail)%>%
    .[monthly_seasonal,
      on="dayOfMonth",
      ":="(monthly_seasonal=i.month_seasonal,
           no_seasonal_part=round(un_week_seasonal_remain-i.month_seasonal,4))]%>%
    .[,un_week_seasonal_remain:=NULL]%>%
    setorder(.,log_day)
  
  # ******************************** 3. 非周期部分线性趋势 ************************************
  # 3.1 去掉极大干扰项（长假日前后及异常点），然后简单拟合出趋势
  
  xx_trimmed<-{
    xx<-stl_week_month_detail[!(str_sub(In_Label,1,1) %in% c(3,7)|str_sub(Back_Label,1,1) %in% c(3,7)|str_sub(Before_Label,1,1) %in% c(3,7)),]
    high_value<-quantile(xx$no_seasonal_part,0.95)%>%
      as.numeric(.)
    low_value<-quantile(xx$no_seasonal_part,0.05)%>%
      as.numeric(.)
    xx[between(no_seasonal_part,low_value,high_value),]
  }
  
  coef<-{
    lr<-lm(no_seasonal_part~id,xx_trimmed)
    fitlist<-summary(lr)%>%
      .$coefficients%>%
      data.frame(.)%>%
      data.frame(coef=row.names(.),.)%T>%
      {
        row.names(.)<-NULL
        colnames(.)<-c("coef","value","sd","t_value","p_value")
      }
  }
  
  # 3.2 获得线性趋势的截距和斜率，并更新原始数据集，将其拆分成7日周期+30日周期+线性趋势+线性残差
  stl_week_month_linear_detail<-copy(stl_week_month_detail)%>%
  {
    intercept<-coef$value[1]
    slope<-coef$value[2]
    .[,linear_trend:=intercept+slope*id]%>%
      .[,linear_residuals:=no_seasonal_part-linear_trend]%>%
      .[,no_seasonal_part:=NULL]
  }
  
  #********************************4. 线性残差的节日影响处理 ************************************
  # 4.1 节日影响训练集
  festivalTrain<-stl_week_month_linear_detail[!is.na(Back_Label)|!is.na(Before_Label)|!is.na(In_Label),]
  # 4.2 七日影响
  # 4.2.1 七日节前影响估计
  feast7_Before_Train<-festivalTrain[str_sub(Before_Label,1,2)=="7f",
                                     c("Before_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.2.2 七日节后影响估计
  feast7_Back_Train<-festivalTrain[str_sub(Back_Label,1,2)=="7b",
                                   c("Back_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.2.3 七日节中影响估计
  feast7_In_Train<-festivalTrain[str_sub(In_Label,1,2)=="7i"&month(log_day)==10,
                                 c("In_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.3 三日影响
  # 4.3.1 三日节前影响估计
  feast3_Before_Train<-festivalTrain[str_sub(Before_Label,1,2)=="3f"&
                                       (is.na(In_Label)|str_sub(In_Label,1,1)!=7)&
                                       (is.na(Back_Label)|str_sub(Back_Label,1,1)!=7),
                                     c("Before_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.3.2 三日节后影响估计
  feast3_Back_Train<-festivalTrain[str_sub(Back_Label,1,2)=="3b"&
                                     (is.na(In_Label)|str_sub(In_Label,1,1)!=7)&
                                     (is.na(Before_Label)|str_sub(Before_Label,1,1)!=7),
                                   c("Back_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.3.3 三日节中影响估计
  feast3_In_Train<-festivalTrain[str_sub(In_Label,1,2)=="3i"&
                                   (is.na(Back_Label)|str_sub(Back_Label,1,1)!=7)&
                                   (is.na(Before_Label)|str_sub(Before_Label,1,1)!=7),
                                 c("In_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.4 二日影响,由于二日较为普遍，样本多，所以在平均时采用异常值trim
  # 4.4.1 二日节前影响估计
  feast2_Before_Train<-festivalTrain[str_sub(Before_Label,1,2)=="2f"&
                                       is.na(In_Label)&
                                       is.na(Back_Label),
                                     c("Before_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.4.2 二日节后影响估计
  feast2_Back_Train<-festivalTrain[str_sub(Back_Label,1,2)=="2b"&
                                     is.na(In_Label)&
                                     is.na(Before_Label),
                                   c("Back_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
  # 4.4.3 二日节中影响估计
  feast2_In_Train<-festivalTrain[str_sub(In_Label,1,2)=="2i"&
                                   is.na(Back_Label)&
                                   is.na(Before_Label),
                                 c("In_Label","log_day","linear_residuals")]%T>%
    setnames(.,names(.),c("label","date","value"))
  
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
    feast7_In_Train),fill = T)
  
  festivalInfluenceFun<-function(inputdate,festivalLabel){
    if(is.na(festivalLabel)|length(festivalLabel)==0){
      result=0
    }
    else{
      if(str_sub(festivalLabel,1,1)==2){
        result<-festivalTrain[label==festivalLabel & date<=inputdate,]$value%>%
          tail(.,10)%>%
          mean(.,trim=0.05)
        if(is.nan(result)){
          result<-festivalTrain[label==festivalLabel,]$value%>%
            tail(.,10)%>%
            mean(.,trim=0.05)
        }
      }
      else{
        result<-festivalTrain[label==festivalLabel & date<=inputdate,]$value%>%
          mean(.)
        if(is.nan(result)){
          result<-festivalTrain[label==festivalLabel,]$value%>%
            mean(.)
        }
      }
    }
    return(result)
  }
  
  festivalInfluence<-{
    Before_influenceResult<-mapply(festivalInfluenceFun,
                                   stl_week_month_linear_detail$log_day,
                                   stl_week_month_linear_detail$Before_Label)
    Back_InfluenceResult<-mapply(festivalInfluenceFun,
                                 stl_week_month_linear_detail$log_day,
                                 stl_week_month_linear_detail$Back_Label)
    In_influenceResult<-mapply(festivalInfluenceFun,
                               stl_week_month_linear_detail$log_day,
                               stl_week_month_linear_detail$In_Label)
    (Before_influenceResult+Back_InfluenceResult+In_influenceResult)%>%
      replace(.,is.nan(.),0)
  }
  
  stl_week_month_linear_unfeast_detail<-data.table(stl_week_month_linear_detail,festivalInfluence)%>%
    .[,unfeast_residuals:=round(linear_residuals-festivalInfluence,4)]%>%
    .[,linear_residuals:=NULL]%>%
    setorder(.,log_day)
  
  #******************************** 5. 残差ARCH效应处理************************************
  rt = stl_week_month_linear_unfeast_detail$unfeast_residuals
  at = log(rt+1e8)
  library(tseries)
  library(fGarch)
  library(FinTS)
  # 检查是否具有arch效应，原假设均为无ARCH效应
  # Box.test：前lag项自相关系数为0（检查ARCH效应需要对方差进行检验，因此为at^2）
  # ArchTest：均值方程的残差序列无异方差性
  # Box.test(at^2,type="Ljung-Box",lag = 12)
  if(adf.test(at)$p.value<=0.05 & ArchTest(at)$p.value<=0.05){
    source('~/rstudio/20170609net_premium_predict/(fun)user.garchInternalFit.r', echo=TRUE)
    paralist = {
      a = 0:3
      b = 1:6
      d = 0:3
      expand.grid(a,a,b,d)
    }
    num = nrow(paralist)
    aic = rep(0,num)
    for(i in 1:num){
      para = paralist[i,]
      m<-.garchFit(formula.mean = eval(parse(text=paste0("~arma(",para[1],",",para[2],")"))),
                   formula.var = eval(parse(text=paste0("~garch(",para[3],",",para[4],")"))),
                   series = at,
                   algorithm = "nlminb",
                   robust.cvar = F,
                   trace = F)
      aic[i] = m@fit$ics[1]
      print(paste(i,"finished"))
    }
    bestPar<-paralist[which(aic==min(aic)),]%>%as.matrix(.)%>%drop(.)
    m<-.garchFit(formula.mean = eval(parse(text=paste0("~arma(",bestPar[1],",",bestPar[2],")"))),
                 formula.var = eval(parse(text=paste0("~garch(",bestPar[3],",",bestPar[4],")"))),
                 series = at,
                 algorithm = "nlminb",
                 robust.cvar = F,
                 trace = F)
    fitted = exp(m@fitted)-1e8
  }else{
    m = list()
    fitted = 0
  }
  stl_week_month_linear_unfeast_noarch_detail<-copy(stl_week_month_linear_unfeast_detail)%>%
    .[,":="(archfitted = fitted,
            residuals = round(rt - fitted,4))]
  # ******************************** 5. 结果输出 ***********************************
  estimate_result<-structure(list(weekly_seasonal = weekly_seasonal,
                                  monthly_seasonal = monthly_seasonal,
                                  trend = coef,
                                  festivalInfluence = festivalTrain,
                                  archmodel = m,
                                  residuals = stl_week_month_linear_unfeast_noarch_detail$residuals,
                                  trainingSet = stl_week_month_linear_unfeast_noarch_detail
                                  ),
                             class = "cash_flow_type_train")
  return(estimate_result)
}












