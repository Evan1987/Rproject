#用于用户未来的存量估计：	uservector用户id的向量，
#							plot决定是否作图，
#							path图的输出地址，
#							latesdate预测原点（供给给user_action函数使用，前提是需要确保数据库时间范围真的包含latesdate），
#							forecast_step预测步长，就是基于预测原点向后预测几天。
user_premium_predict<-function(uservector,doplot=F,path=NULL,latestdate=NULL,forecast_step=1){
  library(data.table)
  library(tseries)
  library(forecast)
  library(fUnitRoots)
  source('~/RStudio/(fun)stringbind.R', echo=TRUE)
  source('~/RStudio/(fun)user_action.R', encoding = 'UTF-8')
  source('~/RStudio/(fun)mysqlconn.R')
  conn<-mysqlconn("mysql_settings.csv")

  #通过user_action函数获得用户的行为数据
  user_data<-user_action(uservector=uservector,latestdate=latestdate)
  o1<-user_data[[1]]
  user_type<-as.data.table(o1[[1]])
  user_info<-as.data.table(o1[[2]])
  
  print("User action info query succeed! Do the Arima now!")
  
  #对每个用户进行自动Arima拟合
  rownum<-dim(user_info)[1]
  Inv_model_info<-list(user_type,user_info)
  pred<-data.frame()
  for (i in 3:(rownum+2)) {
    tempdata<-o1[[i]]
    userid<-user_info[id==i,]$userid
    invest1st_day<-user_info[id==i,]$invest1st_day
    latestdate<-max(tempdata$date)			#设置预测原点
    x<-tempdata[date>=invest1st_day,]$premium
    span_days<-length(x)
    remarks<-ifelse(span_days>30,rep("normal",forecast_step),rep("unpredictable",forecast_step))
    
    if(remarks=="unpredictable"){
      df<-data.frame(  id=i,					                   #用户顺序id
                       userid=userid,                    #用户id
                       invest1st_day=invest1st_day,      #首投日期
                       actual_base=ifelse(span_days==0,0,x[span_days]),			   #预测原点
                       Inv_base=NA,
                       pred_date=NA,		           #预测的时间向量
                       actual_mean=NA,		 #预测的期望值
                       Inv_mean=NA,			           #正态转换后的期望值
                       Inv_se=NA,				                 #正态转换后的标准差
                       lambda=NA,				                 #正态转换的参数（BoxCox）
                       method=NA,                 #Arima模型
                       mean_adj=NA,                #预测值是否出现NA需进行调整估计
                       remarks=remarks)
      
    }
    else{
    x[x<=0]<-0.01
    l<-BoxCox.lambda(x,method = "loglik")
    m<-auto.arima(x,ic="aic",lambda = l,d=1)
    f<-forecast(m,forecast_step)
    
    #为防止NA值出现，先做转换，只对转换后的数据进行预测
    x1<-BoxCox(x,l)
    m1<-auto.arima(x1,ic="aic",d=1)
    f1<-forecast(m1,forecast_step)
    mean_adj<-ifelse(is.na(f$mean),1,0)
    #因为forecast不输出预测标准差，所以计算正态转换后的标准差
    f1_mean<-as.numeric(f1$mean)
    f1_upper<-f1$upper[,1]
    f1_lower<-f1$lower[,1]
    f1_level<-f1$level[1]
    se<-(f1_mean-f1_lower)/qnorm(0.5*(1+f1_level/100))
      
    #设置预测时间向量
    pred_date<-seq.Date(from=latestdate+1,by='day',length.out=forecast_step)
    #预测值一览
    aa<-as.numeric(f$mean)  
    df<-data.frame(  id=i,					                   #用户顺序id
                     userid=userid,                    #用户id
                     invest1st_day=invest1st_day,      #首投日期
                     actual_base=x[span_days],			   #预测原点
					           Inv_base=x1[span_days],			     #正态转换的预测原点
					           pred_date=pred_date,		           #预测的时间向量
                     actual_mean=ifelse(aa<0,0,aa),		 #预测的期望值
                     Inv_mean=f1_mean,			           #正态转换后的期望值
                     Inv_se=se,				                 #正态转换后的标准差
                     lambda=l,				                 #正态转换的参数（BoxCox）
                     method=f1$method,                 #Arima模型
                     mean_adj=mean_adj,                #预测值是否出现NA需进行调整估计
                     remarks=remarks)			             #备注信息 
    Inv_model_info<-c(Inv_model_info,list(f1))
    }
    pred<-rbind(pred,df)
    
  
	#根据需要画出用户预测图像
	if(doplot){
      png(filename=paste0(path,"\\",i,"-",userid,"-",remarks,".png"),width =1000,height=800,units ="px")
      plot(f,main=paste0(i,"-(",l,")-",userid,"-",remarks[1]))
      dev.off()
    }
  print(paste(i-2,"/",rownum,"arima fit finished!"))
  }
  row.names(pred)<-NULL
  #返回一个模块，由预测值和模型信息组成
  return(structure(list(result=pred,Inv_model_info=Inv_model_info),class="user_prediction"))
}