library(data.table)
library(magrittr)
library(bit64)
library(corrplot)
source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_train.R', encoding = 'UTF-8')
source('~/rstudio/!common/(fun)myqqplot.r')

trainTimeSpan=c("2016-03-01","2017-02-27")
redeem_trainSetPath<-"F:/Project/20170220投资额度预测/redeem_trainSet.csv"
invest_trainSetPath<-"F:/Project/20170220投资额度预测/invest_trainSet.csv"
calendarPath<-"F:/Project/20170220投资额度预测/calendar.csv"
quant300Path<-"F:/Project/20170220投资额度预测/399300.csv"

items=c("amount","amount_per_num","type_num_ratio")
user_types=c("S","A","B","C","D","newcomer","brush","freshman")
trainSetPaths=c("invest","redeem")

for(item in items){
  for(user_type in user_types){
    for(k in trainSetPaths){
      
      trainSetPath<-switch(k,"invest"=invest_trainSetPath,"redeem"=redeem_trainSetPath)
      quant300<-fread(quant300Path)%>%
        .[,date:=as.Date(date)]%>%
        .[between(date,as.Date(trainTimeSpan[1]),as.Date(trainTimeSpan[2]))]%>%
        .[,":="(Change_amount=as.numeric(Change_amount),
                Change_ratio=as.numeric(Change_ratio),
                Volume=Volume/1e+08,
                Amount=Amount/1e+08)]%T>%
        setkey(.,date)
      
      amount_train<-cash_flow_type_train(item = item,
                                         user_type = user_type,
                                         trainTimeSpan = trainTimeSpan,
                                         trainSetPath = trainSetPath,
                                         calendarPath = calendarPath)
      
      residuals<-amount_train$trainingSet%>%
        .[,c("week_seasonal","month_seasonal","linear_trend","festivalInfluence"):=NULL]
      
      kk<-residuals[quant300,on=c("date"="date"),nomatch=0,mult="all"]%>%
        .[,c("id",
             "residuals",
             "Close","High","Low","Open","Change_amount","Change_ratio","Volume","Amount")]
      
      corr<-cor(kk)
      png(filename=paste0("F:/Project/20170220投资额度预测/corr/",user_type,"_",item,"_",k,".png"),
          width = 800,
          height = 600,
          units = "px")
      corrplot(corr,method = "number",main=paste0(user_type,"_",item,"_",k))
      dev.off()
      
      colname<-colnames(kk)
      xx<-as.data.table(corr)%>%
        .[,2]%>%
        .[3:length(colname)]
      if(max(abs(xx))>0.2){
        variable<-colname[which(abs(xx)==max(abs(xx)))+2]
        
        eval(parse(text=paste0("lr<-lm(residuals~",variable,",kk)")))
        
        png(filename=paste0("F:/Project/20170220投资额度预测/corr/",user_type,"_",item,"_",k,"_lr_fitting.png"),
            width = 1200,
            height = 800,
            units = "px")
        eval(parse(text=paste0("plot(kk$",
                               variable,
                               ",kk$residuals,main=paste0(user_type,'_',item,'_',k,'_','residuals~',variable))")))
        abline(lr)
        dev.off()
        
        png(filename=paste0("F:/Project/20170220投资额度预测/corr/",user_type,"_",item,"_",k,"_qqplot.png"),
            width = 800,
            height = 600,
            units = "px")
        myqqplot(lr$residuals,main=paste(user_type,"_",item,"_",k,"_Residuals Q-Q Plot"))
        dev.off()
        
        
        zz<-data.table(ori_res=kk$residuals,lr_res=lr$residuals)
        
        iqr_diff<-IQR(lr$residuals)-IQR(kk$residuals)
        png(filename=paste0("F:/Project/20170220投资额度预测/corr/",user_type,"_",item,"_",k,"_boxplot.png"),
            width = 800,
            height = 600,
            units = "px")
        boxplot(zz,main=paste(user_type,"_",item,"_",k,"_residuals",round(iqr_diff)))
        dev.off()
        }
    }
  }
}







