
# 输出当前模型算法下的拟合效果（残差和参数相关性）

library(corrplot)
source('~/rstudio/cash_flow_predict/(fun)estimate.R', encoding = 'UTF-8')

types<-c("A","B","C","D","E","S","brush","freshman","newcomer")
items<-c("type_num_ratio","amount_per_num")
trainSetPath = "F:/Project/20170220投资额度预测/trainSet.csv"
calendarPath = "F:/Project/20170220投资额度预测/calendar.csv"
plotpath<-"F:/Project/20170220投资额度预测/estimate/"

for(type in types){
  for(item in items){
    eval(parse(text=paste0(type,"_",item,
                           "<-estimate(item='",item,
                           "',user_type='",type,
                           "',trainSetPath='",trainSetPath,
                           "',calendarPath='",calendarPath,"')")))
    png(filename=paste0(plotpath,type,"_",item,"_residuals.png"),width =1200,height=468,units ="px")
    eval(parse(text=paste0("boxplot(",type,"_",item,"$residuals)")))
    dev.off()
    png(filename=paste0(plotpath,type,"_",item,"_corr.png"),width =800,height=600,units ="px")
    eval(parse(text=paste0("corrplot(",type,"_",item,"$corr,method='number')")))
    dev.off()
  }
}


temp_type_num_ratio<-C_type_num_ratio$trainingSet
plot(temp_type_num_ratio$id,temp_type_num_ratio$total,xlim=c(250,300))

