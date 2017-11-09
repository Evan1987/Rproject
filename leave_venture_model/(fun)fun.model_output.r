

#根据维度提取sql和训练集、测试集范围输出训练模型和测试结果
#df：全体数据
#trainning_list：训练集的用户集合
#testing_list：测试集的用户集合


fun.model_output<-function(df,
                           trainning_list,
                           testing_list,
                           label_col,
                           value_col,
                           parms,
                           doplot=T,prune=F,...){
  library(rpart)
  library(rpart.plot)
  library(rattle)
  library(ROCR)
  
  trainning_data<-df[userid%in%trainning_list,]
  testing_data<-df[userid%in%testing_list,]
  
  formula<-paste0(label_col,"~",str_c(value_col,collapse = "+"))%>%
    parse(text=.)
  
  ct=rpart.control(...)
  
  fit<-rpart(eval(formula),data=trainning_data,method = "class",control = ct,parms = parms)
  if(prune){
    cp_threhold<-{
      cptable<-fit$cptable%>%as.data.table(.)%>%.[,xx:=xerror+xstd]
      lo<-cptable[which(xerror==min(xerror)),]$xx
      max(cptable[xerror<=lo,]$CP)
    }
    fit<-prune(fit,cp=cp_threhold)
  }
  if(doplot){fancyRpartPlot(fit)}
  
  testing_result<-{
    pred<-predict(fit,testing_data,type = "class")%>%as.character(.)%>%as.numeric(.)
    testing_data[,predict_result:=pred]
  }
  
  pred_performance<-{
    tmp<-prediction(testing_result$predict_result,testing_result$isleave)
    performance(tmp,"fpr","tpr")
    }
  
  structure(list(trainning_data = trainning_data,
                 testing_result = testing_result,
                 model = fit,
                 pred_performance = pred_performance
                 ), class = "pred")
  
}