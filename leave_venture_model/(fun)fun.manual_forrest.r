
#根据维度列表创建不同的树，分别给出不同的结果
#df：全体数据
#trainning_list：训练集的用户集合
#testing_list：测试集的用户集
#value_col_list：创建不同的树采用的不同维度
#judge：由多棵树结果导出最终决策结果的运算法则

fun.manual_forest<-function(df,
                          trainning_list,
                          testing_list,
                          label_col,
                          value_col_list,
                          parms,
                          judge="or",
                          prune=F,...){
  library(rpart)
  library(rattle)
  library(ROCR)
  
  trainning_data<-df[userid%in%trainning_list,]
  testing_data<-df[userid%in%testing_list,]
  tree_num = length(value_col_list)
  model_list=list()
  for(i in 1:tree_num){
    formula<-paste0(label_col,"~",str_c(value_col_list[[i]],collapse = "+"))%>%
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
    testing_result<-{
      pred<-predict(fit,testing_data,type = "class")%>%as.character(.)%>%as.numeric(.)
      testing_data[,(paste0("tree_",i)):=pred]
    }
    model_list<-c(model_list,list(fit))
  }
  
  if(judge=="or"){
    testing_result<-testing_data[,pred:=rowSums(.SD,dims = 1),
                               .SDcols=names(testing_data)%>%.[which(str_detect(.,"tree_"))]]%>%
      .[,pred:=ifelse(pred>0,1,0)]
  }
  pred_performance<-{
    tmp<-prediction(testing_result$pred,testing_result$isleave)
    performance(tmp,"fpr","tpr")
  }
  structure(list(trainning_data = trainning_data,
                 testing_result = testing_result,
                 model_list = model_list,
                 n_tree = tree_num,
                 pred_performance = pred_performance
  ), class = "pred")
  
}