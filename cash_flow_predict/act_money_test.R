library(corrplot)
library(data.table)
library(magrittr)
library(car)

temp<-S_type_num_ratio$trainingSet
act_money<-fread("F:\\Project\\20170220投资额度预测\\act_money.csv")%>%
  .[,part_log_day:=as.Date(part_log_day)]%>%
  .[,":="(used_num_ratio=ifelse(received_num==0,NA,used_num/received_num),
          used_amount_ratio=ifelse(received_amount==0,NA,used_amount/received_amount))]%T>%
  setkey(.,part_log_day)
  

act_money_temp<-temp[act_money,on=c("date"="part_log_day"),nomatch=NA,mult="all"]%>%
  .[!is.na(used_num_ratio),]%>%
  .[,":="(user_type=NULL,
          used_num_type_ratio=used_num/type_num,
          received_num_type_ratio=received_num/type_num)]

act_money_temp_train<-act_money_temp[is.na(Before_Label)&is.na(Back_Label)&is.na(In_Label),]

act_money_temp_train_corr<-copy(act_money_temp)%>%
  
  as.data.frame(act_money_temp_train)%>%
  .[,c(which(colnames(.)=="residuals"):dim(.)[2])]

corr<-cor(act_money_temp_train_corr)

corrplot(corr,method = "number")
scatterplotMatrix(act_money_temp_train_corr)
