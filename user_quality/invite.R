library(data.table)

raw_data<-fread("D:\\Project\\20161212用户质量评价\\邀请\\20161214用户邀请评价.csv")
raw_data[,dim(raw_data)[2]:=NULL]
colnames(raw_data)<-c("date","userid","invite_quantity","invite_quality")


raw_data[,log_invite_quantity:=log10(invite_quantity)]

raw_data_1<-raw_data[invite_quality>0,]
raw_data_1[,log_invite_quality:=log10(invite_quality)]


x<-cut_label(raw_data$log_invite_quantity,level = 4)
y<-cut_label(raw_data_1$log_invite_quality,level = 4)
