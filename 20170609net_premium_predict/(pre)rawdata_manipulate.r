library(data.table)
library(magrittr)
library(readr)
path="F:/Project/20170609日存量增量预测/"

invest_record<-read_csv(paste0(path,"invest_record.csv"))%>%as.data.table(.)%>%
  .[userid!="3f65d2a472304ce994c7d009160d1362",]
redeem_record<-read_csv(paste0(path,"redeem_record.csv"))%>%as.data.table(.)%>%
  .[userid!="3f65d2a472304ce994c7d009160d1362",]
write.csv(redeem_record,paste0(path,"redeem_record.csv"),row.names = F)

invest_record<-read_csv(paste0(path,"invest_record.csv"))%>%as.data.table(.)%T>%
  setnames(.,"amount","premium_change")
redeem_record<-read_csv(paste0(path,"redeem_record.csv"))%>%as.data.table(.)%>%
  .[,premium_change:=-1*amount]%>%
  .[,amount:=NULL]


invest_summary<-invest_record[,.(amount=sum(premium_change)),by=.(log_day,mean_premium)]%T>%
  setnames(.,"mean_premium","user_type")

redeem_summary<-redeem_record[,.(amount=-1*sum(premium_change)),by=.(log_day,mean_premium)]%T>%
  setnames(.,"mean_premium","user_type")

write.csv(invest_summary,paste0(path,"invest_summary.csv"),row.names = F)
write.csv(redeem_summary,paste0(path,"redeem_summary.csv"),row.names = F)


user_info<-{
  vars<-c("userid","mean_premium","log_day")
  rbind(invest_record[,.SD,.SDcols=vars],redeem_record[,.SD,.SDcols=vars])
}%>%
  unique(.)

notag_user_info<-user_info[mean_premium=="notag",]

net_premium<-rbind(invest_record,redeem_record)%>%
  .[,.(net_premium=sum(premium_change)),by=.(log_day,userid)]%>%
  .[user_info,on=c("log_day","userid"),mean_premium:=i.mean_premium]

write.csv(net_premium,paste0(path,"net_premium.csv"),row.names = F)

net_premium<-read_csv(paste0(path,"net_premium.csv"))%>%as.data.table(.)%T>%setkey(.,log_day,userid)


############################# 最终训练数据 ################################
net_premium_summary<-net_premium[,.(net_premium=sum(net_premium),user_num=.N),by=.(log_day,mean_premium)]%>%
{
  start=min(.$log_day)
  end=max(.$log_day)
  .[,.SD[J(log_day=seq.Date(from = start,to=end,by=1)),on="log_day"],by=mean_premium]
}%>%
{
  vars = c("net_premium","user_num")
  .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
}%T>%setnames(.,"mean_premium","user_type")

write.csv(net_premium_summary,paste0(path,"trainSet.csv"),row.names = F)
###########################################################################

library(ggplot2)

ggplot(data=net_premium[mean_premium!="notag",],
       mapping = aes(log_day,net_premium,colour=mean_premium))+
geom_line()+ylim(-1e+7,1e+7)

types = unique(net_premium_summary$mean_premium)
for(i in types){
  target_data<-net_premium_summary[mean_premium==i,]
  png(filename = paste0(path,"analysis/",i,".png"),width = 2000,height = 800)

  plot(target_data$log_day,target_data$net_premium,type = "l",ylim = c(-1e+7,1e+7))
  dev.off()
}

calendar<-read_csv(paste0(path,"calendar.csv"),
                   locale = locale(encoding = "GBK")
                   )%>%as.data.table(.)%>%.[,log_day:=as.Date(log_day)]
################## type in S mining ###################
trainingSet <- net_premium_summary[mean_premium%in%"S",]%T>%
  setorder(.,log_day)%>%
  .[,mean_premium:=NULL]%>%
  .[CJ(log_day=seq.Date(from=min(log_day),to=max(log_day),by=1)),on="log_day"]%>%
  .[,net_premium:=replace(net_premium,is.na(net_premium),0)]























