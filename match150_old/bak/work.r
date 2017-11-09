library(data.table)
library(magrittr)
#rawdata为完成智能投匹配的资产信息（status=600）
rawdata<-fread("F:/Project/20170315资产匹配穿透150人调研/20170314ast_distribution.csv")
rawdata<-rawdata[,":="(create_date=as.Date(create_date),
                       end_date=as.Date(end_date))]


amount_quantile<-quantile(rawdata$corpusamount,probs = seq(0,1,0.1))
breaks<-c(0,1e+05,5e+05,10e+05,20e+05,40e+05,100e+05,400e+05)
rawdata[,amountlabel:=cut(corpusamount,breaks)]


match_user_num_quantile<-quantile(rawdata$matched_user_num,probs = seq(0,1,0.1))
user_breaks<-c(0,150,500,1000,2000,5000,40000)

rawdata[,matchlabel:=cut(matched_user_num,user_breaks)]
amount_table<-table(cut(rawdata$corpusamount,breaks))
barplot(amount_table,width = 10)



