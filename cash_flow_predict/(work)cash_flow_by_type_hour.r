library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)

rawdata<-fread(file.choose())
rawdata<-rawdata[,create_date:=as.Date(create_date)]%>%
  .[invest_amount>0|redeem_amount>0,]


rawdata_summary<-rawdata[,.(invest_num_total=sum(invest_num),
                            redeem_num_total=sum(redeem_num),
                            invest_amount_total=sum(invest_amount),
                            redeem_amount_total=sum(redeem_amount)),
                         by=.(create_date,create_hour)]


mydata<-rawdata[rawdata_summary,
                on=c("create_date"="create_date","create_hour"="create_hour"),
                nomatch=NA,
                mult="all"]%>%
  .[,":="(invest_num_ratio=ifelse(invest_num_total==0,0,invest_num/invest_num_total),
          redeem_num_ratio=ifelse(redeem_num_total==0,0,redeem_num/redeem_num_total),
          invest_amount_ratio=ifelse(invest_amount_total==0,0,invest_amount/invest_amount_total),
          redeem_amount_ratio=ifelse(redeem_amount_total==0,0,redeem_amount/redeem_amount_total))]%>%
  .[mean_premium %in% c("S","A","B","C","brush","newcomer","freshman"),]


daylines<-unique(rawdata$create_date)
vars<-c("invest_amount",
        "redeem_amount",
        "invest_num",
        "redeem_num",
        "invest_num_ratio",
        "redeem_num_ratio",
        "invest_amount_ratio",
        "redeem_amount_ratio")

for(i in 1:length(daylines)){
  temp<-mydata[create_date==daylines[i],]
  for(var in vars){
    
    p<-qplot(x=create_hour,
          eval(parse(text=paste0("y=",var))),
          data=temp,
          geom = "line",
          ylab = var,
          colour=mean_premium,main = paste(daylines[i],"_",var))
    ggsave(filename = paste0(daylines[i],"_",var,".png"),
           device = "png",
           path="F:/cash_flow_by_type_hour/",
           width = 20,
           height = 8,
           units = "cm",
           plot=p)
  }
}


