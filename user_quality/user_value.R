library(data.table)
source('~/RStudio/(fun)cut_label.R', encoding = 'UTF-8', echo=TRUE)
freq<-data.table(freq=c(0:7))
for (i in 1207:1213) {
  command1<-paste0("temp<-fread('D:/Project/20161212用户质量评价/价值/2016",i,"留存用户价值.csv')")
  eval(parse(text=command1))
  colnames(temp)<-c("userid","mean_premium")
  temp[mean_premium=="NULL",mean_premium:=0]
  temp$mean_premium<-as.numeric(temp$mean_premium)
  
  temp[,log_mean_premium:=log10(mean_premium+0.01)]
  command2<-paste0("raw_data_",i,"<-temp")
  eval(parse(text=command2))
  x<-cut_label(temp$log_mean_premium,level = 5)
  table(x$result)
  10^(x$breaks_node)
  
}
rm(temp)
