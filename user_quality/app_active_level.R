library(data.table)
source('~/RStudio/(fun)cut_label.R', encoding = 'UTF-8', echo=TRUE)
freq<-data.table(freq=c(0:7))
for (i in 1207:1213) {
  command1<-paste0("temp<-fread('D:/Project/20161212用户质量评价/活跃度/2016",i,"留存用户活跃度.csv')")
  eval(parse(text=command1))
  colnames(temp)<-c("userid","frequency")
  temp[frequency=="NULL",frequency:=0]
  temp$frequency<-as.numeric(temp$frequency)
  
  command2<-paste0("raw_data_",i,"<-temp")
  eval(parse(text=command2))
  aa<-as.data.frame(table(temp$frequency))
  colnames(aa)<-c("num","freq")
  
  
  command3<-paste0("freq[,day",i,":=aa$freq]")
  eval(parse(text=command3))
}
rm(temp)

x<-cut_label(raw_data_1208$frequency,3)