library(RODBC)
library(data.table)
conn<-odbcConnect("MySQL",uid='root',pwd='871226')
setwd("D:/csv to be uploaded")
files<-dir()
num<-length(files)
for (i in 14:num) {
  temp<-fread(files[i])
  temp[,6:=NULL]
  colnames(temp)<-c("id","userid","amount","create_time","platform")
  temp[,create_time:=as.POSIXct(create_time)]
  sqlSave(conn,dat = temp,tablename = "invest_record",append=T,rownames = F)
  print(paste(files[i],"upload successfully"))
}