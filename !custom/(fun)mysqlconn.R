mysqlconn<-function(file){
  library(RODBC)
  settings<-read.csv(file,stringsAsFactors = F)
  conn<-odbcConnect(settings$dsn,uid=settings$uid,pwd=settings$pwd,DBMSencoding = "UTF-8")
  return(conn)
}