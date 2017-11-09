library(methods)
library(DBI)
library(rJava)
library(RJDBC)
library(mailR)
library(rjson)
library(xlsxjars)
library(xlsx)
fun.sql <- function(sqlurl) {
  tryCatch({
  json_setting <- fromJSON(paste(readLines("/home/zhanghuayang/settings/server_setting.json"), collapse=""))
  drv2<- JDBC('org.apache.hive.jdbc.HiveDriver',list.files("/publicfile/hive/",pattern = "jar$",full.names=TRUE,recursive=TRUE))
  conn2<- dbConnect(drv2, json_setting[[1]]$jdbc,json_setting[[1]]$account,json_setting[[1]]$passwd)
  print(sqlurl)
  bb<-dbGetQuery(conn2,sqlurl)
  print("query success")
  dbDisconnect(conn2)
  return(bb)},
  error=function(e){
    cat("程序报错",conditionMessage(e),"\n\n")
    dbDisconnect(conn2)
    return("error")
  },
  finally = {
    dbDisconnect(conn2)
  })
}

fun.mysqlsql <- function(sqlurl) {
  tryCatch({
    json_setting <- fromJSON(paste(readLines("/home/zhanghuayang/settings/server_setting.json"), collapse=""))
    drv2<- JDBC('com.mysql.jdbc.Driver',list.files("/publicfile/mysql/",pattern = "jar$",full.names=TRUE,recursive=TRUE))
    conn2<- dbConnect(drv2, json_setting[[3]]$jdbc,json_setting[[3]]$account,json_setting[[3]]$passwd)
    print(sqlurl)
    bb<-dbGetQuery(conn2,sqlurl)
    print("query success")
    dbDisconnect(conn2)
    return(bb)},
    error=function(e){
      cat("程序报错",conditionMessage(e),"\n\n")
      dbDisconnect(conn2)
      return("error")
    },
    finally = {
      dbDisconnect(conn2)
    })
}

fun.writesql <- function(data,tablename) {
  tryCatch({
    json_setting <- fromJSON(paste(readLines("/home/zhanghuayang/settings/server_setting.json"), collapse=""))
    drv2<- JDBC('com.mysql.jdbc.Driver',list.files("/publicfile/mysql/",pattern = "jar$",full.names=TRUE,recursive=TRUE))
    conn2<- dbConnect(drv2, json_setting[[3]]$jdbc,json_setting[[3]]$account,json_setting[[3]]$passwd)
    dbWriteTable(conn2,tablename,data,overwrite=FALSE,append=TRUE)
    print("write success")
    dbDisconnect(conn2)},
    error=function(e){
      cat("程序报错",conditionMessage(e),"\n\n")
      dbDisconnect(conn2)
      return("error")
    },
    finally = {
      dbDisconnect(conn2)
    })
}

fun.mail_success <- function(senduser,subject,content,filename) {
    json_setting <- fromJSON(paste(readLines("/home/zhanghuayang/settings/server_setting.json"), collapse=""))
    send.mail(
    from=json_setting[[2]]$username,
    to=senduser,
    subject = subject,
    body=content,
    smtp=list(host.name="smtp.exmail.qq.com",
              port=465,
              user.name = json_setting[[2]]$username, 
              passwd = json_setting[[2]]$passwd, ssl = TRUE),
    attach.files = filename,
    authenticate = TRUE,
    send = TRUE,
    encoding ="utf-8"
  )
  print("send_data_mail success")
}

fun.mail_unsuccess <- function(senduser,subject,content) {
  json_setting <- fromJSON(paste(readLines("/home/zhanghuayang/settings/server_setting.json"), collapse=""))
  send.mail(
    from=json_setting[[2]]$username,
    to=senduser,
    subject = subject,
    body=content,
    smtp=list(host.name="smtp.exmail.qq.com",
              port=465,
              user.name = json_setting[[2]]$username, 
              passwd = json_setting[[2]]$passwd, ssl = TRUE),
    authenticate = TRUE,
    send = TRUE,
    encoding ="utf-8"
  )
  print("send_error_mail success")
}

fun.writexlsx <- function(filepath,database,startrow,startcol) {
  a<-loadWorkbook(filepath)
  b<-getSheets(a)
  c<-b[[1]]
  addDataFrame(database,c,col.names = F,row.names = F,startRow = startrow,startColumn = startcol)
  saveWorkbook(a,filepath)
  print("writexlsx success")
}

fun.writexlsx2 <- function(filepath,database,sheetindex,startrow,startcol) {
  a<-loadWorkbook(filepath)
  b<-getSheets(a)
  c<-b[[sheetindex]]
  addDataFrame(database,c,col.names = F,row.names = F,startRow = startrow,startColumn = startcol)
  saveWorkbook(a,filepath)
  print("writexlsx success")
}
