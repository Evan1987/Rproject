
#将某文件夹下的所有数据文件传输到MySQL数据库现有的数据表中，其中colnum_to_be_deleted 是数据文件中待删除的列序号向量，
#time_col为数据文件中的时间列，需要由字符串转成POSIXct时间格式。
bat_upload<-function(path,table_name,colnum_to_be_deleted=0,time_col=NULL,append=T,rownames=F){
  library(RODBC)
  library(data.table)
  source('~/RStudio/(fun)mysqlconn.R', echo=TRUE)
  conn<-mysqlconn("mysql_settings.csv")
  
  files<-dir(path)
  num<-length(files)
  a<-sqlQuery(conn,paste0("select * from ",table_name," limit 1"))
  colname<-colnames(a)
  for (i in 1:num) {
    temp<-fread(paste0(path,"\\",files[i]))
    temp_colname<-colnames(temp)
    if(!is.null(time_col)){
      for (j in 1:length(time_col)){
        name<-temp_colname[time_col][j]
        eval(parse(text=paste0("temp[",name,"=='(null)',",name,":=NA]")))
        eval(parse(text=paste0("temp[,",name,":=as.POSIXct(",name,")]")))
      }
    }
    if(colnum_to_be_deleted[1]!=0){
    temp[,eval(colnum_to_be_deleted):=NULL]
    }
    
    #保证待导入的数据列数与数据库字段数量一致，缺少数不得多于26.
    if(dim(temp)[2]<length(colname)){
      addnum<-length(colname)-dim(temp)[2]
      addcolumn<-letters[1:addnum]
      temp[,eval(addcolumn):=NA]
    }
    colnames(temp)<-colname
    print(paste(files[i],"index i=",i,"pretreated finished"))
    sqlSave(conn,dat = temp,tablename = table_name,append=append,rownames = rownames)
    print(paste(files[i],"index i=",i,"/",num,"upload successfully"))
  }
}