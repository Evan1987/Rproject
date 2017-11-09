iwrite<-function(x,path,method="write.csv",type=".csv",row.names=F){
  y<-deparse(substitute(x))
  filepath<-paste0(path,y,type)
  switch(method,
         "write.csv"=write.csv(x,filepath,row.names=row.names),
         "fwrite"=fwrite(x,filepath,row.names=row.names)
  )
}