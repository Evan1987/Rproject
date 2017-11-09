colname_treat<-function(df){
  library(stringr)
  colname<-colnames(df)
  colname_new<-c()
  for (i in 1:length(colname)) {
    if(str_detect(colname[i],"\\.")){
      xx<-str_split_fixed(colname[i],pattern = "\\.",n=2)
      temp<-xx[,2]
    }
    else{
      temp<-colname[i]
    }
    colname_new<-c(colname_new,temp)
  }
  colnames(df)<-colname_new
  return(df)
}