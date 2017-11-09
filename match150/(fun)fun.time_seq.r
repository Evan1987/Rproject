
# example:Dseq=c("2017-03-15","2017-03-16"),HMSseq=c(7:12)

fun.time_seq<-function(Dseq,HMSseq){
  library(magrittr)
  HMSseq<-do.call(paste0,expand.grid(HMSseq,":00:00"))
  timeseq<-do.call(paste,expand.grid(Dseq,HMSseq,stringsAsFactors = F))%>%
    strftime(.,"%Y/%m/%d %H:%M:%S")%>%as.POSIXct(.)
  
}