user_action_plot<-function(num,user_record,path){
  library(data.table)
  library(varhandle)
  user_data<-user_record[[1]]
  user_type<-as.data.table(user_data[[1]])
  user_info<-as.data.table(user_data[[2]])
  setkey(user_info,userid)
  if(is.numeric(num)){
    plotuser<-sample(user_type[type==1,]$userid,num)
    plotid<-user_info[plotuser,nomatch=0,mult="all"]
  }
  else if(is.character(num)){
    plotuser<-num
    plotid<-user_info[plotuser,nomatch=0,mult="all"]
  } 
    user_num<-dim(plotid)[1]
    
  for (i in 1:user_num) {
      fileid<-plotid$id[i]
      reg_day<-as.Date(plotid$reg_day[i])
      png(filename=paste0(path,"\\",fileid,"-",plotid$userid[i],".png"),width =1200,height=468,units ="px")
      plot(user_data[[fileid]]$date,user_data[[fileid]]$premium,type="l",ylim=c(0,max(user_data[[fileid]]$premium)),
           main=paste(fileid,"-",plotid$userid[i],"存量变化"),ylab="存量",xlab="日期",lwd=3)
      lines(user_data[[fileid]]$date,user_data[[fileid]]$invest_amount,col="red",lwd=1)
      lines(user_data[[fileid]]$date,user_data[[fileid]]$redeem_amount,col="blue",lwd=1)
      arrows(reg_day,0,reg_day,10000,col="red",length = 0,lty=3)
      dev.off()
  }
}