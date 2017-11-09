# The bat treatment of match_log_mining.
# The rawdata must be from the hive table(jlc_asset.ast_user_match_priority).
# The rawdata must include following columns:
#"userid","type","log_create_time","matched_amount","outed_unmatched_amount","unouted_unmatched_amount"

asset_match_log_mining_bat<-function(rawdata,doplot=F,plotpath=NULL){
  library(data.table)
  library(magrittr)
  library(tcltk)
  library(plotrix)
  source('~/rstudio/user_asset_match_efficiency/(fun)user_match_log_mining.R', encoding = 'UTF-8')
  colnames(rawdata)<-c("userid","type","ratio","matched_amount","outed_unmatched_amount",
                       "unouted_unmatched_amount","create_time","log_create_date")
  
  rawdata[,":="(create_time=as.Date(create_time),
                log_create_date= as.Date(log_create_date),
                premium=matched_amount+outed_unmatched_amount+unouted_unmatched_amount)]%T>%
    setkey(.,log_create_date)
  #去掉存量很少的匹配记录
  slimdata<-rawdata[premium>50,]%>%
    .[,ratio:=round(ratio,2)*100]%T>%
    setkey(.,userid)
  
  #用户ID集合，循环前准备
  users<-slimdata$userid[!duplicated(slimdata$userid)]
  num<-length(users)
  result<-data.table()
  pb <- tkProgressBar("进度","已完成 %", 0, 100) 
  
  for(i in 1:num){
    user<-users[i]
    userdata<-slimdata[userid==user,]
    userlog<-user_match_log_mining(userdata)
    result<-rbind(result,userlog)
    if(doplot){
      premiummax<-max(userdata$premium)
      png(filename=paste0(plotpath,"\\slimdata\\",i,"-",user,".png"),width =1200,height=468,units ="px")
      twoord.plot(lx=userdata$log_create_date,ly=userdata$ratio,rx=userdata$log_create_date,ry=userdata$premium,
                  type="l",lylim=c(0,1),rylim=c(0,premiummax),xlab = "Date",ylab = "ratio",rylab = "premium",
                  lwd=3,lcol = "black",rcol = "red",main=paste(i,"-",user,"匹配比例及存量变化"))
      dev.off()
    }
    info<- sprintf("已完成 %d%%", round(i*100/num))  
    setTkProgressBar(pb, i*100/num, sprintf("进度 (%s)", info),info)
  }
  result
}














