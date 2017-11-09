
#针对match主程序的金额放缩函数，根据adjust_ratio对相应资金序列的匹配额度目标进行调整，从而改变序列额度。
#user_data：用户资金序列信息（包含unmatched_regular_premium、unmatched_tPlus_premium、unmatched_current_premium）
#item：确定对哪个队列进行目标调整。
#breaks：调整的区间端点，可以精细化调整范围。
#adjust_ratio：调整的金额系数，根据breaks确定的区间做相应的放缩调整。
match_adjust<-function(user_data,item="tPlus",breaks,adjust_ratio){
  library(data.table)
  library(magrittr)
  
  if(length(adjust_ratio)!=length(breaks)-1){
    stop("the adjust value length must be smaller than length of breaks by 1 !")
  }
  
  colname<-switch(item,
                  "tPlus" = "unmatched_tPlus_premium",
                  "regular" = "unmatched_regular_premium",
                  "current" = "unmatched_current_premium")
  user_adjust_data<-copy(user_data)%>%
    .[,adjust_ratio:=eval(parse(text=paste0("cut(",colname,",breaks,right=F)")))]
  
  levels(user_adjust_data$adjust_ratio)<-adjust_ratio
  
  user_adjust_data[,adjust_ratio:=as.character(adjust_ratio)%>%as.numeric(.)]%>%
    .[,eval(parse(text=paste0(colname,"_formal:=",colname)))]%>%
    .[,eval(parse(text=paste0(colname,":=adjust_ratio*",colname)))]%T>%
    setkey(.,userid)
  
  return(user_adjust_data)
}