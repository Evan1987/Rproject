library(data.table)
library(magrittr)
library(readr)
library(stringr)
library(rpart)
library(rpart.plot)
library(rattle)


source('~/rstudio/!custom/(fun)iwrite.r')
source('~/rstudio/!custom/(fun)fun.elbow_point.r', encoding = 'UTF-8')
path = "F:/Project/20170515用户app访问数据/"

lastPageStayTime=7
slimdata<-read_csv(paste0(path,"slimdata.csv"))%>%
  as.data.table(.)%>%
  .[,stay_time_true:=ifelse(is.na(isEnd),stay_time,lastPageStayTime)]

slim_user_info<-read_csv(paste0(path,"my_user_info.csv"))%>%
  as.data.table(.)%T>%
  setkey(.,userid)%>%{
    userList<-unique(slimdata$userid)
    .[,isin:=ifelse(userid%in%userList,1,0)]
  }%>%
  .[isin==1,]%>%
  .[,":="(isLeaveInOpen=ifelse(is.na(open_time),1,0),
          isLeaveInInvest=ifelse(is.na(invest1st_time),1,0))]

slimdata<-slimdata


user_summary<-slimdata[,.(total_sec=sum(stay_time_true),total_freq=.N),by=.(userid,module)]%>%
  .[module!="exit",]%>%
  {
    tmp<-slimdata[module!="exit",][,.(total_sec_all=sum(stay_time_true),total_freq_all=.N),by=userid]
    .[tmp,on="userid",":="(time_ratio=round(total_sec/total_sec_all,4),
                           freq_ratio=round(total_freq/total_freq_all,4))]
  }

user_summary_wide<-dcast(user_summary,userid~module,,value.var = c("time_ratio","freq_ratio"),fill=0)%>%
  .[slim_user_info[,c("userid","isLeaveInOpen","isLeaveInInvest")],
    on="userid",
    nomatch=0]%>%
    {
      tmp<-tmp<-slimdata[module!="exit",][,.(total_sec_all=sum(stay_time_true),total_freq_all=.N),by=userid]
      .[tmp,on="userid",":="(total_sec_all=i.total_sec_all,total_freq_all=i.total_freq_all)]
    }


label_col="isLeaveInInvest"
value_col=names(user_summary_wide)%>%.[which(str_detect(.,"_"))]%>%.[which(!str_detect(.,"buy")&
                                                                           !str_detect(.,"Password")&
                                                                           !str_detect(.,"sec_all")&
                                                                           !str_detect(.,"share")&
                                                                           !str_detect(.,"regular")&
                                                                           !str_detect(.,"account")&
                                                                           !str_detect(.,"freq_all")&
                                                                           !str_detect(.,"homepage"))]
formula=paste0(label_col,"~",str_c(value_col,collapse = "+"))%>%parse(text=.)
error_cost<-matrix(c(0,1,2,0),nrow = 2)
parms<-list(split="gini",loss=error_cost)

fit<-rpart(data=user_summary_wide,formula = eval(formula),method = "class",parms=parms)
cp_threhold<-{
  cptable<-fit$cptable%>%as.data.table(.)%>%.[,xx:=xerror+xstd]
  lo<-cptable[which(xerror==min(xerror)),]$xx
  max(cptable[xerror<=lo,]$CP)
}
fit<-prune(fit,cp=cp_threhold)
fancyRpartPlot(fit)


