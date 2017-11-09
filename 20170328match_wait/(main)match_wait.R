library(data.table)
library(dplyr)
library(tibble)
library(magrittr)

#calculate the possible summit between two snapshots
medium_estimate<-function(m1,m2,s1,s2,alpha,beta=0.1,type){
  if(is.na(type)){return(as.double(NA))}
  if(missing(alpha)){alpha=s1/(s1+s2)}
  if(missing(beta)){beta=0.1}
  if(type=="invest"){
    if(m2<m1){return(m1/s1)}
    else{
      delta_m=alpha*(m2-m1)
      return((m1+delta_m)/s1)
    }
  }
  if(type=="redeem"){
    if(m2+beta*(s1-s2)-m1<0){return(m1/s1)}
    else{
      delta_m=alpha*(m2-m1+beta*(s1-s2))
      return((m1+delta_m)/s1)
    }
  }
}

path<-"F:/Project/20170328用户匹配停留时间/"
rawdata<-fread(paste0(path,"MarchData.csv"))

rawdata[,":="(create_time=as.Date(create_time),
              log_create_date= as.Date(log_create_date),
              premium=matched_amount+outed_unmatched_amount+unouted_unmatched_amount)]
slimdata<-rawdata[premium>100,]%>%
  .[,ratio:=round(ratio,2)*100]


slimdata<-tbl_df(slimdata)%>%
  mutate(.,premium=matched_amount+outed_unmatched_amount+unouted_unmatched_amount)%>%
  select(.,-contains("unmatched"),-create_time)%>%
  arrange(.,userid,log_create_date)%>%
  group_by(.,userid)%>%
  mutate(.,premium_diff=round(premium-lag(premium)))%>%
  rownames_to_column(.,"id")%>%
  mutate(.,id=as.numeric(id))%>%
  mutate(.,remark=ifelse(premium_diff>=100,"invest",ifelse(premium_diff<=-100,"redeem",as.character(NA))))%>%
  left_join(.,mutate(.,id=id+1)%>%select(.,id,userid_copy=userid,m1=matched_amount,s1=premium),by="id")%>%
  group_by(.,id)%>%
  mutate(.,summit=(medium_estimate(m1=m1,m2=matched_amount,s1=s1,s2=premium,type=remark)*100)%>%round(.))%>%
  select(.,-c(m1,s1,userid_copy))%>%
  mutate(.,true_ratio=max(summit,ratio,na.rm=T))













