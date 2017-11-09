library(data.table)
library(magrittr)
library(stringr)

source('~/rstudio/match150/20170517按利率进行资产置换/(fun)trans_temp.r', encoding = 'UTF-8')
path="F:/Project/20170315资产匹配穿透150人调研/20170517按利率进行资产置换/match_result/"

rate_tPlus=6.1
rate_cur=5.5
plain_rate=2.5
target_ratio=0.92
n=200

user_list<-fread(paste0(path,"user_list.csv"))
cash_list<-fread(paste0(path,"cash_list.csv"))
match_record<-fread(paste0(path,"match_record.csv"))%>%.[,isfix:=0]
asset_data<-fread(paste0(path,"asset_data.csv"))
asset_list<-fread(paste0(path,"asset_list.csv"))

# 计算用户实际利率信息（整体及分产品）
fun.user_rate_actual<-function(user_list,asset_data,match_record,plain_rate){
  user_rate_actual<-copy(user_list)%>%
  {
    user_match_result<-{
      match_record[asset_data[,c("id","rate")],on=c("asset_id"="id"),nomatch=0]%>%
        .[,.(matched_amount=sum(amount),annual_interest=sum(amount*rate/100)),by=.(userid,type)]%>%
        dcast(.,userid~type,value.var=c("matched_amount","annual_interest"),fill=0)%>%
        {
          vars = names(.)%>%.[which(str_detect(.,"interest"))]
          .[,total_annual_interest:=rowSums(.SD,dims = 1),.SDcols=vars]
        }
    }
    vars = names(user_match_result)%>%.[which(str_detect(.,"_"))]
    .[user_match_result,
      on="userid",
      ":="(unmatched_regular_premium=round(premium_regular-i.matched_amount_regular,2),
           unmatched_tPlus_premium=round(premium_tPlus-i.matched_amount_tPlus,2),
           unmatched_current_premium=round(premium_cur-i.matched_amount_current,2),
           annual_interest_regular=i.annual_interest_regular,
           annual_interest_tPlus=i.annual_interest_tPlus,
           annual_interest_current=i.annual_interest_current)
      ]%>%{
        vars=names(.)%>%.[which(.!="userid")]
        .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]  
      }
  }%>%
    .[,":="(actual_rate_regular=((annual_interest_regular*100+
                                  unmatched_regular_premium*plain_rate)/(premium_regular+0.01))%>%
              round(.,2),
            actual_rate_tPlus=((annual_interest_tPlus*100+
                                unmatched_tPlus_premium*plain_rate)/(premium_tPlus+0.01))%>%
              round(.,2),
            actual_rate_current=((annual_interest_current*100+
                                  unmatched_current_premium*plain_rate)/(premium_cur+0.01))%>%
              round(.,2))]%>%
    .[,actual_rate_general:=((actual_rate_regular*premium_regular+
                              actual_rate_tPlus*premium_tPlus+
                              actual_rate_current*premium_cur)/(premium+0.01))%>%round(.,2)]%>%
                                {
                                  vars=names(.)%>%
                                    .[which(str_detect(.,"actual_rate"))]%>%c("userid",.)
                                  .[,.SD,.SDcols=vars]
                                }
  return(user_rate_actual)
}

# 用户期待利率（整体及分产品）
user_rate_exp<-copy(user_list)%>%
  .[cash_list[,c("userid","current_hold")],
   on="userid",
   ":="(exp_rate_regular=(regular_daily_interest*365*100/(premium_regular+0.01))%>%round(.,2),
        exp_rate_tPlus=(rate_tPlus*premium_tPlus/(premium_tPlus+0.01))%>%round(.,2),
        exp_rate_current=(((premium_cur-current_hold)*rate_cur+current_hold*plain_rate)/
                            (premium_cur+0.01))%>%round(.,2))]%>%
  .[,exp_rate_general:=((exp_rate_regular*premium_regular+
                         exp_rate_tPlus*premium_tPlus+
                         exp_rate_current*premium_cur)/(premium+0.01))%>%round(.,2)]%>%
                            {
                              vars=names(.)%>%
                                .[which(str_detect(.,"exp_rate"))]%>%c("userid",.)
                              .[,.SD,.SDcols=vars]
                            }


# 用户实际利率（整体及分产品）
user_rate_actual<-copy(user_list)%>%
  .[cash_list[,-c("premium","unmatched_current_premium_formal","exp_match_ratio")],
    on="userid",
    nomatch=0]%>%
    {
      user_match_result<-{
        match_record[asset_data[,c("id","rate")],on=c("asset_id"="id"),nomatch=0]%>%
          .[,.(matched_amount=sum(amount),annual_interest=sum(amount*rate/100)),by=.(userid,type)]%>%
          dcast(.,userid~type,value.var=c("matched_amount","annual_interest"),fill=0)%>%
          {
            vars = names(.)%>%.[which(str_detect(.,"interest"))]
            .[,total_annual_interest:=rowSums(.SD,dims = 1),.SDcols=vars]
          }
      }
      vars = names(user_match_result)%>%.[which(str_detect(.,"annual_interest_"))]%>%c("userid",.)
      .[user_match_result[,.SD,.SDcols=vars],
        on="userid",
        ":="(annual_interest_regular=i.annual_interest_regular,
             annual_interest_tPlus=i.annual_interest_tPlus,
             annual_interest_current=i.annual_interest_current)
        ]%>%
      .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]  
    }%>%
  .[,":="(actual_rate_regular=((annual_interest_regular*100+
                                unmatched_regular_premium*plain_rate)/(premium_regular+0.01))%>%
            round(.,2),
          actual_rate_tPlus=((annual_interest_tPlus*100+
                              unmatched_tPlus_premium*plain_rate)/(premium_tPlus+0.01))%>%
            round(.,2),
          actual_rate_current=((annual_interest_current*100+
                               (unmatched_current_premium+current_hold)*plain_rate)/(premium_cur+0.01))%>%
            round(.,2))]%>%
  .[,actual_rate_general:=((actual_rate_regular*premium_regular+
                            actual_rate_tPlus*premium_tPlus+
                            actual_rate_current*premium_cur)/(premium+0.01))%>%round(.,2)]%>%
                            {
                              vars=names(.)%>%
                                .[which(str_detect(.,"actual_rate"))]%>%c("userid",.)
                              .[,.SD,.SDcols=vars]
                            }


user_rate_detail<-user_rate_exp[user_rate_actual,
                                on="userid",
                                nomatch=0]%>%
  .[,":="(diff_rate_regular=round(actual_rate_regular-exp_rate_regular,2),
          diff_rate_tPlus=round(actual_rate_tPlus-exp_rate_tPlus,2),
          diff_rate_current=round(actual_rate_current-exp_rate_current,2),
          diff_rate_general=round(actual_rate_general-exp_rate_general,2))]
                                
# diff_seq<-{
#   a<-abs(user_rate_detail[diff_rate_regular!=0,]$diff_rate_regular)
#   b<-abs(user_rate_detail[diff_rate_tPlus!=0,]$diff_rate_tPlus)
#   c(a,b)
# }
# quantile(a,probs = seq(0,1,0.1))
# quantile(b,probs = seq(0,1,0.1))
# quantile(diff_seq,probs = seq(0,1,0.1))
# 
# result<-data.table()
# for(i in seq(0,0.3,0.01)){
#   ratio<-length(diff_seq[diff_seq<i])/length(diff_seq)
#   temp<-data.table(value=i,ratio=ratio)
#   result<-rbind(result,temp)
# }
# 
# source('~/rstudio/!custom/(fun)fun.elbow_point.r', encoding = 'UTF-8')
# fun.elbow_point(x=result$value,y=result$ratio,doplot = T)

rate_threhold=0.1
regular_vars<-names(user_rate_detail)%>%.[which(str_detect(.,"regular"))]
tPlus_vars<-names(user_rate_detail)%>%.[which(str_detect(.,"tPlus"))]
current_vars<-names(user_rate_detail)%>%.[which(str_detect(.,"current"))]
vars<-str_split(regular_vars,"_regular",simplify = T)[,1]

# 获得应提高利率的用户资产信息
wanna_up<-{
  wanna_up_regular<-user_rate_detail[diff_rate_regular<=-rate_threhold,]%>%
    .[,.SD,.SDcols=c("userid",regular_vars)]%T>%
    setnames(.,regular_vars,vars)%>%
    .[,type:="regular"]
    
  wanna_up_tPlus<-user_rate_detail[diff_rate_tPlus<=-rate_threhold,]%>%
    .[,.SD,.SDcols=c("userid",tPlus_vars)]%T>%
    setnames(.,tPlus_vars,vars)%>%
    .[,type:="tPlus"]
  
  tmp<-rbind(wanna_up_regular,wanna_up_tPlus)
  
  match_record[asset_data[,c("id","rate")],on=c("asset_id"="id")]%>%
    .[tmp,on=c("userid"="userid","type"="type")]%>%
    .[,diff_rate_asset:=rate-exp_rate]%>%
    .[diff_rate_asset<=-rate_threhold,]%T>%
    setorder(.,diff_rate,-exp_rate,diff_rate_asset,-amount)%>%
    .[,id:=.I]
}

# 获得应降低利率的用户资产信息
wanna_down<-{
  wanna_down_regular<-user_rate_detail[diff_rate_regular>=rate_threhold,]%>%
    .[,.SD,.SDcols=c("userid",regular_vars)]%T>%
    setnames(.,regular_vars,vars)%>%
    .[,type:="regular"]
  
  wanna_down_tPlus<-user_rate_detail[diff_rate_tPlus>=rate_threhold,]%>%
    .[,.SD,.SDcols=c("userid",tPlus_vars)]%T>%
    setnames(.,tPlus_vars,vars)%>%
    .[,type:="tPlus"]
  
  wanna_down_current<-user_rate_detail[diff_rate_current>=rate_threhold,]%>%
    .[,.SD,.SDcols=c("userid",current_vars)]%T>%
    setnames(.,current_vars,vars)%>%
    .[,type:="current"]
  
  tmp<-rbind(wanna_down_regular,wanna_down_tPlus,wanna_down_current)
  
  match_record[asset_data[,c("id","rate")],on=c("asset_id"="id")]%>%
    .[tmp,on=c("userid"="userid","type"="type")]%>%
    .[,diff_rate_asset:=rate-exp_rate]%>%
    .[diff_rate_asset>=rate_threhold,]%>%
    .[,id:=.I]
}

wanna_up_snap<-copy(wanna_up)
wanna_down_snap<-copy(wanna_down)
match_record_snap<-copy(match_record)
num=nrow(wanna_up_snap)
trans_log<-data.table()

for(i in 1:10000){
  # t1<-Sys.time()
  wait_trans<-wanna_up_snap[i,]
  asset_match_status<-match_record[,.(avail_num=n-uniqueN(userid)),by=asset_id]
  user_match_status<-match_record[,.(total_amount=sum(amount)),by=.(userid,asset_id)]
  # t2<-Sys.time()
  wait_trans<-wait_trans[asset_match_status,on="asset_id",avail_num:=i.avail_num]%>%
    .[user_match_status,on=c("userid"="userid","asset_id"="asset_id"),total_amount:=i.total_amount]
  
  if(wait_trans$avail_num==0&wait_trans$amount<wait_trans$total_amount){
    next
  }
  if(nrow(wanna_down)==0){
    break
  }
  # t3<-Sys.time()
  wanna_down_list<-wanna_down[rate>wait_trans$rate,]%>%
  {
    if(nrow(.)>0){
      .[,":="(match.up=ifelse(rate>=wait_trans$exp_rate,
                              0.5*(rate-wait_trans$exp_rate),
                              2*(wait_trans$exp_rate-rate)),
              match.down=abs(exp_rate-wait_trans$rate),
              match.amount=(abs(amount-wait_trans$amount)/wait_trans$amount))]%>%
      .[,punishment:=match.up+0.5*match.down+7*match.amount]%T>%  
        #惩罚函数，用来决定哪些用户资产优先来与之置换
        setorder(.,punishment)
    }else{next}
  }%>%
    .[asset_match_status,on="asset_id",avail_num:=i.avail_num]%>%
    .[user_match_status,on=c("userid"="userid","asset_id"="asset_id"),total_amount:=i.total_amount]
  # t4<-Sys.time()
  # 根据结果改变数据
  temp<-trans_temp(wait_trans,wanna_down_list)
  if(nrow(temp)==0){
    next
  }
  temp[,trans_no:=i]
  trans_log<-rbind(trans_log,temp)
  wanna_down<-wanna_down[temp[amount<0,],
                         on=c("asset_id"="asset_id",
                              "userid"="userid",
                              "type"="type"),
                         amount:=round(amount+i.amount,2)]%>%
    .[amount>0,]
  wanna_up<-wanna_up[temp[amount<0,],
                     on=c("asset_id"="asset_id",
                          "userid"="userid",
                          "type"="type"),
                     amount:=round(amount+i.amount,2)]%>%
    .[amount>0,]
  # t5<-Sys.time()
  match_record<-{
    a<-match_record[isfix==0,][temp[amount<0,],
                               on=c("asset_id"="asset_id",
                                    "userid"="userid",
                                    "type"="type"),
                               amount:=round(amount+i.amount,2)]
    b<-temp[,-"trans_no"][amount>0,isfix:=1]
    d<-match_record[isfix==1,]
    rbindlist(list(a,b,d),fill = T)
  }%>%.[amount>0,]
  # t6<-Sys.time()
  print(paste(i,"finished!"))
  if(i%%1000==0){
    eval(parse(text=paste0("match_record_",i,"<-copy(match_record)")))
  }
}
# 
# xx<-data.table(id=1:6,time=c(t1,t2,t3,t4,t5,t6))%>%
#   .[,diff_time:=c(0,diff(time))]

for(i in 0:10){
  if(i==0){
    match_record_temp<-match_record_snap
    }else{
    match_record_temp<-eval(parse(text=paste0("match_record_",i*1000)))%>%
    .[,.(amount=sum(amount)),by=.(userid,asset_id,type)]
  }
  user_rate_actual<-fun.user_rate_actual(user_list,asset_data,match_record_temp,plain_rate)
  user_rate_detail_temp<-user_rate_exp[user_rate_actual,
                                       on="userid",
                                       nomatch=0]%>%
    .[,":="(diff_rate_regular=round(actual_rate_regular-exp_rate_regular,2),
            diff_rate_tPlus=round(actual_rate_tPlus-exp_rate_tPlus,2),
            diff_rate_current=round(actual_rate_current-exp_rate_current,2),
            diff_rate_general=round(actual_rate_general-exp_rate_general,2))]
  
  filepath<-paste0(path,"png/")
  # 负差分布
  breaks<-seq(-4,0,0.5)
  png(filename = paste0(filepath,"up_regular_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[diff_rate_regular<=-rate_threhold,]$diff_rate_regular,
       breaks = breaks,
       labels = T,
       main = paste("用户定期实际与预期利率负差分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()

  png(filename = paste0(filepath,"up_tPlus_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[diff_rate_tPlus<=-rate_threhold,]$diff_rate_tPlus,
       breaks = breaks,
       labels = T,
       main = paste("用户T+N实际与预期利率负差分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()

  # 正差分布
  downbreaks<-seq(0,6,0.5)
  png(filename = paste0(filepath,"down_current_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[diff_rate_current>=rate_threhold,]$diff_rate_current,
       breaks = downbreaks,
       labels = T,
       main = paste("用户活期实际与预期利率正差分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()

  png(filename = paste0(filepath,"down_tPlus_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[diff_rate_tPlus>=rate_threhold,]$diff_rate_tPlus,
       breaks = downbreaks,
       labels = T,
       main = paste("用户T+N实际与预期利率正差分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()

  png(filename = paste0(filepath,"down_regular_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[diff_rate_regular>=rate_threhold,]$diff_rate_regular,
       breaks = downbreaks,
       labels = T,
       main = paste("用户定期实际与预期利率正差分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()
  
  # 整体差异分布
  total_breaks<-seq(-6,6,0.5)
  png(filename = paste0(filepath,"total_regular_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[exp_rate_regular>0,]$diff_rate_regular,
       breaks = total_breaks,
       labels = T,
       main = paste("用户定期实际与预期利率整体分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()
  
  png(filename = paste0(filepath,"total_tPlus_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[exp_rate_tPlus>0,]$diff_rate_tPlus,
       breaks = total_breaks,
       labels = T,
       main = paste("用户T+N实际与预期利率整体分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()
  
  png(filename = paste0(filepath,"total_current_diff",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(user_rate_detail_temp[exp_rate_current>0,]$diff_rate_current,
       breaks = total_breaks,
       labels = T,
       main = paste("用户活期实际与预期利率整体分布-",i*1000,"循环"),
       xlab = "实际利率-预期利率",ylab = "人数")
  dev.off()
  
}


# 资产剩余可匹人数分布
for(i in 0:10){
  if(i==0){
    match_record_temp<-match_record_snap
  }else{
    match_record_temp<-eval(parse(text=paste0("match_record_",i*1000)))%>%
      .[,.(amount=sum(amount)),by=.(userid,asset_id,type)]
  }
  
  xx<-match_record_temp[amount>0,]%>%
    .[,.(num=n-uniqueN(userid)),by=asset_id]
  
  breaks=seq(0,220,20)
  filepath<-paste0(path,"png/")
  png(filename = paste0(filepath,"asset_distr",i*1000,".png"),height = 600,width = 800,units = "px")
  hist(xx$num,
       breaks = breaks,
       labels = T,
       main = paste("资产剩余可匹人数分布-",i*1000,"循环"),
       xlab = "剩余可匹人数",ylab = "资产数",right=F)
  dev.off()
}



filepath<-"F:/Project/20170315资产匹配穿透150人调研/20170517按利率进行资产置换/match_result/result/"

objectList<-ls(pattern = "match_record_")%>%c(.,"trans_log","user_list")
for(i in objectList){
  fwrite(eval(parse(text=i)),paste0(filepath,i,".csv"))
}
















