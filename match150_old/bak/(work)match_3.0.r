library(data.table)
library(magrittr)

######################## 0. 初始条件及数据集处理模拟 ##########################
#基本常量
n=150
##################### 0.1 匹配差异化处理函数 ###################
regular_adjust=F
tPlus_adjust=T
tPlus_breaks=c(0,1e+04,10e+04,50e+04,100e+04,5000e+04)
tPlus_adjust_ratio=c(1,1,0.95,0.9,0.8)
current_adjust=F

# 【FUN】建立匹配缩放函数，对相应产品的存量金额进行区间放缩。
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

##################### 0.2 不同产品存量模拟函数 ###################
# 【FUN】建立存量分解模拟函数，按照指定期望比例正态随机出不同产品的存量。
premium_split_simulation<-function(user_data,
                                   item=c("regular","tPlus","current"),
                                   ratio_mean=c(0.15,0.6,0.25),
                                   ratio_sd=c(0.05,0.2,NA),
                                   unit_premium=c(1000,1000,NA)){
  library(data.table)
  library(magrittr)
  if(length(item)!=length(ratio_mean)){
    stop("premium split ratio must have same length of items !")
  }
  result<-copy(user_data)%>%
    .[,regular_premium_ratio:=rnorm(nrow(.),
                                          mean=ratio_mean[1],
                                          sd=ratio_sd[1])%>%abs(.)%>%round(.,4)]%>%
    .[,regular_premium_ratio:=ifelse(regular_premium_ratio>1,1,regular_premium_ratio)]%>%
    .[,regular_premium:=(regular_premium_ratio*premium/unit_premium[1])%>%floor(.)*unit_premium[1]]%>%
    .[,regular_premium_ratio:=regular_premium/premium]%>%
    .[,tPlus_premium_ratio:=rnorm(nrow(.),
                                  mean=ratio_mean[2],
                                  sd=ratio_sd[2])%>%abs(.)%>%round(.,4)]%>%
    .[,tPlus_premium_ratio:=ifelse(tPlus_premium_ratio+regular_premium_ratio>1,
                                   1-regular_premium_ratio,
                                   tPlus_premium_ratio)]%>%
    .[,tPlus_premium:=(tPlus_premium_ratio*premium/unit_premium[2])%>%floor(.)*unit_premium[2]]%>%
    .[,":="(tPlus_premium_ratio=tPlus_premium/premium,
            unmatched_regular_premium=regular_premium,
            unmatched_tPlus_premium=tPlus_premium)]%>%
    .[,unmatched_current_premium:=premium-unmatched_regular_premium-unmatched_tPlus_premium]
  
  return(result)
}


################## 0.3 用户信息表 #######################
user_data<-fread("F:/Project/20170315资产匹配穿透150人调研/user_data.csv")%>%
  .[premium>=100,]%>%
  premium_split_simulation(user_data = .)

################## 0.4 资产信息表 #######################
asset_data<-fread("~/match150/asset_data.csv")%>%
  .[,":="(unmatched_amount=amount,
          avail_num=n)]%>%
  .[,avg_avail_amount:=unmatched_amount/avail_num]%T>%
  setkey(.,id)

################# 0.5 循环初始化 ########################
asset_list<-copy(asset_data)
user_list<-copy(user_data)%>%
  .[,":="(regular_premium_ratio=NULL,
          tPlus_premium_ratio=NULL)]

match_record<-data.table(id=NA,userid=NA,asset_id=NA,type=NA,amount=NA)

i=1
################### 1. 定期匹配 #####################################
if(regular_adjust){
  user_list<-match_adjust(user_data = user_list,
                          item = "regular",
                          breaks = regular_breaks,
                          adjust_ratio = regular_adjust_ratio)
}
is_regular_normal_break=1
while(sum(user_list$unmatched_regular_premium)>0){
  if(nrow(asset_list[unmatched_amount>0&avail_num>0,])==0){
    is_regular_normal_break=0
    break
  }
  wait_user_data<-setorder(user_list[unmatched_regular_premium>0,],-unmatched_regular_premium)%>%
    head(.,1)
  
  avail_asset_list<-copy(asset_list)%>%
    .[id%in%match_record[userid==wait_user_data$userid,]$asset_id & unmatched_amount>0,]%>%
    .[,remarks:=1]%>%
    rbind(.,copy(asset_list)%>%
            .[unmatched_amount>0 & avail_num>0,]%>%
            .[,remarks:=0])%>%
    .[!is.na(remarks),]%T>%
    setorder(.,-remarks,-unmatched_amount)%>%
    head(.,1)
  
  temp<-data.table(id=i,
                   userid=wait_user_data$userid,
                   asset_id=avail_asset_list$id,
                   type="regular",
                   amount=min(wait_user_data$unmatched_regular_premium,
                              avail_asset_list$unmatched_amount)
  )
  match_record<-rbind(match_record,temp)
  
  avail_asset_list[,":="(unmatched_amount=unmatched_amount-temp$amount,
                         avail_num=n-uniqueN(match_record[asset_id==avail_asset_list$id,]$userid,na.rm=T)
  )]%>%
    .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]
  
  
  user_list[userid==wait_user_data$userid,
            unmatched_regular_premium:=unmatched_regular_premium-temp$amount]
  
  asset_list[id==avail_asset_list$id,
             ":="(unmatched_amount=avail_asset_list$unmatched_amount,
                  avail_num=avail_asset_list$avail_num,
                  avg_avail_amount=avail_asset_list$avg_avail_amount)]
  
  if(i%%1000==0){
    print(paste("regular -",i,"finished!"))
  }
  
  i=i+1
}
print("--------regular matching finished!----------")

################### 2. T+N匹配 #####################################
if(tPlus_adjust){
  user_list<-match_adjust(user_data = user_list,
                          item = "tPlus",
                          breaks = tPlus_breaks,
                          adjust_ratio = tPlus_adjust_ratio)
}
is_tPlus_normal_break=1
while(sum(user_list$unmatched_tPlus_premium)>0){
  if(nrow(asset_list[unmatched_amount>0&avail_num>0,])==0){
    is_tPlus_normal_break=0
    break
  }
  
  wait_user_data<-setorder(user_list[unmatched_tPlus_premium>0,],-unmatched_tPlus_premium)%>%
    head(.,1)
  
  avail_asset_list<-copy(asset_list)%>%
    .[id%in%match_record[userid==wait_user_data$userid,]$asset_id & unmatched_amount>0,]%>%
    .[,remarks:=1]%>%
    rbind(.,copy(asset_list)%>%
            .[unmatched_amount>0 & avail_num>0,]%>%
            .[,remarks:=0])%>%
    .[!is.na(remarks),]%T>%
    setorder(.,-remarks,-unmatched_amount)%>%
    head(.,1)
  
  temp<-data.table(id=i,
                   userid=wait_user_data$userid,
                   asset_id=avail_asset_list$id,
                   type="tPlus",
                   amount=min(wait_user_data$unmatched_tPlus_premium,
                              avail_asset_list$unmatched_amount)
  )
  match_record<-rbind(match_record,temp)
  
  avail_asset_list[,":="(unmatched_amount=unmatched_amount-temp$amount,
                         avail_num=n-length(unique(match_record[asset_id==avail_asset_list$id,]$userid))
  )]%>%
    .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]
  
  
  user_list[userid==wait_user_data$userid,
            unmatched_tPlus_premium:=unmatched_tPlus_premium-temp$amount]
  
  asset_list[id==avail_asset_list$id,
             ":="(unmatched_amount=avail_asset_list$unmatched_amount,
                  avail_num=avail_asset_list$avail_num,
                  avg_avail_amount=avail_asset_list$avg_avail_amount)]
  
  if(i%%1000==0){
    print(paste("tPlus -",i,"finished!"))
  }
  i=i+1
}
########################### 2.* tPlus fillin #####################################
#如果不是完全匹配完资金，则需要观察是否有avail_num=0但unmatched_amount>0的资产还可以匹配给其已匹配用户
if(!is_tPlus_normal_break){
  print("tPlus fillin unempty&full asset started!")
  while(nrow(asset_list[avail_num==0&unmatched_amount>0,])>0){
    full_num_asset<-asset_list[avail_num==0&unmatched_amount>0,]%T>%
      setorder(.,-unmatched_amount)
    
    #满人非空资产遍历，初始化fillin金额变量
    fillin=0
    for(j in 1:nrow(full_num_asset)){
      wait_asset_data<-full_num_asset[j,]
      avail_user_list<-user_list[userid%in%match_record[asset_id==wait_asset_data$id,]$userid,]%>%
        .[unmatched_tPlus_premium>0,]
      
      #如果有这样的可匹配用户，进行fillin
      if(nrow(avail_user_list)>0){
        
        avail_user_list<-setorder(avail_user_list,-unmatched_tPlus_premium)%>%
          head(.,1)
        temp<-data.table(id=i,
                         userid=avail_user_list$userid,
                         asset_id=wait_asset_data$id,
                         type="tPlus_fillin",
                         amount=min(avail_user_list$unmatched_tPlus_premium,
                                    wait_asset_data$unmatched_amount)
        )
        
        match_record<-rbind(match_record,temp)
        
        
        user_list[userid==avail_user_list$userid,
                  unmatched_tPlus_premium:=unmatched_tPlus_premium-temp$amount]
        
        asset_list[id==wait_asset_data$id,
                   unmatched_amount:=unmatched_amount-temp$amount]
        i=i+1
        #记录这样补充进来的用户金额
        if(i%%1000==0){
          print(paste("tPlus fillin -",i,"finished!"))
        }
        fillin=fillin+temp$amount
      }
    }
    #遍历这样的满人非空资产结束仍然没有可以匹配的金额就跳出
    if(fillin==0){break}
  }
}

print("--------tPlus matching finished!----------")



#################################### 3. 活期匹配 ###############################################
if(current_adjust){
  user_list<-match_adjust(user_data = user_list,
                          item = "current",
                          breaks = current_breaks,
                          adjust_ratio = current_adjust_ratio)
}
is_current_normal_break=1
while(sum(user_list$unmatched_current_premium)>0){
  if(nrow(asset_list[unmatched_amount>0&avail_num>0,])==0){
    is_current_normal_break=0
    break
  }
  wait_user_data<-setorder(user_list[unmatched_current_premium>0,],-unmatched_current_premium)%>%
    head(.,1)
  
  avail_asset_list<-copy(asset_list)%>%
    .[id%in%match_record[userid==wait_user_data$userid,]$asset_id & unmatched_amount>0,]%>%
    .[,remarks:=1]%>%
    rbind(.,copy(asset_list)%>%
            .[unmatched_amount>0 & avail_num>0,]%>%
            .[,remarks:=0])%>%
    .[!is.na(remarks),]%T>%
    setorder(.,-remarks,-unmatched_amount)%>%
    head(.,1)
  
  temp<-data.table(id=i,
                   userid=wait_user_data$userid,
                   asset_id=avail_asset_list$id,
                   type="current",
                   amount=min(wait_user_data$unmatched_current_premium,
                              avail_asset_list$unmatched_amount)
  )
  match_record<-rbind(match_record,temp)
  
  avail_asset_list[,":="(unmatched_amount=unmatched_amount-temp$amount,
                         avail_num=n-length(unique(match_record[asset_id==avail_asset_list$id,]$userid))
  )]%>%
    .[,avg_avail_amount:=ifelse(avail_num==0,0,unmatched_amount/avail_num)]
  
  
  user_list[userid==wait_user_data$userid,
            unmatched_current_premium:=unmatched_current_premium-temp$amount]
  
  asset_list[id==avail_asset_list$id,
             ":="(unmatched_amount=avail_asset_list$unmatched_amount,
                  avail_num=avail_asset_list$avail_num,
                  avg_avail_amount=avail_asset_list$avg_avail_amount)]
  
  if(i%%1000==0){
    print(paste("current -",i,"finished!"))
  }
  i=i+1
}

########################### 3.* current fillin #####################################
#如果不是完全匹配完资金，则需要观察是否有avail_num=0但unmatched_amount>0的资产还可以匹配给其已匹配用户
if(!is_current_normal_break){
  print("current fillin unempty&full asset started!")
  while(nrow(asset_list[avail_num==0&unmatched_amount>0,])>0){
    full_num_asset<-asset_list[avail_num==0&unmatched_amount>0,]%T>%
      setorder(.,-unmatched_amount)
    
    #满人非空资产遍历，初始化fillin金额变量
    fillin=0
    for(j in 1:nrow(full_num_asset)){
      wait_asset_data<-full_num_asset[j,]
      avail_user_list<-user_list[userid%in%match_record[asset_id==wait_asset_data$id,]$userid,]%>%
        .[unmatched_current_premium>0,]
      
      #如果有这样的可匹配用户，进行fillin
      if(nrow(avail_user_list)>0){
        
        avail_user_list<-setorder(avail_user_list,-unmatched_current_premium)%>%
          head(.,1)
        temp<-data.table(id=i,
                         userid=avail_user_list$userid,
                         asset_id=wait_asset_data$id,
                         type="current_fillin",
                         amount=min(avail_user_list$unmatched_current_premium,
                                    wait_asset_data$unmatched_amount)
        )
        
        match_record<-rbind(match_record,temp)
        
        
        user_list[userid==avail_user_list$userid,
                  unmatched_current_premium:=unmatched_current_premium-temp$amount]
        
        asset_list[id==wait_asset_data$id,
                   unmatched_amount:=unmatched_amount-temp$amount]
        i=i+1
        #记录这样补充进来的用户金额
        if(i%%1000==0){
          print(paste("current fillin -",i,"finished!"))
        }
        fillin=fillin+temp$amount
      }
    }
    #遍历这样的满人非空资产结束仍然没有可以匹配的金额就跳出
    if(fillin==0){break}
  }
}

print("--------current matching finished!----------")

