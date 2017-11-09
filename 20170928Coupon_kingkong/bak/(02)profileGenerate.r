library(data.table)
library(magrittr)
library(tcltk)
path = "F:/meituan_Project/20170928金刚位复购优惠券推荐/"

# 时间替换函数，将用户生日转换到特定年份
replaceYear<-function(now_day,year=2017){
  now_day = as.Date(now_day)
  char = format(now_day,"%m-%d")
  char = ifelse(char=="02-29"& !lubridate::leap_year(year),"02-28",char)
  thisYearChar = paste0(year,"-",char)
  return(as.Date(thisYearChar))
}

poi_info = fread(paste0(path,"poi_info.txt"))%>%
  .[,-c("version","type_id","close_status")]
user_birthday = fread(paste0(path,"user_birthday.txt"))%>%
  .[,birthday:=as.Date(birthday)]%>%
  .[!is.na(birthday),":="(thisYearBirthday=replaceYear(birthday,year(Sys.time())),nextYearBirthday=replaceYear(birthday,year(Sys.time())+1))]


sample_prefer = fread(paste0(path,"sample_prefer.csv"))

## 1.1 Generate the UserProfile
userProfileGenerator<-function(user_poi_prefer,poi_info){
  
  area_prefer = copy(user_poi_prefer)%>%
    .[poi_info,on="poi_id",":="(lat=i.latitude/1e+6,lng=i.longitude/1e+6,barea_id=i.barea_id,city_id=i.cityid)]
  
  user_tenant_prefer = user_poi_prefer[,.(score=sum(score)),by=.(mobile,tenant_id)]%T>%
    setorder(.,mobile,-score)%>%
    .[,rank:=seq(1,length(tenant_id)),by=mobile]%>%
    .[rank<=50,-"score"]
  
  user_area_prefer = area_prefer[,.(score=sum(score)),by=.(mobile,barea_id)]%T>%
    setorder(.,mobile,-score)%>%
    .[,rank:=seq(1,length(barea_id)),by=mobile]%>%
    .[rank<=2,-"score"]
  
  user_city_prefer = area_prefer[,.(score=sum(score)),by=.(mobile,city_id)]%>%
    setorder(.,mobile,-score)%>%
    .[,rank:=seq(1,length(city_id)),by=mobile]%>%
    .[rank<=1,-"score"]
  
  user_coord_prefer = area_prefer[user_city_prefer,on=c("mobile","city_id")]%>%
    .[,.(score=sum(score+0.01),lat=sum(lat*(score+0.01)),lng=sum(lng*(score+0.01))),by=mobile]%>%
    .[,":="(lat=round(lat/score,6),lng=round(lng/score,6))]
  
  return(list("tenant_prefer"=user_tenant_prefer,
              "area_prefer"=user_area_prefer,
              "city_prefer"=user_city_prefer,
              "coord_prefer"=user_coord_prefer))
}

userProfile = userProfileGenerator(sample_prefer,poi_info)%>%c(.,list("user_birthday"=user_birthday))

## 1.2 Generate the ItemProfile
itemProfile = fread(paste0(path,"couponListSim.csv"))%>%
  .[poi_info[,-c("tenant_id")],on="poi_id",":="(lat=i.latitude/1e+6,lng=i.longitude/1e+6,barea_id=i.barea_id,city_id=i.cityid)]%>%
  .[,city_id:=as.integer(city_id)]%>%
  {
    poi_summary = poi_info[,.(num=uniqueN(poi_id)),by=tenant_id]
    .[poi_summary,on="tenant_id",num:=i.num]
  }%>%
  setnames(.,"id","coupon_id")


rm(list=c("sample_prefer","poi_info","user_birthday"))
gc()

## 2. Do Recommend

### some identification:
#### coupon_type = 0:7
#### 0: birthday
#### 1: waimai
#### 2: highF
#### 3: mediumF
#### 4: lowF
#### 5: active
#### 6: silent
#### 7: drained
#### priority when conflict: 0>5>2>1>6>7>3>4
users = unique(userProfile$tenant_prefer$mobile)

# ///排序函数生成器
mapGenerator<-function(order){
  rank = 1:length(order)
  sort_rank = rank[order(order)]
  text = "switch(x+1"
  for(i in sort_rank){
    text = paste(text,",",i)
  }
  text = paste(text,")")
  
  y<-eval(parse(text = paste("function(x){",text,"}")))
  return(y)
}
priorFun<-mapGenerator(order=c(0,5,2,1,6,7,3,4))

itemProfile[,priorLevel:=sapply(coupon_type,priorFun)]

# ///曼哈顿距离计算器
ManhattanDist<-function(ja,wa,jb,wb,R=6371){
  R*abs(wa-wb)*pi/180 + R*cos(0.5*(wa+wb)*pi/180)*abs(ja-jb)*pi/180
}

# ///计算itemProfile 和 userProfile 的匹配推荐程度
item_user_matchCal<-function(target_item_profile,
                             target_user_profile,
                             max_dist = 30,
                             deleteCouponList = c(0),
                             num=50,
                             weightList = list("isPreferedTenant"=1000,
                                               "isPreferedBarea"=100,
                                               "tenant_rank"=100,
                                               "barea_rank"=50,
                                               "dist_punish"= -800,
                                               "dist_bound_ratio"=0.8,
                                               "dist_weight_change_ratio"=-0.2)){
  
  target_item_profile = target_item_profile[!coupon_type%in%deleteCouponList,]
  
  itemEval = target_item_profile[,dist:=ManhattanDist(lng,lat,target_user_profile$coord_prefer$lng,target_user_profile$coord_prefer$lat)]%>%
    .[dist<max_dist,]%>%
    .[target_user_profile$tenant_prefer[,-"mobile"],on="tenant_id",tenant_rank:=i.rank]%>%
    .[target_user_profile$area_prefer[,-"mobile"],on="barea_id",barea_rank:=i.rank]%>%
    {
      vars = c("tenant_rank", "barea_rank")
      .[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),-1)),.SDcols=vars]
    }%>%
    .[,score:=
        weightList$isPreferedTenant*(tenant_rank>0) + 
        weightList$isPreferedBarea*(barea_rank>0) + 
        weightList$tenant_rank*1/tenant_rank*(tenant_rank>0) + 
        weightList$barea_rank*1/barea_rank*(barea_rank>0) + 
        weightList$dist_punish*1/(1+exp(weightList$dist_weight_change_ratio*(dist-max_dist*weightList$dist_bound_ratio)))]
  
  summary = itemEval[,.(max_score=max(score),min_priorLevel=min(priorLevel)),by=tenant_id]%T>%
    setorder(.,-max_score)%>%
    .[,rank:=.I]%>%
    head(.,num)
  
  selectItem = itemEval[summary[,-"max_score"],on=c("tenant_id","priorLevel"="min_priorLevel")]
  
  return(unique(selectItem[,c("coupon_id","tenant_id","rank")]))
}


# ///推荐函数

recommendFun<-function(user,userProfile,itemProfile,max_dist,num=50){
  target_user_profile = sapply(userProfile,function(x) x[mobile==user,])
  target_item_profile = itemProfile[is.na(city_id)|city_id==target_user_profile$city_prefer$city_id,]
  
  now_day =as.Date(Sys.time())
  
  # 0. birthday coupon: coupon_type==0
  echelon0<-data.table()
  ## 判断生日条件
  if(nrow(user_birthday<-target_user_profile$user_birthday[!is.na(birthday) & 
                                                           (between(thisYearBirthday,now_day,now_day+7)|between(nextYearBirthday,now_day,now_day+7)),])>0){
    # 判断
    if(nrow(birthday_hit_item<-target_item_profile[coupon_type==0 & tenant_id%in%user_birthday$tenant_id,])>0){
      echelon0 = item_user_matchCal(target_item_profile = birthday_hit_item,
                                    target_user_profile = target_user_profile,
                                    max_dist = max_dist,
                                    deleteCouponList = NA,
                                    num=1)
    }
  }
  
  # 1. hit coupon: itemProfile$tenant_id %in% tenant_prefer$tenant_id
  echelon1<-data.table()
  if(nrow(tenant_hit_item<-target_item_profile[tenant_id%in%target_user_profile$tenant_prefer$tenant_id,])>0){
    echelon1 = item_user_matchCal(target_item_profile=tenant_hit_item,
                                  target_user_profile=target_user_profile,
                                  max_dist=max_dist,
                                  deleteCouponList = c(0),
                                  num=num)%T>%
                                  {
                                    if(nrow(echelon0)>0){
                                      .[!tenant_id%in%echelon0$tenant_id,]
                                    }
                                  }
  }
  
  
  # 2. supplement: use
  echelon2 = data.table()
  now_num = nrow(echelon0) + nrow(echelon1)
  supple_num = ifelse(now_num<num,num-now_num,0)
  if(supple_num>0){
    supple_item = target_item_profile[!tenant_id%in%target_user_profile$tenant_prefer$tenant_id & num>=5,]
    echelon2 = item_user_matchCal(target_item_profile=supple_item,
                                  target_user_profile=target_user_profile,
                                  max_dist=max_dist,
                                  deleteCouponList = c(0),
                                  num=supple_num)
                                  
  }
  
  result = rbindlist(list(echelon0,echelon1,echelon2))%>%
    .[,rank:=.I]
  
  return(result)
}


# ///main
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)
conn = MySQL_conn_select("local",db_name = "meituan")
user_num = length(users)
pb <- tkProgressBar("进度","已完成 %", 0, 100)
for(i in 1:user_num){
  user = users[i]
  user_recommend = recommendFun(user,
                                userProfile = userProfile,
                                itemProfile = itemProfile,
                                num=50)%>%
    .[,mobile:=user]
  
  tryCatch({dbWriteTable(conn,"coupon_recommend",user_recommend,append=T,row.names=F)},
           error=function(e){
             cat("error occured", conditionMessage(e),"\n\n")
             conn = MySQL_conn_select("local",db_name = "meituan")
             dbWriteTable(conn,"coupon_recommend",user_recommend,append=T,row.names=F)
             return("error")
           },finally = {
             info<- sprintf("已完成 %.4f%%", round(i*100/user_num,4))  
             setTkProgressBar(pb, i*100/user_num, sprintf("进度 (%s)", info),info)
           })
}

dbDisconnect(conn)
