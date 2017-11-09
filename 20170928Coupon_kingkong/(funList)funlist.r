# 函数列表
# // 用户喜爱度评价列表
preferEval<-function(m_0_1_cnt,
                     m_0_1_amt,
                     m_1_3_cnt,
                     m_1_3_amt,
                     m_3_6_cnt,
                     m_3_6_amt,
                     m_6_12_cnt,
                     m_6_12_amt,
                     total_cnt,
                     total_amt,
                     weight=c(1,0.8,0.5,0.3),class_weight=c(0.3,0.7)){
  cnt_score = (m_0_1_cnt*weight[1]+m_1_3_cnt*weight[2]+m_3_6_cnt*weight[3]+m_6_12_cnt*weight[4])/total_cnt
  amt_score = (m_0_1_amt*weight[1]+m_1_3_amt*weight[2]+m_3_6_amt*weight[3]+m_6_12_amt*weight[4])/total_amt
  score = cnt_score*class_weight[1]+amt_score*class_weight[2]+0.01
  return(score)
}

# // 时间替换函数，将用户生日转换到特定年份
replaceYear<-function(now_day,year=2017){
  now_day = as.Date(now_day)
  char = format(now_day,"%m-%d")
  char = ifelse(char=="02-29"& !lubridate::leap_year(year),"02-28",char)
  thisYearChar = paste0(year,"-",char)
  return(as.Date(thisYearChar))
}

# // 用户偏好UserProfile生成函数
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
    .[,.(score=sum(score),lat=sum(lat*score),lng=sum(lng*score)),by=mobile]%>%
    .[,":="(lat=round(lat/score,6),lng=round(lng/score,6))]
  
  return(list("tenant_prefer"=user_tenant_prefer,
              "area_prefer"=user_area_prefer,
              "city_prefer"=user_city_prefer,
              "coord_prefer"=user_coord_prefer))
}


# // 排序函数生成器
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

# // 曼哈顿距离计算器
ManhattanDist<-function(ja,wa,jb,wb,R=6371){
  R*abs(wa-wb)*pi/180 + R*cos(0.5*(wa+wb)*pi/180)*abs(ja-jb)*pi/180
}

# // 计算itemProfile 和 userProfile 的匹配推荐程度
item_user_matchCal<-function(target_item_profile,
                             target_user_profile,
                             max_dist = 100,
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
    .[dist<max_dist,]
  
  if(nrow(itemEval)==0){
    return(data.table())
  }
  
  itemEval<-itemEval[target_user_profile$tenant_prefer[,-"mobile"],on="tenant_id",tenant_rank:=i.rank]%>%
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


# // 推荐函数
recommendFun<-function(user,userProfile,itemProfile,max_dist=100,num=50){
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







