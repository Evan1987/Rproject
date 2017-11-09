
# 优惠券总池数据模拟
couponSim<-function(tenants,
                    poi_maps,
                    simNum,
                    amtSpan = c(5,20),
                    typeSet=0:7,
                    timeSpan = c("2017-10-09","2017-12-31"),
                    poi_include=0.8){
  df = expand.grid(tenants,typeSet)%>%
    as.data.table(.)%T>%
    {
      names(.)<-c("tenant_id","coupon_type")
    }
  
  if(simNum>nrow(df)){
    warning("the simNum is bigger than the simu ability. Have reduce the simNum to the 0.8*tenantNum*typeNum!")
    simNum = 0.8*nrow(df)
  }
  
  result = df[sample(1:nrow(df),simNum),]%>%
    .[,amt:=ceiling(runif(simNum,min=amtSpan[1],max=amtSpan[2]))]%>%
    .[,":="(start_time=as.Date(timeSpan[1]),end_time=as.Date(timeSpan[2]))]%>%
    .[,id:=.I]
  
  # 随机选择适用门店
  tenant_summary = poi_maps[tenant_id%in%unique(result$tenant_id),]%>%
    .[,.(num=uniqueN(poi_id)),by=tenant_id]%>%
    .[,sample_num:=ceiling(poi_include*num)]
  
  result2 = copy(result)%>%
    .[tenant_summary,on="tenant_id",sample_num:=i.sample_num]%>%
    .[poi_maps[,c("tenant_id","poi_id")],on="tenant_id",allow.cartesian=TRUE]%>%
    .[complete.cases(.),]%>%
    .[,poi_id:=as.character(poi_id)]%>%
    {
      poi_select = .[,.(poi_id=sample(poi_id,first(sample_num))),by=.(id,tenant_id)]%>%
        .[,poi_id:=as.integer(poi_id)]
      result[poi_select,on=c("id","tenant_id")]
    }%>%
    setorder(.,tenant_id,id,poi_id)
  return(result2)
}




