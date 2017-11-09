library(data.table)
library(magrittr)
source('~/rstudio/20170928Coupon_kingkong/(fun)couponSim.r', encoding = 'UTF-8', echo=TRUE)
path = "F:/meituan_Project/20170928金刚位复购优惠券推荐/"
invalid_tenant_id = fread(paste0(path,"rawdata/invalid_tenant_id.txt"))

user_data = fread(paste0(path,"rawdata/user_poi_summary.txt"))
sample_data<-copy(user_data)%>%
{
  user_summary = user_data[,.(tenant_num=uniqueN(tenant_id)),by=mobile]%>%
    setorder(.,-tenant_num)
  sample_user = user_summary[tenant_num>2,mobile]
  .[mobile%in%sample_user,]
}

write.csv(sample_data,paste0(path,"sample_data.csv"),row.names = F)
sample_user<-data.table(mobile = unique(sample_data$mobile))
write.csv(sample_user,paste0(path,"sample_user.csv"),row.names = F)

########################### simulate the coupons ################################
sample_data = fread(paste0(path,"sample_data.csv"))
tenant_poi_info = fread(paste0(path,"tenant_poi_map.txt"))%>%.[,-c("version")]
poi_maps = unique(tenant_poi_info[,c("tenant_id","poi_id")])
tenants = unique(sample_data$tenant_id)%>%.[which(.%in%poi_maps$tenant_id)]
simNum = 7*length(tenants)
couponList = couponSim(tenants,
                       poi_maps,
                       simNum,
                       amtSpan=c(5,20),
                       typeSet=0:7,
                       timeSpan = c("2017-10-09","2017-12-31"),
                       poi_include = 0.8)
write.csv(couponList,paste0(path,"couponListSim.csv"),row.names = F)


