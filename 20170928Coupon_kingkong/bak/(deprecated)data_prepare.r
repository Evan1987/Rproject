library(data.table)
library(magrittr)

path = "F:/meituan_Project/20170928金刚位复购优惠券推荐/"

user_data = fread(paste0(path,"user_poi_summary.txt"))

setnames(user_data,c("t0.mobile","t0.tenant_id","t0.poi_id"),c("mobile","tenant_id","poi_id"))
clean_user_data = user_data[total_order_amt>0&total_order_cnt>0&mobile!="NULL",]

vars = c("m_0_1_order_amt","m_0_1_order_cnt")
clean_user_data = clean_user_data[,(vars):=lapply(.SD,as.numeric),.SDcols=vars]
clean_user_data = clean_user_data[,(vars):=lapply(.SD,function(x) replace(x,is.na(x),0)),.SDcols=vars]
