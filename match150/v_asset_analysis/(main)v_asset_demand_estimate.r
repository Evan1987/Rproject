

#根据大资产匹配的资金流结果计算小V资产需求量
library(data.table)
library(magrittr)
path<-"F:/Project/20170315资产匹配穿透150人调研/20170413匹配试验/"
#读取大资产匹配的剩余资金流
user_result<-fread(paste0(path,"user_result.csv"))

#需要靠小V资产匹配达到的目标利率
rt=5.5

result<-data.table()
#简理财现金宝资产的利率不定，所以用不同数值进行评估，[2.5:3.2,0.1]
#估算需求的资金量
# delta_interest=v_asset_supply*(v_rate-plain_rate)/100
for(plain_rate in seq(from=2.5,to=3.2,by=0.1)){
  temp<-copy(user_result)%>%
    .[,general_rate:=((asset_interest+unmatched_premium*plain_rate/100)/premium*100)%>%round(.,2)]
  consider_temp<-temp[general_rate<rt,]%>%
    .[,delta_interest:=(rt-general_rate)*premium/100]
  result_temp<-data.table(plain_rate=plain_rate,delta_interest=sum(consider_temp$delta_interest),usernum=nrow(consider_temp))
  result<-rbind(result,result_temp)
}





