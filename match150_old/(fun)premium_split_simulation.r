
# 存量分割模拟函数，根据需求将每个用户的资金存量分成“定期”、“T+N”、“活期”
# user_data：match主程序的用户资金序列
# item：分割的产品名称
# ratio_mean：各产品分割比例期望
# ratio_sd：各产品分割比例标准差
# unit_premium：各产品要求的最小单位金额
premium_split_simulation<-function(user_data,
                                   item=c("regular","tPlus","current"),
                                   ratio_mean=c(0.16,0.61,NA),
                                   ratio_sd=c(0.05,0.05,NA),
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