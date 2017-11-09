library(data.table)
library(magrittr)

path ="F:/Project/20170913大额用户超额体现需求/"

user_info = fread(paste0(path,"user_info.csv"))
rawData = fread(paste0(path,"result.csv"))

potentialNumCal <- function(redeem_limit_amount,rawData,target_users,N){
  num = {
    users = rawData[redeem_amount>=redeem_limit_amount,]$userid%>%unique(.)
    length(users[which(users%in%target_users)])
  }
  return(num)
}


userTestCalbyPremium<- function(premium,rawdata,user_info,redeem_limit = seq(3e+5,10e+5,1e+5)){
  target_users = user_info[max_premium_final>=premium,]$userid
  N = length(target_users)
  potential_num = mapply(potentialNumCal,
                         redeem_limit,
                         MoreArgs = list(rawData=rawData,
                                         target_users = target_users,
                                         N=N))
  return(data.table(redeem_limit_amount=redeem_limit,
                    potential_num=potential_num,
                    total_num=N,
                    ratio = round(potential_num/N,2)))
}


# 50万存量用户
result50w = userTestCalbyPremium(premium = 5e+5,rawdata,user_info)

# 100万存量用户
result100w = userTestCalbyPremium(premium = 10e+5,rawdata,user_info)










