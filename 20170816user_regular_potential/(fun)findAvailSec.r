
# 给定minLen（定期长度）确定可使用户收益最大化的投资区间和金额（“水面”高度）
# 采用二分法确定“水面”高度
findAvailSec<-function(user_data,
                       judge.var="premium",
                       date.var="date",
                       minPremium=1000,
                       minStep=1000,
                       minLen=30,
                       maxStepNum=10){
  # 连续匹配的正则
  pattern = paste0("1{",minLen,",}")
  
  # 二分法寻找给定匹配模式的最大可投金额
  x = minPremium
  user_cash_log<-copy(user_data)
  log_day = setnames(user_cash_log,date.var,"log_day")%>%.$log_day
  judge = setnames(user_cash_log,judge.var,"judge")%>%.$judge
  up = max(judge)
  judgeVec = as.integer(judge>=x)%>%
    str_c(.,collapse="")
  availSec = str_locate_all(judgeVec,pattern)[[1]]
  if(nrow(availSec)>0){
    finalAvailSec = availSec
    finalX = down = x
    newX = floor(1/2*(up+down)/minStep)*minStep
    step = newX - x
    x = newX
    stepNum = 1
    while(stepNum<=maxStepNum & step>=minStep){
      judgeVec = as.integer(judge>=x)%>%
        str_c(.,collapse="")
      availSec = str_locate_all(judgeVec,pattern)[[1]]
      if(nrow(availSec)>0){
        finalAvailSec = availSec
        finalX = down = x
        newX = floor(1/2*(up+down)/minStep)*minStep
        step = abs(newX - x)
        x = newX
      }else{
        up = x
        newX = floor(1/2*(up+down)/minStep)*minStep
        step = abs(newX - x)
        x = newX
      }
      stepNum = stepNum + 1  
    }
    df = data.table(len = minLen,
                    from = log_day[finalAvailSec[,1]],
                    to = log_day[finalAvailSec[,2]+1-minLen],
                    amount = finalX
    )
  }else{
    df = data.table()
  }
  return(df)
}
