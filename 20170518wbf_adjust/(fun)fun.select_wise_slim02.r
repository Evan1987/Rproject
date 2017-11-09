# 采用0-1规划求解选择用户
# 三个约束方程：①总人数==exp_user_num；②总订单数==exp_num；③总金额>=exp_amount
# 一个目标方程: 总金额的最小值
fun.select_wise_slim<-function(exp_user_num,exp_num,exp_amount,target_data){
  
  model<-list(obj = )
  
  
  
  
  result<-gurobi(model = model)
  
  if(result$status=="OPTIMAL"){
    x<-result$x
    select_result<-target_data_summary[which(x==1),]
  }else{
    select_result<-target_data_summary[0]
  }
  
  return(select_result)
}

# 采用0-1规划求解选择用户
# 三个约束方程：①总人数==exp_user_num；②总订单数==exp_num；③总金额>=exp_amount
# 一个目标方程: 总金额的最小值
fun.select_wise_slim_loose<-function(exp_user_num,
                                     exp_num,
                                     exp_amount,
                                     target_data_summary,
                                     looselist=c(3,5,0),
                                     doLooseIt = T,
                                     looseIteration = 5){
  
  model<-list()
  # 目标方程系数
  model$obj<-target_data_summary$amount
  # 约束方程左边系数矩阵
  model$A<-matrix(c(rep(1,nrow(target_data_summary)),
                    rep(1,nrow(target_data_summary)),
                    target_data_summary$num,
                    target_data_summary$num,
                    target_data_summary$amount),nrow = 5,byrow = T)
  # 约束方程等式关系
  model$sense<-c("<",">","<",">","<=")
  # 约束方程等式右边
  model$rhs = c(exp_user_num+looselist[1],
                exp_user_num-looselist[1],
                exp_num+looselist[2],
                exp_num-looselist[2],
                exp_amount)
  # 求解类型：0-1规划
  model$vtype = "B"
  # 目标优化方向
  model$modelsense = "max"
  
  params = list(OutputFlag=0)
  result<-gurobi(model = model)
  
  select_result<-target_data_summary[0]
  
  if(result$status=="OPTIMAL"){
    x<-result$x
    select_result<-target_data_summary[which(x==1),]
  }else if(doLooseIt){
    warning("There is no solutions! Try another looselist!")
    i=1
    while(nrow(select_result)==0&i<=looseIteration){
      looselist[1:2]<-looselist[1:2]+1
      select_result<-fun.select_wise_slim_loose(exp_user_num,
                                                exp_num,
                                                exp_amount,
                                                target_data_summary,
                                                looselist=looselist,
                                                doLooseIt=F)
      i=i+1
    }
    if(nrow(select_result)==0){stop("There is no solutions!")}
    
  }else{
    select_result<-target_data_summary[0]
  }
  return(select_result)
}


