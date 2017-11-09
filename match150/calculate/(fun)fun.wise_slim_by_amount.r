fun.wise_slim_by_amount<-function(user_list,
                                 tol = 5e+6,
                                 target_cash_amount=5e+7){
  now_amount<-function(user_list,target_ratio){
    user_list[,target_amount:=(premium*target_ratio-exp_match_amount)%>%pmax(.,0)]
    return(sum(user_list$target_amount))
  }
  
  up = target_ratio = 1
  down = 0
  max_cash_amount = now_cash_amount = now_amount(user_list,target_ratio)
  
  if(now_cash_amount>target_cash_amount+tol){
    stepnum = 0
    up = target_ratio
    step = abs(1/2*(up+down)-target_ratio)
    target_ratio = 1/2*(up+down)
    while(stepnum<=4 & step>=0.0005){
      now_cash_amount = now_amount(user_list,target_ratio)
      if(now_cash_amount>target_cash_amount+tol){
        up = target_ratio
        step = abs(1/2*(up+down)-target_ratio)
        target_ratio = 1/2*(up+down)
      }else if(now_cash_amount<target_cash_amount-tol){
        down = target_ratio
        step = abs(1/2*(up+down)-target_ratio)
        target_ratio = 1/2*(up+down)
      }else{
        break
      }
      stepnum = stepnum+1
    }
    print(paste("stepnum :",stepnum,"!"))
    return(list(final_ratio = target_ratio,
                remainCash = now_cash_amount))
  }else{
    print("there is no enough cash for target_cash_amount!")
    return(list(final_ratio = 1,
                remainCash = max_cash_amount))
  }
}