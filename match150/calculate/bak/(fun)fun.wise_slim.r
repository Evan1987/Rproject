fun.wise_slim<-function(user_list_pre,
                        remainTotalNum,
                        ref_ratio,
                        target_amount,
                        target_asset,
                        min_breaks=100,
                        slim_contribution = c(3,3,4),
                        by){
  library(data.table)
  library(magrittr)
  source('~/rstudio/match150/calculate/(fun)fun.wise_slim_by_amount.r', encoding = 'UTF-8',echo=TRUE)
  source('~/rstudio/match150/calculate/(fun)fun.wise_slim_by_asset.r', encoding = 'UTF-8', echo=TRUE)
  if(missing(by)){
    if(!missing(target_amount)){
      by = "amount"
    }else if(!missing(target_asset)){
      by = "asset"
    }
  }
  
  result<-
  switch(by,
         "amount"=fun.wise_slim_by_amount(user_list_pre = user_list_pre,
                                          ref_ratio = ref_ratio,
                                          target_amount = target_amount,
                                          slim_contribution = slim_contribution,
                                          min_breaks = min_breaks),
         "asset" = fun.wise_slim_by_asset(user_list_pre = user_list_pre,
                                          ref_ratio = ref_ratio,
                                          remainTotalNum = remainTotalNum,
                                          target_asset = target_asset,
                                          min_breaks = 0,
                                          slim_contribution = slim_contribution))
  return(result)
}