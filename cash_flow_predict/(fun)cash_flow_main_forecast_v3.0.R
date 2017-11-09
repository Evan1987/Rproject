
# v3.0是对v2.0的版本，brush和newcomer用户只进行amount预估，而不估计其具体计算参数。
# v2.0是对v1.0的详细版本，支持今后简便的参数细化。需要仔细考量各个参数训练的详细时间跨度。
cash_flow_main_forecast<-function(S_type_trainTimeSpan=c("2016-02-01","2017-02-27"),
                                  S_type_num_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  
                                  A_type_trainTimeSpan=c("2016-02-01","2017-02-27"),
                                  A_type_num_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  
                                  B_type_trainTimeSpan=c("2016-02-01","2017-02-27"),
                                  B_type_num_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  
                                  C_type_trainTimeSpan=c("2016-02-01","2017-02-27"),
                                  C_type_num_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  
                                  D_type_trainTimeSpan=c("2016-02-01","2017-02-27"),
                                  D_type_num_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  
                                  brush_type_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  brush_type_num_trainTimeSpan=c("2017-02-10","2017-02-27"),
                                  
                                  newcomer_type_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  newcomer_type_num_trainTimeSpan=c("2017-02-10","2017-02-27"),
                                  
                                  freshman_type_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  freshman_type_num_trainTimeSpan=c("2017-01-01","2017-02-27"),
                                  
                                  forecastDaySpan=c("2017-02-28","2017-03-10"),
                                  trainSetPath,calendarPath,detail=F)
{
  library(data.table)
  library(magrittr)
  library(tcltk)
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_train.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_num_train.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_forecast.R', encoding = 'UTF-8')
  source('~/rstudio/cash_flow_predict/(fun)cash_flow_type_num_forecast.R', encoding = 'UTF-8')
  
  # ******************************** 1. S 用户投资预测**************************************************
  # 1.1 S用户投资人数比例训练
  S_type_num_ratio_train<-cash_flow_type_train(item = "type_num_ratio",
                                               user_type = "S",
                                               trainTimeSpan = S_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 1.2 S用户单人投资额度训练
  S_amount_per_num_train<-cash_flow_type_train(item = "amount_per_num",
                                               user_type = "S",
                                               trainTimeSpan = S_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 1.3 S用户人数训练
  S_type_num_train<-cash_flow_type_num_train(user_type = "S",
                                             trainTimeSpan = S_type_num_trainTimeSpan,
                                             trainSetPath = trainSetPath,
                                             calendarPath = calendarPath)
  
  # 1.4 S用户投资人数比例预测
  S_type_num_ratio_forecast<-cash_flow_type_forecast(S_type_num_ratio_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 1.5 S用户单人投资额度预测
  S_amount_per_num_forecast<-cash_flow_type_forecast(S_amount_per_num_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 1.6 S用户人数预测
  S_type_num_forecast<-cash_flow_type_num_forecast(S_type_num_train,
                                                   forecastDaySpan = forecastDaySpan)
  
  # 1.7 S用户投资预测结果
  S_result<-S_type_num_ratio_forecast[S_amount_per_num_forecast,
                                      on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","total","i.total","non_res_total","i.non_res_total")]%>%
    .[S_type_num_forecast,on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","type","type_num","total","i.total","non_res_total","i.non_res_total")]
  
  colnames(S_result)<-c("date",
                        "type",
                        "type_num",
                        "type_num_ratio",
                        "amount_per_num",
                        "pure_type_num_ratio",
                        "pure_amount_per_num")
  
  S_result[,":="(amount=type_num*type_num_ratio*amount_per_num,
                 pure_amount=type_num*pure_type_num_ratio*pure_amount_per_num)]
  print("S succeed!")
  
  # ******************************** 2. A 用户投资预测**************************************************
  # 2.1 A用户投资人数比例训练
  A_type_num_ratio_train<-cash_flow_type_train(item = "type_num_ratio",
                                               user_type = "A",
                                               trainTimeSpan = A_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 2.2 A用户单人投资额度训练
  A_amount_per_num_train<-cash_flow_type_train(item = "amount_per_num",
                                               user_type = "A",
                                               trainTimeSpan = A_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 2.3 A用户人数训练
  A_type_num_train<-cash_flow_type_num_train(user_type = "A",
                                             trainTimeSpan = A_type_num_trainTimeSpan,
                                             trainSetPath = trainSetPath,
                                             calendarPath = calendarPath)
  
  # 2.4 A用户投资人数比例预测
  A_type_num_ratio_forecast<-cash_flow_type_forecast(A_type_num_ratio_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 2.5 A用户单人投资额度预测
  A_amount_per_num_forecast<-cash_flow_type_forecast(A_amount_per_num_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 2.6 A用户人数预测
  A_type_num_forecast<-cash_flow_type_num_forecast(A_type_num_train,
                                                   forecastDaySpan = forecastDaySpan)
  
  # 2.7 A用户投资预测结果
  A_result<-A_type_num_ratio_forecast[A_amount_per_num_forecast,
                                      on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","total","i.total","non_res_total","i.non_res_total")]%>%
    .[A_type_num_forecast,on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","type","type_num","total","i.total","non_res_total","i.non_res_total")]
  
  colnames(A_result)<-c("date",
                        "type",
                        "type_num",
                        "type_num_ratio",
                        "amount_per_num",
                        "pure_type_num_ratio",
                        "pure_amount_per_num")
  
  A_result[,":="(amount=type_num*type_num_ratio*amount_per_num,
                 pure_amount=type_num*pure_type_num_ratio*pure_amount_per_num)]
  
  print("A succeed!")
  
  # ******************************** 3. B 用户投资预测**************************************************
  # 3.1 B用户投资人数比例训练
  B_type_num_ratio_train<-cash_flow_type_train(item = "type_num_ratio",
                                               user_type = "B",
                                               trainTimeSpan = B_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 3.2 B用户单人投资额度训练
  B_amount_per_num_train<-cash_flow_type_train(item = "amount_per_num",
                                               user_type = "B",
                                               trainTimeSpan = B_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 3.3 B用户人数训练
  B_type_num_train<-cash_flow_type_num_train(user_type = "B",
                                             trainTimeSpan = B_type_num_trainTimeSpan,
                                             trainSetPath = trainSetPath,
                                             calendarPath = calendarPath)
  
  # 3.4 B用户投资人数比例预测
  B_type_num_ratio_forecast<-cash_flow_type_forecast(B_type_num_ratio_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 3.5 B用户单人投资额度预测
  B_amount_per_num_forecast<-cash_flow_type_forecast(B_amount_per_num_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 3.6 B用户人数预测
  B_type_num_forecast<-cash_flow_type_num_forecast(B_type_num_train,
                                                   forecastDaySpan = forecastDaySpan)
  
  # 3.7 B用户投资预测结果
  B_result<-B_type_num_ratio_forecast[B_amount_per_num_forecast,
                                      on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","total","i.total","non_res_total","i.non_res_total")]%>%
    .[B_type_num_forecast,on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","type","type_num","total","i.total","non_res_total","i.non_res_total")]
  
  colnames(B_result)<-c("date",
                        "type",
                        "type_num",
                        "type_num_ratio",
                        "amount_per_num",
                        "pure_type_num_ratio",
                        "pure_amount_per_num")
  
  B_result[,":="(amount=type_num*type_num_ratio*amount_per_num,
                 pure_amount=type_num*pure_type_num_ratio*pure_amount_per_num)]
  print("B succeed!")
  
  # ******************************** 4. C 用户投资预测**************************************************
  # 4.1 C用户投资人数比例训练
  C_type_num_ratio_train<-cash_flow_type_train(item = "type_num_ratio",
                                               user_type = "C",
                                               trainTimeSpan = C_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 4.2 C用户单人投资额度训练
  C_amount_per_num_train<-cash_flow_type_train(item = "amount_per_num",
                                               user_type = "C",
                                               trainTimeSpan = C_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 4.3 C用户人数训练
  C_type_num_train<-cash_flow_type_num_train(user_type = "C",
                                             trainTimeSpan = C_type_num_trainTimeSpan,
                                             trainSetPath = trainSetPath,
                                             calendarPath = calendarPath)
  
  # 4.4 C用户投资人数比例预测
  C_type_num_ratio_forecast<-cash_flow_type_forecast(C_type_num_ratio_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 4.5 C用户单人投资额度预测
  C_amount_per_num_forecast<-cash_flow_type_forecast(C_amount_per_num_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 4.6 C用户人数预测
  C_type_num_forecast<-cash_flow_type_num_forecast(C_type_num_train,
                                                   forecastDaySpan = forecastDaySpan)
  
  # 4.7 C用户投资预测结果
  C_result<-C_type_num_ratio_forecast[C_amount_per_num_forecast,
                                      on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","total","i.total","non_res_total","i.non_res_total")]%>%
    .[C_type_num_forecast,on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","type","type_num","total","i.total","non_res_total","i.non_res_total")]
  
  colnames(C_result)<-c("date",
                        "type",
                        "type_num",
                        "type_num_ratio",
                        "amount_per_num",
                        "pure_type_num_ratio",
                        "pure_amount_per_num")
  
  C_result[,":="(amount=type_num*type_num_ratio*amount_per_num,
                 pure_amount=type_num*pure_type_num_ratio*pure_amount_per_num)]
  print("C succeed!")
  
  # ******************************** 5. D 用户投资预测**************************************************
  # 5.1 D用户投资人数比例训练
  D_type_num_ratio_train<-cash_flow_type_train(item = "type_num_ratio",
                                               user_type = "D",
                                               trainTimeSpan = D_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 5.2 D用户单人投资额度训练
  D_amount_per_num_train<-cash_flow_type_train(item = "amount_per_num",
                                               user_type = "D",
                                               trainTimeSpan = D_type_trainTimeSpan,
                                               trainSetPath = trainSetPath,
                                               calendarPath = calendarPath)
  # 5.3 D用户人数训练
  D_type_num_train<-cash_flow_type_num_train(user_type = "D",
                                             trainTimeSpan = D_type_num_trainTimeSpan,
                                             trainSetPath = trainSetPath,
                                             calendarPath = calendarPath)
  
  # 5.4 D用户投资人数比例预测
  D_type_num_ratio_forecast<-cash_flow_type_forecast(D_type_num_ratio_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 5.5 D用户单人投资额度预测
  D_amount_per_num_forecast<-cash_flow_type_forecast(D_amount_per_num_train,
                                                     forecastDaySpan = forecastDaySpan,
                                                     calendarPath = calendarPath)
  # 5.6 D用户人数预测
  D_type_num_forecast<-cash_flow_type_num_forecast(D_type_num_train,
                                                   forecastDaySpan = forecastDaySpan)
  
  # 5.7 D用户投资预测结果
  D_result<-D_type_num_ratio_forecast[D_amount_per_num_forecast,
                                      on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","total","i.total","non_res_total","i.non_res_total")]%>%
    .[D_type_num_forecast,on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","type","type_num","total","i.total","non_res_total","i.non_res_total")]
  
  colnames(D_result)<-c("date",
                        "type",
                        "type_num",
                        "type_num_ratio",
                        "amount_per_num",
                        "pure_type_num_ratio",
                        "pure_amount_per_num")
  
  D_result[,":="(amount=type_num*type_num_ratio*amount_per_num,
                 pure_amount=type_num*pure_type_num_ratio*pure_amount_per_num)]
  
  print("D succeed!")
  
  # ******************************** 6. brush 用户投资预测**************************************************
  # 6.1 brush用户投资额度训练
  brush_amount_train<-cash_flow_type_train(item = "amount",
                                                   user_type = "brush",
                                                   trainTimeSpan = brush_type_trainTimeSpan,
                                                   trainSetPath = trainSetPath,
                                                   calendarPath = calendarPath)
  # 6.2 brush用户人数训练
  brush_type_num_train<-cash_flow_type_num_train(user_type = "brush",
                                                 trainTimeSpan = brush_type_num_trainTimeSpan,
                                                 trainSetPath = trainSetPath,
                                                 calendarPath = calendarPath)
  
  # 6.3 brush用户投资额度预测
  brush_amount_forecast<-cash_flow_type_forecast(brush_amount_train,
                                                         forecastDaySpan = forecastDaySpan,
                                                         calendarPath = calendarPath)
  # 6.4 brush用户人数预测
  brush_type_num_forecast<-cash_flow_type_num_forecast(brush_type_num_train,
                                                       forecastDaySpan = forecastDaySpan)
  
  # 6.5 brush用户投资预测结果
  brush_result<-brush_type_num_forecast[brush_amount_forecast,
                                              on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,":="(type_num_ratio=NA,
            amount_per_num=NA,
            pure_type_num_ratio=NA,
            pure_amount_per_num=NA,
            amount=total,
            pure_amount=non_res_total)]%>%
    .[,c("date","type","type_num","type_num_ratio","amount_per_num",
         "pure_type_num_ratio","pure_amount_per_num","amount","pure_amount")]
  
  print("brush succeed!")
  
  # ******************************** 7. newcomer 用户投资预测**************************************************
  # 7.1 newcomer用户投资额度训练
  newcomer_amount_train<-cash_flow_type_train(item = "amount",
                                           user_type = "newcomer",
                                           trainTimeSpan = newcomer_type_trainTimeSpan,
                                           trainSetPath = trainSetPath,
                                           calendarPath = calendarPath)
  # 7.2 newcomer用户人数训练
  newcomer_type_num_train<-cash_flow_type_num_train(user_type = "newcomer",
                                                 trainTimeSpan = newcomer_type_num_trainTimeSpan,
                                                 trainSetPath = trainSetPath,
                                                 calendarPath = calendarPath)
  
  # 7.3 newcomer用户投资额度预测
  newcomer_amount_forecast<-cash_flow_type_forecast(newcomer_amount_train,
                                                 forecastDaySpan = forecastDaySpan,
                                                 calendarPath = calendarPath)
  # 7.4 newcomer用户人数预测
  newcomer_type_num_forecast<-cash_flow_type_num_forecast(newcomer_type_num_train,
                                                       forecastDaySpan = forecastDaySpan)
  
  # 7.5 newcomer用户投资预测结果
  newcomer_result<-newcomer_type_num_forecast[newcomer_amount_forecast,
                                        on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,":="(type_num_ratio=NA,
            amount_per_num=NA,
            pure_type_num_ratio=NA,
            pure_amount_per_num=NA,
            amount=total,
            pure_amount=non_res_total)]%>%
    .[,c("date","type","type_num","type_num_ratio","amount_per_num",
         "pure_type_num_ratio","pure_amount_per_num","amount","pure_amount")]
  
  print("newcomer succeed!")
  
  # ******************************** 8. freshman 用户投资预测**************************************************
  # 8.1 freshman用户投资人数比例训练
  freshman_type_num_ratio_train<-cash_flow_type_train(item = "type_num_ratio",
                                                      user_type = "freshman",
                                                      trainTimeSpan = freshman_type_trainTimeSpan,
                                                      trainSetPath = trainSetPath,
                                                      calendarPath = calendarPath)
  # 8.2 freshman用户单人投资额度训练
  freshman_amount_per_num_train<-cash_flow_type_train(item = "amount_per_num",
                                                      user_type = "freshman",
                                                      trainTimeSpan = freshman_type_trainTimeSpan,
                                                      trainSetPath = trainSetPath,
                                                      calendarPath = calendarPath)
  # 8.3 freshman用户人数训练
  freshman_type_num_train<-cash_flow_type_num_train(user_type = "freshman",
                                                    trainTimeSpan = freshman_type_num_trainTimeSpan,
                                                    trainSetPath = trainSetPath,
                                                    calendarPath = calendarPath)
  
  # 8.4 freshman用户投资人数比例预测
  freshman_type_num_ratio_forecast<-cash_flow_type_forecast(freshman_type_num_ratio_train,
                                                            forecastDaySpan = forecastDaySpan,
                                                            calendarPath = calendarPath)
  # 8.5 freshman用户单人投资额度预测
  freshman_amount_per_num_forecast<-cash_flow_type_forecast(freshman_amount_per_num_train,
                                                            forecastDaySpan = forecastDaySpan,
                                                            calendarPath = calendarPath)
  # 8.6 freshman用户人数预测
  freshman_type_num_forecast<-cash_flow_type_num_forecast(freshman_type_num_train,
                                                          forecastDaySpan = forecastDaySpan)
  
  # 8.7 A用户投资预测结果
  freshman_result<-freshman_type_num_ratio_forecast[freshman_amount_per_num_forecast,
                                                    on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","total","i.total","non_res_total","i.non_res_total")]%>%
    .[freshman_type_num_forecast,on=c("date"="date"),nomatch=NA,mult="all"]%>%
    .[,c("date","type","type_num","total","i.total","non_res_total","i.non_res_total")]
  
  colnames(freshman_result)<-c("date",
                               "type",
                               "type_num",
                               "type_num_ratio",
                               "amount_per_num",
                               "pure_type_num_ratio",
                               "pure_amount_per_num")
  
  freshman_result[,":="(amount=type_num*type_num_ratio*amount_per_num,
                        pure_amount=type_num*pure_type_num_ratio*pure_amount_per_num)]
  print("freshman succeed!")
  
  # ******************************* 9. 最终预测结果汇总并输出*************************************
  result<-rbindlist(list(S_result,
                         A_result,
                         B_result,
                         C_result,
                         D_result,
                         brush_result,
                         newcomer_result,
                         freshman_result))
  if(detail){
    return(result)
  }
  else{
    result_summary<-result[,.(amount=sum(amount),pure_amount=sum(pure_amount)),by="date"]
    return(result_summary)
  }
}
