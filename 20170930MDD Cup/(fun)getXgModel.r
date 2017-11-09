getXgModel<-function(features,labels,trainDF,params){
  library(data.table)
  library(magrittr)
  library(xgboost)
  
  x_train = trainDF[,.SD,.SDcols=features]%>%as.matrix(.)
  y_train = trainDF[,.SD,.SDcols=labels]%>%as.matrix(.)
  dtrain = xgb.DMatrix(x_train,label=y_train)
  watchlist = list("train"=dtrain)
  xgbModel = xgb.train(params = params,
                       nrounds = params$num_round,
                       data = dtrain,
                       watchlist = watchlist,
                       verbose = 1)
  return(xgbModel)
}