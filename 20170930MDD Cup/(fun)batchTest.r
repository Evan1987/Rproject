batchTest<-function(trainDF,testDF,features,labels,params,workType=c("test","output")){
  library(xgboost)
  library(data.table)
  source('~/rstudio/20170930MDD Cup/(fun)maeEval.r', echo=TRUE)
  source('~/rstudio/20170930MDD Cup/(fun)getXgModel.r', echo=TRUE)
  source('~/rstudio/20170930MDD Cup/(fun)getXgSub.r', echo=TRUE)
  workType = match.arg(workType)
  
  xgModel = getXgModel(features,labels,trainDF,params)
  sub = getXgSub(xgModel,testDF,avail_features,labels)
  if(workType == "test"){
    maeResult = maeEval(sub,testDF)
    return(list("model"=xgModel,"mae"=maeResult$mae,"result" = maeResult$result))
  }
  
  if(workType=="output"){
    return(list("model"=xgModel,"result" = sub))
  }
}