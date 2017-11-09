
# 根据打点信息输出点击路径流量情况的函数
# data:pageAnalysis，必有字段：startPage、endPage、clickno
# page：标识从哪页开始探究，一般都是首页（homepage00）
# stage：表示点击路径的探索深度
# min_cumratio：表示每一层向下探索时，选取的主要页面（PV降序排列，cum pv_ratio要超过这个值）

fun.pvflow<-function(data,page="homepage00",stage=5,min_cumratio=0.8){
  
  # 单层探究函数，针对某一特定层进行输出
  fun.pvflowSingle<-function(data,page,clickseq,min_cumratio){
    focus<-data[clickno==clickseq&startPage==page,]
    result<-focus[,.(pv=.N),by=.(startPage,endPage)]%>%
    {
      tmp<-focus[,.(total_pv=.N),by=startPage]
      .[tmp,on="startPage",pv_ratio:=round(pv/total_pv,4)]
    }%T>%
      setorder(.,-pv_ratio)%>%
      .[,cum_pv_ratio:=cumsum(pv_ratio)]%>%
      {
        tmp<-split(.,(.$cum_pv_ratio<=min_cumratio|.$endPage=="exit002"))
        x<-data.table()
        for(i in 1:length(names(tmp))){
          a<-switch(names(tmp)[i],
                    "TRUE" = tmp[[i]]%>%.[,num:=1]%>%.[,-"cum_pv_ratio"],
                    "FALSE"= tmp[[i]]%>%.[,.
                                          (pv_ratio=sum(pv_ratio),pv=sum(pv),num=.N),
                                          by=startPage]%>%
                                        .[,endPage:="others"])
          x<-rbind(x,a)
        }
        x
      }
    return(result)
  }
  
  result<-fun.pvflowSingle(data,page,clickseq = 1,min_cumratio)%>%.[,stage:=1]
  
  i=2
  while(i<=stage){
    pagelist<-result[(stage==i-1)&(!endPage%in%c("others","exit002")),]$endPage%>%unique(.)
    tmp<-data.table()
    for(j in pagelist){
      result_temp<-fun.pvflowSingle(data,page=j,clickseq = i,min_cumratio = min_cumratio)
      tmp<-rbind(tmp,result_temp)
    }
    tmp[,stage:=i]
    result<-rbind(result,tmp)
    i=i+1
  }
  return(result)
}