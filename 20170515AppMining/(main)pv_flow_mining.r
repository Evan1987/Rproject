library(data.table)
library(magrittr)
library(stringr)
library(readr)
path = "F:/Project/20170515用户app访问数据/"

slimdata<-fread(paste0(path,"slimdata.csv"))%>%as.data.table(.)
notClickEvent<-fread(paste0(path,"notClickEvent.csv"))
notClickList<-c(notClickEvent$event,"homepage00")

falseStartList<-notClickList%>%.[which(str_detect(.,"homepage"))]


# 补齐页面路径的首尾
pageAnalysis2<-slimdata[!(activitytype%in%notClickList&is.na(isEnd)),
                        c("userid","module","activitytype","isEnd","isStart","sessionID")]%T>%
  setnames(.,"activitytype","endPage")%>%
  .[,id:=.I]%>%
  .[,.SD[J(id=min(id)-1+seq_len(.N+1),startPage=c("homepage00",endPage)),
         on="id",
         nomatch=NA],
    by=.(userid,sessionID)]%>%
  .[startPage!="exit002",]%>%
  .[is.na(endPage),":="(module="exit",endPage="exit002")]%>%
  .[,isEnd:=ifelse(endPage=="exit002",1,0)]%>%
  .[!((startPage%in%falseStartList)&(endPage%in%falseStartList)&isEnd==0),]%>%
  .[,clickno:=seq_len(.N),by=.(userid,sessionID)]%>%
  .[clickno==1,startPage:=replace(startPage,str_detect(startPage,"home"),"homepage00")]

write.csv(pageAnalysis2,paste0(path,"pageAnalysis2.csv"),row.names = F)
# 确定需要查看路径的层数=>10
# sessionSummary<-pageAnalysis[,.(PV=.N),by=.(userid,sessionID)]
# quantile(sessionSummary$PV,probs = seq(0,1,0.1)
# uniqueN(pageAnalysis[,c("userid","sessionID")])
# table(slimdata$pageno)

library(data.table)
library(magrittr)
library(readr)
library(stringr)
source('~/rstudio/20170515AppMining/(fun)fun.pageAnalysis.r', encoding = 'UTF-8')
source('~/rstudio/20170515AppMining/(fun)fun.pvflow.r', encoding = 'UTF-8')
path = "F:/Project/20170515用户app访问数据/"

# partial users 2017-02-01===2017-05-01
my_user_info<-read_csv(paste0(path,"user_info.csv"))%>%
  as.data.table(.)%>%
  .[reg_time>=as.POSIXct("2017-02-01 0:00:00"),]%>%
  .[,isinvited:=ifelse(is.na(invite_user_id),0,1)]%>%
  .[,-"invite_user_id"]
aa<-my_user_info[,c("userid","reg_time","invest1st_time")]
# the app log of partial users above
rawdata<-read_csv(paste0(path,"rawdata_treated.csv"))%>%as.data.table(.)
pageAnalysis<-fun.pageAnalysis(rawdata,slimTimeInfo = aa)
result<-fun.pvflow(data=pageAnalysis2,stage = 5)



##################################  sankey plot ################################
library(data.table)
library(magrittr)
library(networkD3)
library(stringr)
path = "F:/Project/20170515用户app访问数据/"

app_point_info<-fread(paste0(path,"app_point_info.csv"))

result<-fread(paste0(path,"result/result.csv"))%>%.[stage<=5,]%>%
  .[,":="(fixed_startPage=str_c(startPage,"_",stage-1),fixed_endPage=str_c(endPage,"_",stage))]%>%
  .[,fixed_endPage:=replace(fixed_endPage,str_detect(fixed_endPage,"exit"),"exit002_0")]


fun.module_transfer<-function(unfixed_node){
  switch(unfixed_node,
         "homepage00"="homepage",
         "others"="others",
         "unknown")
}

nodes<-data.table(node=unique(c(result$fixed_startPage,result$fixed_endPage)))%>%
  .[,id:=.I-1]%>%
  .[,unfixed_node:=str_sub(node,start = 1,end = -3)]%>%
  .[app_point_info,on=c("unfixed_node"="event"),module:=i.module]%>%
  .[is.na(module),module:=sapply(unfixed_node,fun.module_transfer)]
  
links<-result[,c("fixed_startPage","fixed_endPage","pv")]%>%
  .[nodes,on=c("fixed_startPage"="node"),start:=i.id]%>%
  .[nodes,on=c("fixed_endPage"="node"),end:=i.id]%>%
  .[,-c("fixed_startPage","fixed_endPage")]

sankeyNetwork(Links = links,
              Nodes = nodes,
              Source = "start",
              Target = "end",
              Value = "pv",
              NodeID = "node",
              NodeGroup = "module",
              fontSize = 10,
              nodeWidth = 50,
              width = 1000,
              height =3000,
              sinksRight = T)

