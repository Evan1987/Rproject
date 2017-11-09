
# 根据rawdata输出最终页面点击表的函数
# rawdata：在原始sql提取后的简单处理，必有字段：userid,log_time,activitytype,module
# sessionTimes：分割会话所用的阈值，即如果stay_time超过这个值则被分成另外一个会话
# slimTimeInfo: 包含用户时间区间筛选信息，只能有三字段：userid,start,end其中后两个字段必为时间格式
# notClickEvent：为行为筛选提供的列表，默认会从WD下读取
fun.pageAnalysis<-function(rawdata,sessionTimes=120,slimTimeInfo,notClickEvent){
  
  judge<-ifelse(missing(slimTimeInfo),0,1)

  # 1. 处理数据，筛减，计时，划分会话，为输出用户点击路径信息做准备。
  slimdata<-rawdata%T>%
    setkey(.,userid,log_time)%T>%
    {
      # 根据给定的用户时间区间筛选数据
      if(judge){
        setnames(slimTimeInfo,names(slimTimeInfo),c("userid","start","end"))
        .[slimTimeInfo,on="userid",nomatch=0]%>%
        .[(is.na(start)|log_time>=start)&(is.na(end)|log_time<=end),]%>%
        .[,-c("start","end")]
      }
    }%>%
    .[module!="open",]%>%
    # 计算每个点击页面的停留时间
    .[,stay_time:=c(NA,as.numeric(diff(log_time)))%>%dplyr::lead(.,default = sessionTimes),by=userid]%>%
    .[,id:=seq_len(.N),by=userid]%>%
    # 标识结束点击
    .[activitytype=="exit002"|stay_time>=sessionTimes,isEnd:=1]%>%{
      # 标识开始点击
      y<-.[,.SD[J(id=c(.SD[isEnd==1,]$id+1,1)),on="id",nomatch=0],by=userid]
      .[y,on=c("userid","id"),isStart:=1]
    }%>%
    .[,isStart:=replace(isStart,is.na(isStart),0)]%>%
    # 划分session
    .[,sessionID:=cumsum(isStart),by=userid]%>%
    # 去掉无效session（一个回话里只有退出）
    .[!(isStart==1&isEnd==1&module=="exit"),]
  
  print("data prepared OK!")
  
  # 2. 对准备好的数据进行改造，输出点击路径信息
  
  # 属于非点击的行为列表，和非首页列表
  if(missing(notClickEvent)){
    notClickEvent<-read.csv("notClickEvent.csv",stringsAsFactors = F)
  }
  notClickList<-c(notClickEvent$event,"homepage00")
  falseStartList<-notClickList%>%.[which(str_detect(.,"homepage"))]
  
  # 去掉非点击且不是会话结束的行为
  pageAnalysis<-slimdata[!(activitytype%in%notClickList&is.na(isEnd)),
                         c("userid","module","activitytype","isEnd","isStart","sessionID")]%T>%
    setnames(.,"activitytype","endPage")%>%
    .[,id:=.I]%>%
    # 为每一会话补充统一的原始开头页：homepage00，这样会使原会话最后一个行为对应的endpage为NA
    .[,.SD[J(id=min(id)-1+seq_len(.N+1),startPage=c("homepage00",endPage)),
           on="id",
           nomatch=NA],
      by=.(userid,sessionID)]%>%
    # startpage不能是exit002，如果是说明原会话已有exit002，不必为其补充
    .[startPage!="exit002",]%>%
    # 为会话补充统一的结束页exit002
    .[is.na(endPage),":="(module="exit",endPage="exit002")]%>%
    # 重新标识结束点
    .[,isEnd:=ifelse(endPage=="exit002",1,0)]%>%
    # 删除startpage和endpage都在非首页列表且不是结束点击的数据=>这个路径内无点击
    # 如果这个数据发生在开始页，一般是原开始页都有合理的首页点击信息，这个会在最后给予统一
    .[!((startPage%in%falseStartList)&(endPage%in%falseStartList)&isEnd==0),]%>%
    # 为每一会话增加点击排序
    .[,clickno:=seq_len(.N),by=.(userid,sessionID)]%>%
    # 统一开始页
    .[clickno==1,startPage:=replace(startPage,str_detect(startPage,"home"),"homepage00")]
  return(pageAnalysis)
}