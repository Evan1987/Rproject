

# 给定入库时间和相应限制条件，计算资产最晚匹配完成时间的函数
#create_time：入库时间
#work_time：工作时间区间
#time_span：从入库到打款允许的最大时长
#free_span：给予人工操作的弛豫时间，即让匹配完成时间总是在
#[start_work_time + free_span, end_work_time - free_span]范围内

fun.asset_deadline<-function(create_time,
                         work_time=c(10,20),
                         time_span=24,
                         free_span=2)
{
  library(magrittr)
  library(lubridate)
  create_time=as.POSIXct(create_time)
  
  #入库时间小时化
  in_time=hour(create_time)+minute(create_time)/60+second(create_time)/3600
  #建立一个虚拟时间轴，包含：工作时间、第二日工作时间、入库时间和打款时间。
  #按照大小排序，取入库、打款时间之间的部分
  timeseq<-c(work_time,time_span+work_time,in_time,time_span+in_time)%>%
    sort(.)%>%
    .[which(.>=in_time&.<=in_time+time_span)]
  
  #t1是计算资产出库前当天耗用的工作时间长度
  #如果时间轴倒数第二项是工作开始时间，则有值，否则为0
  t1<-ifelse(rev(timeseq)[2]%%time_span==min(work_time),rev(timeseq)[1]-rev(timeseq)[2],0)
  #t2是计算最终匹配需完成的截止时间（小时化的HH:MM）
  #如果前面计算的t1大于所需的弛豫时间，则就在出库时间基础上减去弛豫时间
  #否则，则在最临近出库时间的工作结束时间前减去还缺少的弛豫时间
  t2<-ifelse(t1>=free_span,
             rev(timeseq)[1]-free_span,
             which(timeseq%%time_span==max(work_time))%>%max(.)%>%timeseq[.]-(free_span-t1))
  #t2只是一个HH:MM，需要加上日期，形成datetime格式
  deadline<-t2*3600+floor_date(create_time,unit = "day")
  return(deadline)
}

