library(data.table)
library(magrittr)
library(dplyr)
library(readr)
library(stringr)
library(tcltk)
################################ 0. 数据挖掘准备 ####################################
# 函数：计算利率差异的忍受度（不爽度）
fun.tol<-function(general_rate,jlc_rate,yuebao_rate,zhenrongbao_rate,premium_cur){
  x=general_rate-jlc_rate
  y=jlc_rate-yuebao_rate
  z=jlc_rate-zhenrongbao_rate
  result1=(exp(-x)-1)*10
  result2=(exp(-(y-3))-1)*20
  result3=(exp(-z*2)-1)*15
  
  return(max(0,result1)+max(0,result2)+max(0,result3)+ifelse(premium_cur>=100,0,0.01))
}
vec.fun.tol<-Vectorize(fun.tol)

#logistic拟合模型因变量的构建函数
#a：y的最小值
#b：y的最大值
#拟合公式为: y=(b-a)/(1+exp(q(x-p))+a
fun.log_fit<-function(a,b,y){
  m<-((b-a)/ifelse(y<=a,0.01,y-a)-1)%>%round(.,2)
  result<-ifelse(m<=0,0.01,m)
  return(log(result))
}

path<-"F:/Project/20170406用户数量和存量额度的利率弹性/"
end_date=as.Date("2017-04-05")

raw_data<-read_csv(paste0(path,"raw_data.csv"))%>%
  arrange(.,userid,log_date)%>%
  as.data.table(.)
history_rate<-read_csv(paste0(path,"history_rate.csv"))%>%
    mutate(.,log_date=as.Date(log_date))%>%
    arrange(.,log_date)%>%as.data.table(.)

# confirmed_related_data<-read_csv(paste0(path,"confirmed_related_data.csv"))%>%
#   arrange(.,userid,log_date)%>%
#   as.data.table(.)

# users<-unique(confirmed_related_data$useid)


################################ 1. 循环处理 ####################################
users<-unique(raw_data$userid)
user_num<-length(users)
result<-data.table()
pb <- tkProgressBar("进度","已完成 %", 0, 100)

for(k in 1:user_num){
  user<-users[k]
  userdata<-raw_data[userid==user,]
  # 如果用户最大存量不足1000或者总的记录条数少于20，则不分析
  if(max(userdata$premium_cur,na.rm = T)<1000|nrow(userdata)<20){next}
  # 补全用户数据
  userdata<-userdata[,userid:=NULL]%>%
    {
      #将用户数据补全，并附上利率数据
      time_span<-c(min(.$log_date),end_date)
      .[history_rate[between(log_date,time_span[1],time_span[2]),],on=c("log_date"="log_date"),nomatch=NA,mult="all"]
    }%T>%
    {
      tmp<-.[,c("amount","premium_cur","premium_regular","premium")]
      .[,c("amount","premium_cur","premium_regular","premium")][is.na(tmp)]<-0
    }%T>%
    setorder(.,log_date)%>%
    # 计算用户实际利率
    .[,general_rate:=ifelse(amount*premium==0,jlc_rate,round(amount*365*100/premium,2))]%>%
    {
      #设定存量分辨率，分辨率设置比例0.1，最小为1000
      unit=(floor((max(.$premium_cur)-min(.$premium_cur))*0.1/1000)*1000)%>%
        max(.,1000)
      #用以设定break的数量
      num=floor(max(.$premium_cur)/unit)
      #设定合理的breaks序列
      breaks<-c(0,1000,1:num*unit,max(.$premium_cur)+unit)%>%unique(.)%>%sort(.)
      #给用户存量水平进行评价
      .[,premium_level:=cut(premium_cur,breaks = breaks,labels = 0:(length(breaks)-2),right=F)%>%as.character(.)%>%as.numeric(.)]
    }%>%
    # 计算用户每天的不爽度
    .[,tol:=vec.fun.tol(general_rate,jlc_rate,yuebao_rate,zhenrongbao_rate,premium_cur)]
  
  premium_level<-unique(userdata$premium_level)%>%sort(.,decreasing=T)
  
  result_temp<-data.table()
  # 按照用户存量水平降序来寻找存量阶跃模式
  for(i in rev(rev(premium_level)[-1])){
    temp<-copy(userdata)%>%
      .[,":="(level=premium_level-i,
              id=.I)]%>%
      .[,label:=ifelse(level>=0,1,0)]
    # 寻找最佳分析的阶跃模式（满足条件基础上越长越好）高地的起终点
    index<-{
      x<-str_c(temp$label,collapse = "")
      pattern<-{
        start<-str_locate_all(x,"1")[[1]][,1]%>%.[.<=nchar(x)-6]
        grid<-expand.grid(start,start,stringsAsFactors = F)%>%
          as.data.table(.)%T>%{names(.)<-c("start","end")}%>%
          # 阶跃模式高地的长度不得小于7
          .[end>=start+6,]%>%
          .[,length:=end-start+1]%T>%
          setorder(.,start,end)%>%
          .[,vec_pattern:=str_sub(x,start,end)]
      }%>%
        # 在高地中，可以存在少量level偏低的情况，但这种情况不得超过20%（80%稳定）
        .[str_count(vec_pattern,"1")>=0.8*nchar(vec_pattern),]%>%
        setorder(.,-length,start)%>%
        head(.,1)
    }
    #如果没有这样的高地区间，则下一个循环
    if(nrow(index)==0){next}
    start_index<-index$start
    end_index<-index$end
    #寻找有效分析区间的末位点
    idx<-tryCatch(min(temp[id>end_index&label==1,]$id),warning=function(w){return(Inf)})
    #如果可供分析的洼地区间长度太短，则下一个循环
    if(nrow(temp[id>end_index&id<idx,])<7){next}
    
    result_temp<-{
      # 确定有效分析区间的范围
      temp2<-temp[id>=start_index&id<idx,]%>%
        .[,cum_tol:=cumsum(tol)]
      # 确定洼地的存量level
      low_level<-temp2[id>end_index,]%>%
      {
        # 寻找洼地的原则是越低越好，=>不大于此level的样本量达到40%的最低level
        count_summary<-.[,.(num=.N),by=premium_level]%T>%setorder(.,premium_level)%>%
          .[,cum_num_ratio:=cumsum(num)/length(temp2[id>end_index,])]%>%
          .[cum_num_ratio>=0.4,]
        head(count_summary,1)$premium_level
      }
      #存量y的洼地均值
      a<-temp2[id>end_index&premium_level==low_level,]$premium_cur%>%mean(.,trim=0.1)%>%round(.,2)
      #存量y的高地均值
      b<-temp2[between(id,start_index,end_index),]$premium_cur%>%mean(.,trim=0.1)%>%round(.,2)
      #不爽度x的变化阈值，为高地末端与洼地首端之间的累积不爽度均值
      p<-temp2[id>=end_index,]%>%{
        level_idx<-.[premium_level==low_level,]$id%>%min(.)
        .[id<=level_idx,]
      }%>%.$cum_tol%>%mean(.)%>%round(.,2)
      
      #不爽度x的变化敏感度，通过不带截距的线性拟合得到
      q<-temp2[,":="(fity=fun.log_fit(a=a,b=b,y=premium_cur),fitx=cum_tol-p),by=.I]%>%
      {
        model<-lm(data = .,fitx ~ -1+fity)
        round(1/model$coefficients,2)
      }
      #输出结果
      data.table(userid=user,min=a,max=b,interface=p,slope=q,hold_days=end_index-start_index+1)
    }
    break
  }
  if(nrow(result_temp)>0&result_temp$slope>0){result<-rbind(result,result_temp)}
  info<- sprintf("已完成 %.2f%%", round(k*100/user_num,2))  
  setTkProgressBar(pb, k*100/user_num, sprintf("进度 (%s)", info),info)
}

#根据结果统计，确定的筛选规则
result_true<-result[slope>0&slope<1&interface>50&interface<2000,]
#作图
# curve((result$max-result$min)/(1+exp((x-result$interface)/result$slope))+result$min,
#       xlim=c(0,2000),main=paste(user,"premium_cur simulation"),xlab = "tolerance",ylab = "premium_cur")
