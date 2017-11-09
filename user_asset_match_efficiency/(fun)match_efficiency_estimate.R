# The input rawdata is produced by (fun)asset_match_log_mining_bat.r

match_efficiency_esimate<-function(rawdata,ratio_from=0,ratio_end=80,bw=100,ntest=512,unit=50,doplot=F,cdf_threshold=0.99,plotpath=NULL){
  library(data.table)
  library(magrittr)
  rawdata[,":="(start_date=as.Date(start_date),
                end_date=as.Date(end_date),
                differ_ratio=end_ratio-start_ratio)]
  rawdata[,differ_date:=ifelse(as.numeric(end_date-start_date)==0,0.5,as.numeric(end_date-start_date))]
  
  #为减小浮点运算的误差，对时间进行了放大（单位进行了缩小，sec）
  o2<-rawdata[differ_ratio>=5,]%>%
    .[,":="(start_ratio=round(start_ratio,0),
            per_ratio_sec=round(differ_date/differ_ratio*86400))]
  #创建细小的匹配区间，用以统计各自区间的匹配效率。
  startseq<-ratio_from:(ratio_end-1)
  endseq<-startseq+1
  
  #循环准备
  n<-length(startseq)
  r<-1
  for(i in 1:n){
    start<-startseq[i]
    end<-endseq[i]
    #对单一细小区间的匹配效率进行密度估计
    temp<-o2[start_ratio<=start & end_ratio>=end,]$per_ratio_sec
    d<-density(temp,from = 0,to=unit*ntest,bw=bw,n=ntest)
    cdf<-cumsum(unit*d$y)
    #unit固定，寻找满足cdf_threhold的最小样本个数,from=0固定，寻找to，使得cdf(from,to)>=threshold
    while(max(cdf)<cdf_threshold){
      ntest=2*ntest
      d<-density(temp,from = 0,to=unit*ntest,bw=bw,n=ntest)
      cdf<-cumsum(unit*d$y)
    }
    #对cdf归一化（因为区间截取，所以整体cdf和不是1，是threshold）
    sumProb<-sum(d$y*unit)
    #density=>pdf（非连续，实质是离散化的，确切说是分布律，单位宽度*密度值）
    y<-d$y*(1/sumProb)*unit
    #利用卷积求叠加的pdf（还是离散化的）
    r<-convolve(r,rev(y),type="o")
    #做变化图
    if(doplot){
      png(filename=paste0(plotpath,"\\0-",end,".png"),width =1200,height=468,units ="px")
      plot(x=seq(1:length(r))*unit,y=r,type="l",main = paste0("0-",end,"%"),xlab = "secs",ylab="density")
      dev.off()
    }
  }
  #r/unit是为了转回密度值
  model<-approx(x=seq(1:length(r))*unit,y=r/unit,n=length(r))
}


# for(i in 1:n){
#   start<-startseq[i]
#   end<-endseq[i]
#   #对单一细小区间的匹配效率进行密度估计
#   temp<-o2[start_ratio<=start & end_ratio>=end,]$per_ratio_sec
#   png(filename=paste0(plotpath,"\\density\\",start,"-",end,".png"),width =1200,height=468,units ="px")
#   plot(density(temp,from=0,bw=100),main = paste(start,"-",end),xlab = "sec")
#   dev.off()
# }

