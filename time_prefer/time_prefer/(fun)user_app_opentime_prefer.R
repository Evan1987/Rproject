user_app_opentime_prefer<-function(user_log,n=96,P_value_threshold=0.99){
  library(data.table)
  library(sqldf)

  o1<-copy(user_log)  
  setkey(o1,userid)
  all_users<-unique(o1$userid)
  user_num<-length(all_users)
  result<-data.table()
  for (j in 1:user_num){
    user_aa<-all_users[j]
    open_aa<-o1[user_aa,nomatch=NA,mult="all"]
    open_day_freq<-length(unique(open_aa$day))
    if(open_day_freq>=20){
      x<-open_aa$time_dist
      #sample num
      N<-length(x)
      #divide pieces num
      n<-n
      #expected p and q value for binomial distribution(single dot distribution on uniform distribution)
      p<-1/n
      q<-1-p
      #generate kernel density estimation(default Gaussian)
      time_density<-density(x,bw=24/n,n=n,from = 0.25,to=24)
      #density estimation result
      time_curve<-data.table(id=seq.int(from = 1,by=1,length.out = 96),time=time_density$x,freq=time_density$y)
      #the threshold value to deny origin Hypothesis(uniform distribution)
      threshold<-1-P_value_threshold
      #the threshold value transfered to Normal z value
      z_threshold<-qnorm(threshold)
      time_curve[,":="(p_value=pnorm(abs(freq*24/n-p)*N/sqrt(N*p*q)),
                       prefer=ifelse((freq*24/n-p)*N/sqrt(p*q*N)>=z_threshold,1,0))]
      
      time_prefer<-time_curve[prefer==1,]
      time_prefer[,id_add:=seq(from=1,by=1,length.out = dim(time_prefer)[1])]
      time_prefer[,id_diff:=id-id_add]
      
      z1<-"select id_diff,sum(1) as num from time_prefer group by id_diff"
      id_prefer_zone<-as.data.table(sqldf(z1))
      id_prefer_zone<-id_prefer_zone[num>=3,]
      
      time_prefer_zone<-data.table()
      for (i in 1:dim(id_prefer_zone)[1]) {
        temp<-time_prefer[id_diff==id_prefer_zone$id_diff[i],]
        log<-data.table(start=min(temp$time),end=max(temp$time),confidence=pnorm((mean(temp$freq)-p)/sqrt(p*q/N)))
        time_prefer_zone<-rbind(time_prefer_zone,log)
      }
      time_prefer_zone<-data.table(userid=user_aa,freq=open_day_freq,time_prefer_zone)  
    }
    else{
      time_prefer_zone<-data.table(userid=user_aa,freq=open_day_freq,start=NA,end=NA,confidence=NA)
    }
    
    result<-rbind(result,time_prefer_zone)
    print(paste(j,"/",user_num,"have been treated successfully!"))
  }
  return(result)
}