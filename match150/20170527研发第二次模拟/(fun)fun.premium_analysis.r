fun.premium_analysis<-function(user_premium,num=1000,start=1000,end){
  if(missing(end)){
    end=max(user_premium)
  }
  cycle_seq<-seq(from=start,to=end,length.out = num)
  y<-rep(0.0,num)
  y[1]=sum(user_premium[user_premium<=start])
  for(i in 2:num){
    y[i]<-y[i-1]+sum(user_premium[user_premium>cycle_seq[i-1]&user_premium<=cycle_seq[i]])
  }
  return(data.table(x=cycle_seq,y=y))
}