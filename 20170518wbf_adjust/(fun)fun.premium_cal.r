fun.premium_cal<-function(premium_change,days,rate){
 s1=0
 n=length(premium_change)
 result<-rep(0,n)
 for(i in 1:n){
    s0 = s1+premium_change[i]
    s1 = s0+(pmax(s0,0)*(1+rate)^days[i]-pmax(s0,0))
    result[i] = s1
 }
 result
}