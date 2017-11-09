myqqplot<-function(vector,...){
  y<-quantile(vector,seq(0,1,0.01))
  x<-qnorm(seq(0,1,0.01),mean(vector),sd(vector))
  plot(x,y,
       xlab="Theoretical Quantiles",
       ylab="Sample Quantiles",
       xlim=c(0,max(vector)),
       ylim=c(0,max(vector)),...)
  abline(0,1,col="red")
}