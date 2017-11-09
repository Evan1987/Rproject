findAlpha<-function(minN=5,p=1/6,minConfirmedX=3,conf.level=0.95){
  maxP = {
    p_value = 1-conf.level
    (qbinom(p_value,size=minN,prob = p,lower.tail = F)+1)/minN
  }
  
  # sum(qi,i->0:2)/sum(qi,i->0:N-1)>=maxP
  # etc. (1-q^minConfirmedX)/(1-q^N)>=maxP
  # etc. q<=(1-maxP)^(1/x)
  maxQ = (1-maxP)^(1/minConfirmedX)
  return(maxQ)
}