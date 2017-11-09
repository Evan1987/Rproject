fun.BinDist<-function(x,weights,lo,hi,n){
  ixmin = 1
  ixmax = n-1
  delta = (hi-lo)/(n-1)
  
  nx=length(x)
  y<-rep.int(0,2*n)
  for(i in 1:nx){
    xpos = (x[i]-lo)/delta
    ix = ceiling(xpos)
    fx = ix-xpos
    if(ix>=ixmin & ix<=ixmax){
      y[ix]<-y[ix]+fx*weights[i]
      y[ix+1]<-y[ix+1]+(1-fx)*weights[i]
    }
    else if(ix == 0){y[1]<-y[1]+(1-fx)*weights[i]}
    else if(ix == n){y[ix]<-y[ix]+fx*weights[i]}
  }
  return(y)
}

bw.nrd0<-function (x) 
{
  if (length(x) < 2L) 
    stop("need at least 2 data points")
  hi <- sd(x)
  if (!(lo <- min(hi, IQR(x)/1.34))) 						#min(hi,IQR(x)/1.34)==0时执行，否则lo赋值min(hi,IQR(x)/1.34)
    (lo <- hi) || (lo <- abs(x[1L])) || (lo <- 1)		#hi(x的标准差)、abs(x[1])、1，按顺序哪个不是0，lo就赋值为谁
  0.9 * lo * length(x)^(-0.2)
}