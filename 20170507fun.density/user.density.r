density.user<-function(x,
                       bw="nrd0",
                       adjust=1,
                       kernel=c("gaussian",
                                "epanechnikov",
                                "rectangular",
                                "triangular",
                                "biweight",
                                "cosine",
                                "optcosine"),
                       weights = NULL,
                       width,
                       n =512,
                       from,to,cut=3,na.rm=F
){
  source('~/rstudio/fun.density/(source)user.fun.r', encoding = 'UTF-8')
  x.na<-is.na(x)
  if(any(x.na)){
    if(na.rm)
      x<-x[!x.na]
    else
      stop("'x' contains missing values")
  }
  
  N<-nx<-as.integer(length(x))
  
  x.infinite<-is.infinite(x)
  if(any(x.infinite)){
    x<-x[!x.infinite]
    nx<-length(x)
  }
  
  if(is.null(weights)){
    weights<-rep.int(1/nx,nx)
    totMass<-nx/N
  }else{
    if(length(weights)!=N)
      stop("'x' and 'weights' have unequal length")
    
    if(any(is.infinite(weigths)))
      stop("'weights' must all be finite")
    
    if(any(weights<0))
      stop("'weights' must be positive or zero")
    
    wsum <- sum(weights)
    
    if(any(x.infinite)){
      weights<-weights[!x.infinite]
      totMass<-sum(weights)/wsum
    }else{
      totMass<-1
    }
    if(wsum!=1) 
      warning("sum(weights) != 1  -- will not get true density")
  }
  
  n.user<-n
  n<-ifelse(n>512,2^ceiling(log2(n)),512)
    
  if(missing(bw) & !missing(width)){
    if(is.numeric(width)){
      fac<-switch(kernel,
                  "gaussian"=4,
                  "rectangular"=2*sqrt(3),
                  "triangular"=2*sqrt(6),
                  "epanechnikov"=2*sqrt(5),
                  "biweight"=2*sqrt(7),
                  "cosine"=2/sqrt(1/3-2/pi^2),
                  "optcosine"=2/sqrt(1-8/pi^2))
      bw<-width/fac
    }
    if(is.character(width))
      bw<-width
  }
  if(is.character(bw)){
    if(nx<2)
      stop("need at least 2 points to select a bandwidth automatically")
    bw<-switch(tolower(bw),
               "nrd0" = bw.nrd0(x),
               "nrd" = bw.nrd(x),
               "ucv" = bw.ucv(x),
               "bcv" = bw.bcv(x),
               "sj-ste" = bw.SJ(x,method = "ste"),
               "sj-dpi" = bw.SJ(x,method = "dpi"),
               stop("unknown bandwidth rule"))
  }
  if (!is.finite(bw)) 
    stop("non-finite 'bw'")
  bw <- adjust * bw
  if (bw <= 0) 
    stop("'bw' is not positive.")
  
  if(missing(from))
    from<-min(x)-cut*bw
  if(missing(to))
    to<-max(x)+cut*bw
  if (!is.finite(from)) 
    stop("non-finite 'from'")
  if (!is.finite(to)) 
    stop("non-finite 'to'")
  
  lo <- from - 4 * bw
  up <- to + 4 * bw
  y<-fun.BinDist(x,weights = weights,lo = lo,hi=up,n=n)*totMass

  
  kords<-seq.int(0,2*(up-lo),length.out = 2*n)
  kords[(n+2):(2*n)]<- -kords[n:2]
  
  kords<-switch(kernel,
                "gaussian" = dnorm(kords,sd=bw),
                "rectangular" = {
                  a<-bw*sqrt(3)
                  ifelse(abs(kords)<a,0.5/a,0)
                },
                "triangular" = {
                  a<-bw*sqrt(6)
                  ax<-abs(kords)
                  ifelse(ax<a,(1-ax/a)/a,0)
                },
                "epanechnikov" = {
                  a<-bw*sqrt(5)
                  ax<-abs(kords)
                  ifelse(ax<a,3/4*(1-(ax/a)^2)/a,0)
                },
                "biweight" = {
                  a<-bw*sqrt(7)
                  ax<-abs(kords)
                  ifelse(ax<a,15/16*(1-(ax/a)^2)^2/a,0)
                },
                "cosine" = {
                  a<-bw/sqrt(1/3-2/pi^2)
                  ifelse(abs(kords)<a,(1+cos(pi*kords/a))/(2*a),0)
                },
                "optcosine" = {
                  a<-bw/sqrt(1-8/pi^2)
                  ifelse(abs(kords)<a,pi/4*cos(pi*kords/(2*a))/a,0)
                }
                )
  kords<-fft(fft(y)*Conj(fft(kords)),inverse = T)
  kords<-pmax.int(0,Re(kords)[1:n]/length(y))
  xords<-seq.int(lo,up,length.out = n)
  x1<-seq.int(from,to,length.out = n.user)
  y1<-approx(xords,kords,x1)$y
  structure(list(x = x1, y = y1, bw = bw, 
                 n = N, call = match.call()), 
            class = "density.user")
  
}