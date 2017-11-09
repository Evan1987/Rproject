library(magrittr)

solveFun<-function(x){x*sin(10*pi*x)+2}

limitX<-c(-1,2)
vmax = 0.15*(limitX[2]-limitX[1])
particleNum = 20
pbest <- gbest <- gbestAdd <- NULL
w = 0.5;c1<-c2<-2
iters = 10000
alpha = 0.0002


xMat = runif(n = particleNum,min = limitX[1],max = limitX[2])%>%matrix(.,ncol=1,dimnames = list(NULL,"x"))
vMat = runif(n = particleNum,min = -vmax,max=vmax)%>%matrix(.,ncol=1)%>%matrix(.,ncol=1,dimnames = list(NULL,"x"))

adjusts = sapply(xMat[,1],solveFun)
pbest = cbind(xMat,adjusts)

idxAdjusts = ncol(pbest)
gbest <- pbest[which.max(pbest[,idxAdjusts]),]

adjustMat<-matrix(rep(0,particleNum*iters),nrow = particleNum)
for(i in 1:50){
  xMatOld<-xMat
  xMat<-xMat+vMat
  vMat = w*vMat + 
    c1*runif(1,0,1)*(pbest[,1:(idxAdjusts-1),drop=F]-xMatOld) +
    c2*runif(1,0,1)*(matrix(rep(gbest[1:(idxAdjusts-1)],particleNum),ncol = idxAdjusts-1,byrow = T)-xMatOld)
  
  vMat[vMat< -vmax]<- -vmax
  vMat[vMat> vmax]<-vmax
  
  xMat[xMat<limitX[1]] <- limitX[1]
  xMat[xMat>limitX[2]] <- limitX[2]
  
  adjusts = apply(xMat,1,solveFun)
  
  png(filename = paste0("F:/Project/!other research/20170719PSO/",i,".png"),width = 800,height = 600)
  curve(solveFun,from=limitX[1],to=limitX[2])
  points(x=xMat,y=adjusts,col="red",pch=20,cex=3)
  dev.off()
  
  adjustMat[,i]<-adjusts
  mapply(function(no,adj){
    if(adj>pbest[no,idxAdjusts]){
      pbest[no,]<<-c(xMat[no,],adj)
    }
  },1:particleNum,adjusts)
  
  if(max(pbest[,idxAdjusts])>gbest[idxAdjusts]){
    gbestAdd = max(pbest[,idxAdjusts])-gbest[idxAdjusts]
    gbest <- pbest[which.max(pbest[,idxAdjusts]),]
  }
  
  # if(!is.null(gbestAdd) && gbestAdd<alpha){break}
}



