# 1. 定义求解函数
sloveFun <- function(x){
  x*sin(10*pi * x) + 2
}

#2. 初始化粒子群
limitX <- c(-1, 2) # x限制范围
vmax <- 0.15 * (limitX[2] - limitX[1])  # 速度变化范围为x定义域的15%
particleNum <- 20 # 粒子个数
pbest <- NULL
gbest <- NULL
gbestAdd <- NULL
w <- 1          # 设置惯性权重
c1 <- c2 <- 2   # 设置加速度常数
iters <- 10000  # 设置最大迭代次数
alpha <- 0.0002 # 设置最佳适应度值的增值阈值

#-- 在给定定义域内，随机生成位置矩阵
xMat <- matrix(c(x = runif(particleNum, limitX[1], limitX[2])), dimnames = list(NULL, c("x")))
#              x
# [1,]  1.0536155
# [2,] -0.9237345
# [3,] -0.1517228
# [4,] -0.6818320
# [5,]  1.7338915

#-- 在给定定义域内，随机生成速度矩阵
vMat <- matrix(c(x = runif(particleNum, -vmax, vmax)),  dimnames = list(NULL, c("x")))
#3. 计算种群中所有粒子适应度
adjusts <- apply(xMat, 1, sloveFun)
#[1] 3.0468264 1.3732987 1.8484994 2.3683774 0.4834344

#4. 更新迭代pbest\gbest,同时更新所有粒子的位置与速度
pbest <- cbind(xMat, adjusts)
#               x   adjusts
# [1,]  1.0536155 3.0468264
# [2,] -0.9237345 1.3732987
# [3,] -0.1517228 1.8484994
# [4,] -0.6818320 2.3683774
# [5,]  1.7338915 0.4834344

idxAdjusts <- ncol(pbest)
gbest <- pbest[which.max(pbest[, idxAdjusts]),]
#       x  adjusts
# 1.439921 3.368339
for (i in 1:iters){
  #4.1 更新pbest
  # ---如果当前位置比之前的位置更适应则替换之前信息
  mapply(function(no, adj){
    if(adj > pbest[no, idxAdjusts]){
      pbest[no, ] <<- c(xMat[no, ], adj)
    }
  }, 1:length(adjusts), adjusts)
  
  #4.2 更新gbest
  if (max(pbest[, idxAdjusts]) > gbest[idxAdjusts]) {
    gbestAdd <- max(pbest[, idxAdjusts]) - gbest[idxAdjusts]
    gbest <- pbest[which.max(pbest[, idxAdjusts]), ]
    print("--更新gbest")
    print(gbestAdd)
  }
  
  #4.3 更新所有粒子的位置与速度
  xMatOld <- xMat
  xMat <- xMat + vMat
  vMat <- w*vMat +  # 惯性
    c1 * runif(1, 0, 1) * (pbest[, 1:(idxAdjusts - 1), drop=F] - xMatOld) +    # 自身经验，向自身最优值靠近
    c2 * runif(1, 0, 1) * (matrix(rep(gbest[1:(idxAdjusts - 1)], particleNum), ncol = idxAdjusts - 1 , byrow = T)-xMatOld) # 最优值信息共享
  
  #4.4 超界修正 
  #---如果vMat有值超过边界值，则设定为边界值
  vMat[which(vMat < (-vmax))] <- (-vmax)
  vMat[which(vMat > (vmax))]  <- (vmax)
  #---如果xMat有值超过边界值，则设为边界值
  xMat[which(xMat < (limitX[1]))] <- (limitX[1])
  xMat[which(xMat > (limitX[2]))] <- (limitX[2])
  
  #4.5 计算更新候选的种群适应度
  adjusts <- apply(xMat, 1, sloveFun)
  
  png(filename = paste0("F:/Project/!other research/20170719PSO/",i,".png"),width = 800,height = 600)
  curve(x*sin(10*pi * x) + 2,from=limitX[1],to=limitX[2])
  points(x=xMat,y=adjusts,col="red",pch=20,cex=3)
  dev.off()
  
  #4.6 检查全局适应度增量，如果小于最佳适应度值的增值阈值，则算法停止
  if (!is.null(gbestAdd) && gbestAdd < alpha) {
    cat("k =", i, "算法结束！")
    cat("\n", "最终结果为：", gbest)
    break()
  }
}



