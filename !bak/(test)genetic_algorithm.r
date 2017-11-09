library(data.table)
library(magrittr)
library(animation)
library(tcltk)
source('~/rstudio/!custom/(fun)trans_between_gray_decimal.r', echo=TRUE)

set.seed(1234)
first_seq = abs(rnorm(n = 1000,mean=3000,sd = 1000))%>%sort(.,decreasing=T)
second_seq = abs(rnorm(n = 1000,mean=3000,sd = 1000))%>%sort(.,decreasing=T)
third_seq = abs(rnorm(n = 1000,mean=3000,sd = 1000))%>%sort(.,decreasing=T)
maxvalue = c(head(first_seq,200),head(second_seq,200),head(third_seq,200))%>%sort(.,decreasing=T)%>%head(.,200)%>%sum(.)
###################################### 0. Method ###############################################
# 0.1 decode Fun
# decoder<-function(gene){
#   a = ((head(gene,7)%>%gray2decimal(.))/100)%>%pmin(.,1)
#   b = ((tail(gene,7)%>%gray2decimal(.))/100)%>%pmin(.,1)
#   return(list(a=a,b=b))
# }

decoder<-function(gene){
  a = ((head(gene,7)%>%binary2decimal(.))/100)%>%pmin(.,1)
  b = ((tail(gene,7)%>%binary2decimal(.))/100)%>%pmin(.,1)
  return(list(a=a,b=b))
}

# 0.2 genecode evaluation Fun
code_evaluation<-function(gene,first_seq,second_seq,third_seq,n=200){
  # evaluation Fun after decoded
  evaluation<-function(a,b,first_seq,second_seq,third_seq,n=200){
    m_first = min(n*a,length(first_seq))
    m_second = min((n-m_first)*b,length(second_seq))
    m_third = min(n-m_first-m_second,length(third_seq))
    
    result = sum(c(head(first_seq,m_first),head(second_seq,m_second),head(third_seq,m_third)))
    return(result)
  }
  
  decodeGene = decoder(gene)
  a = decodeGene$a
  b = decodeGene$b
  
  result = evaluation(a,b,first_seq,second_seq,third_seq,n=200)
  return(result)
}

# 0.3 gene crossover Fun
crossover<-function(fatherGene,motherGene,crossoverPoint,geneLength){
  father_slice1 = fatherGene[1:crossoverPoint]
  father_slice2 = fatherGene[(crossoverPoint+1):geneLength]
  mother_slice1 = motherGene[1:crossoverPoint]
  mother_slice2 = motherGene[(crossoverPoint+1):geneLength]
  child1 = c(father_slice1,mother_slice2)
  child2 = c(mother_slice1,father_slice2)
  return(list(child1,child2))
}

# 0.4 generate new group Fun
generateGroup<-function(existIndex,parentNum,oldGroup,geneLength,matingProb=0.7){
  id = 1:length(existIndex)
  father_id = sample(id,size=parentNum,replace = F)
  mother_id = sample(id[which(!id%in%father_id)],size=parentNum,replace = F)
  singler_id = id[which(!id%in%c(father_id,mother_id))]
  
  fatherIndex = existIndex[father_id]
  motherIndex = existIndex[mother_id]
  singleIndex = existIndex[singler_id]
  
  father = oldGroup[fatherIndex,]
  mother = oldGroup[motherIndex,]
  singler = oldGroup[singleIndex,]
  
  newGroup = oldGroup[1:(2*parentNum),]
  unmatingProb = runif(parentNum,min=0,max=1)
  crossoverPoint = sample(2:(geneLength-1),size=parentNum,replace = T)
  for(i in 1:parentNum){
    if(unmatingProb[i]>=matingProb){
      newGroup[2*i-1,] = father[i,]
      newGroup[2*i,] = mother[i,]
    }else{
      children = crossover(father[i,],mother[i,],crossoverPoint = crossoverPoint[i],geneLength)
      newGroup[2*i-1,] = children[[1]]
      newGroup[2*i,] = children[[2]]
    }
  }
  newGroup = rbind(newGroup,singler)
  return(newGroup)
}

# 0.5 mutate gene
mutation<-function(group,mutationProb){
  shape =dim(group)
  groupNum = shape[1]
  geneLength = shape[2]
  ismutate<-sample(c(0,1),groupNum,replace = T,prob = c(1-mutationProb,mutationProb))
  
  mutateRowIndex = which(ismutate==1)
  mutateColIndex<-sample(1:geneLength,groupNum,replace = T)[mutateRowIndex]
  if(length(mutateRowIndex)>0){
    mutateIndex = sapply(1:length(mutateRowIndex),function(i) (mutateColIndex[i]-1)*groupNum+mutateRowIndex[i])
    group[mutateIndex] = (group[mutateIndex]+1)%%2
  }
  return(group)
}

############################################### 1. Main ##########################################
# parameters
epochNum = 500
mutationProb = 0.001
matingProb = 0.7
e=100
geneLength =14
groupNum = 80
# initial_generatation
group  = matrix(sample(c(1,0),size=geneLength*groupNum,replace = T),nrow = groupNum,byrow = T)

# evolve
evolveList = list()
mean_adaptive <- max_adaptive <- median_adaptive <-rep(0,epochNum)
pb <- tkProgressBar("进度","已完成 %", 0, 100)
for(i in 1:epochNum){
  adaptive = apply(group,
                   MARGIN = 1,
                   code_evaluation,
                   first_seq=first_seq,
                   second_seq=second_seq,
                   third_seq=third_seq)
  
  max_adaptive[i] = max(adaptive)
  mean_adaptive[i] = mean(adaptive)
  median_adaptive[i] = median(adaptive)
  
  
  # if(maxAdaptive-meanAdaptive<=e){
  #   print(paste("evolve finished!", "epoch: ",i,"the current mean adaptive is",meanAdaptive))
  #   break
  # }
  evolveList[[i]] = adaptive
  
  existProb = adaptive/sum(adaptive)
  existIndex = sample(1:groupNum,size = groupNum,replace = T,prob = existProb)
  parentNum = floor(groupNum/2)
  
  group = generateGroup(existIndex = existIndex,
                        parentNum = parentNum,
                        oldGroup = group,
                        geneLength = geneLength,
                        matingProb = matingProb)
  
  group = mutation(group,mutationProb = mutationProb)
  info<- sprintf("已完成 %d%%", round(i*100/epochNum))  
  setTkProgressBar(pb, i*100/epochNum, sprintf("进度 (%s)", info),info)
}

# summary the evolve shift
evolveShift = data.table(epoch=1:epochNum,mean_adaptive,max_adaptive,median_adaptive)
plot(evolveShift$epoch,evolveShift$median_adaptive,ylim=c(0.9e+6,1e+6))
lines(evolveShift$epoch,y=rep(maxvalue,epochNum),col="red")

# decode the final epoch group
a<-apply(group,MARGIN = 1,FUN = function(x) decoder(x)%>%.$a)
b<-apply(group,MARGIN = 1,FUN = function(x) decoder(x)%>%.$b)
adaptive = evolveList[[epochNum]]

final_result = data.table(a,b,adaptive)

saveHTML({ for(j in 1:length(evolveList)){
  x = evolveList[[j]]
  plot(x,main=paste("epoch:",j,"mean adaptive:",mean(x)),ylim=c(0.9e+6,1e+6))
  ani.pause()
  } })



