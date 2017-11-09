cut_label<-function(cut_vector,level,dokmeans=T){
  if(is.character(cut_vector)){
    print("warning! cut_vector is character!")
    cut_vector<-as.numeric(cut_vector)
  }  
    
  if(dokmeans){
    library(rpart)
    max_value<-max(cut_vector)
    min_value<-min(cut_vector)
   
	  km<-kmeans(cut_vector,level)
    a<-cbind(cut_vector,km$cluster)
    colnames(a)<-c("value","label")
    a<-as.data.frame(a,stringsAsFactors=F)
    
	  a_rp<-rpart(label~.,data=a)							#获得kmeans聚类分割节点
    b<-as.data.frame(a_rp$splits)
	
	  km_node<-sort(b$index)
	  max_node<-max_value
	  min_node<-min_value-abs(km_node[1]-min_value)*0.001     #由于cut函数的半开半闭性（默认左开右闭），所以当数据点与最大（小）值重叠时，无法分类，所以对最小值向下做缩小处理。
    
	  breaks_node<-sort(c(min_node,km_node,max_node))	#获得整体完整分割节点，最小值、kmeans分割节点、最大值。
  }
  else{
    breaks_node<-seq(0,1,1/level)
  }
  
  x<-cut(cut_vector,breaks = breaks_node,labels = c(1:level))	#核心函数，根据分割节点进行数据点分类。
  
  return(structure(list(result=x,breaks_node=breaks_node),class="cut_label"))	#返回一个模块，result为分类结果，breaks_node为分割节点信息
}