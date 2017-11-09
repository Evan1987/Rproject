
library(data.table)

#取得不同用户对活动类型的汇总列联表，并转为数据框，待聚类分析
clusterX<-read.csv("D:/Project/20161027用户活动标签/csv/03结果归一/score_final_type.csv",stringsAsFactors = F)
clusterX[,3:11]<-lapply(clusterX[,3:11], as.numeric)
clusterX<-as.data.table(clusterX)
clusterX[,":="(act_degree1=ifelse(Type1_should_in==0,0,(Type1_actual_in+1)/(Type1_should_in+2)),
               act_degree2=ifelse(Type2_should_in==0,0,(Type2_actual_in+1)/(Type2_should_in+2)),
               act_degree3=ifelse(Type3_should_in==0,0,(Type3_actual_in+1)/(Type3_should_in+2)),
               score_1=ifelse(Type1_actual_in==0,0,score_1/Type1_actual_in),
               score_2=ifelse(Type2_actual_in==0,0,score_2/Type2_actual_in),
               score_3=ifelse(Type3_actual_in==0,0,score_3/Type3_actual_in)
)]
result_final<-copy(clusterX)
result_final[]
##定义维度因子化函数
cut_label<-function(cut_vector,level,kmeans=T,breakinfo=F){
  if(kmeans){
  library(rpart)
  max_value<-max(cut_vector)
  min_value<-min(cut_vector)
  km<-kmeans(cut_vector,level)
  a<-cbind(cut_vector,km$cluster)
  colnames(a)<-c("value","label")
  a<-as.data.frame(a,stringsAsFactors=F)
  a_rp<-rpart(label~.,data=a)
  b<-as.data.frame(a_rp$splits)
  breaks_node<-sort(c(min_value,b$index,max_value))
  }
  else{
    breaks_node<-seq(0,1,1/level)
  }
  if(!breakinfo){
    x<-cut(cut_vector,breaks = breaks_node,labels = c(1:level))
    return(x)
  }
  else{
    return(breaks_node)
  }
}


#活动type1聚类
clusterX_type1<-clusterX[Type1_actual_in!=0,]
clusterX_type1[,c(3:4,6:11,13:14):=NULL]
###给两个分析维度分别加因子标签
####获得因子分割的边界区间
score_1_level_breaks<-cut_label(clusterX_type1$score_1,level=3,breakinfo=T)
act_degree1_level_breaks<-cut_label(clusterX_type1$act_degree1,level=3,kmeans=F,breakinfo = T)
####加标签
clusterX_type1[,":="(score_1_level=cut_label(clusterX_type1$score_1,level=3),
                     act_degree1_level=cut_label(clusterX_type1$act_degree1,level=3,kmeans = F))]
clusterX_type1[,index:=10*as.numeric(score_1_level)+as.numeric(act_degree1_level)]

####维度标准化
clusterX_type1[,":="(score_1=scale(score_1),
                     act_degree1=scale(act_degree1))]
####转化为数据框，准备聚类
clusterX_type1<-as.data.frame(clusterX_type1,stringsAsFactors = F)
km_score1<-kmeans(clusterX_type1[,c(3:4)],3)
clusterX_type1<-cbind(clusterX_type1,cluster_index=km_score1$cluster)



#活动type2聚类
clusterX_type2<-clusterX[Type2_actual_in!=0,]
clusterX_type2[,c(3:7,9:12,14):=NULL]
###给两个分析维度分别加因子标签
####获得因子分割的边界区间
score_2_level_breaks<-cut_label(clusterX_type2$score_2,level=3,breakinfo=T)
act_degree2_level_breaks<-cut_label(clusterX_type2$act_degree2,level=3,kmeans = F,breakinfo = T)
####加标签
clusterX_type2[,":="(score_2_level=cut_label(clusterX_type2$score_2,level=3),
                     act_degree2_level=cut_label(clusterX_type2$act_degree2,level=3,kmeans = F))]
clusterX_type2[,index:=10*as.numeric(score_2_level)+as.numeric(act_degree2_level)]

####维度标准化
clusterX_type2[,":="(score_2=scale(score_2),
                     act_degree2=scale(act_degree2))]
####转化为数据框，准备聚类
clusterX_type2<-as.data.frame(clusterX_type2,stringsAsFactors = F)
km_score2<-kmeans(clusterX_type2[,c(3:4)],3)
clusterX_type2<-cbind(clusterX_type2,cluster_index=km_score2$cluster)



#活动type3聚类
clusterX_type3<-clusterX[Type3_actual_in!=0,]
clusterX_type3[,c(3:10,12:13):=NULL]
###给两个分析维度分别加因子标签
####获得因子分割的边界区间
score_3_level_breaks<-cut_label(clusterX_type3$score_3,level=3,breakinfo=T)
act_degree3_level_breaks<-cut_label(clusterX_type3$act_degree3,level=3,kmeans = F,breakinfo = T)
####加标签
clusterX_type3[,":="(score_3_level=cut_label(clusterX_type3$score_3,level=3),
                     act_degree3_level=cut_label(clusterX_type3$act_degree3,level=3,kmeans = F))]
clusterX_type3[,index:=10*as.numeric(score_3_level)+as.numeric(act_degree3_level)]

####维度标准化
clusterX_type3[,":="(score_3=scale(score_3),
                     act_degree3=scale(act_degree3))]
####转化为数据框，准备聚类
clusterX_type3<-as.data.frame(clusterX_type3,stringsAsFactors = F)
km_score3<-kmeans(clusterX_type3[,c(3:4)],3)
clusterX_type3<-cbind(clusterX_type3,cluster_index=km_score3$cluster)


#汇总聚类数据
result_type1<-as.data.frame(cbind(index=row.names(km_score1$centers),km_score1$centers,size=km_score1$size))
result_type2<-as.data.frame(cbind(index=row.names(km_score2$centers),km_score2$centers,size=km_score2$size))
result_type3<-as.data.frame(cbind(index=row.names(km_score3$centers),km_score3$centers,size=km_score3$size))

grid_type1<-as.data.frame(cbind(act_degree_grid=act_degree1_level_breaks,score_grid=score_1_level_breaks))
grid_type2<-as.data.frame(cbind(act_degree_grid=act_degree2_level_breaks,score_grid=score_2_level_breaks))
grid_type3<-as.data.frame(cbind(act_degree_grid=act_degree3_level_breaks,score_grid=score_3_level_breaks))

write.csv(result_type1,"D:/Project/20161027用户活动标签/csv/03结果归一/result_type1.csv",row.names = F)
write.csv(result_type2,"D:/Project/20161027用户活动标签/csv/03结果归一/result_type2.csv",row.names = F)
write.csv(result_type3,"D:/Project/20161027用户活动标签/csv/03结果归一/result_type3.csv",row.names = F)
write.csv(grid_type1,"D:/Project/20161027用户活动标签/csv/03结果归一/grid_type1.csv",row.names = F)
write.csv(grid_type2,"D:/Project/20161027用户活动标签/csv/03结果归一/grid_type2.csv",row.names = F)
write.csv(grid_type3,"D:/Project/20161027用户活动标签/csv/03结果归一/grid_type3.csv",row.names = F)
