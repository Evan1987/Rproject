#获取总数据，以用于聚类
library(data.table)
library(xlsx)
library(varhandle)

path<-"D:/Project/20161027用户活动标签/csv/02参与向量"
setwd(path)
files<-dir()
files<-as.data.frame(files,stringAsFactors=F)
sheet_data<-read.xlsx(paste0(path,"/",files[22,1]),1,encoding = "UTF-8",header = T)
sheet_data<-unfactor(sheet_data)
#选取利用 F_value MF_value RF_value 评价的活动数据
sample01<-sheet_data[which(sheet_data$dim1=="F_value"&sheet_data$dim2=="MF_value"),]

##不同维度的权重标度
###创建权重矩阵    F_value & MF_value
weight_matrix2<-matrix(rep(1),nrow = 2,ncol = 2)

row.names(weight_matrix2)<-c("F_value","MF_value")
colnames(weight_matrix2)<-c("F_value","MF_value")
###设置不同维度间的标度值
weight_matrix2[2,1]<-3
weight_matrix2[1,2]<-1/3
feature_vector2<-eigen(weight_matrix2)$vectors
F2_value_weight<-feature_vector2[1,1]/sum(feature_vector2[,1])
MF2_Value_weight<-feature_vector2[2,1]/sum(feature_vector2[,1])

###创建权重矩阵   F_value & MF_value & RF_value
weight_matrix3<-matrix(rep(1),nrow = 3,ncol = 3)

row.names(weight_matrix3)<-c("F_value","MF_value","RF_value")
colnames(weight_matrix3)<-c("F_value","MF_value","RF_value")
###设置不同维度间的标度值
weight_matrix3[2,1]<-4
weight_matrix3[1,2]<-1/4
weight_matrix3[3,1]<-7
weight_matrix3[1,3]<-1/7
weight_matrix3[3,2]<-2
weight_matrix3[2,3]<-1/2
feature_vector3<-eigen(weight_matrix3)$vectors
F3_value_weight<-as.numeric(feature_vector3[1,1]/sum(feature_vector3[,1]))
MF3_value_weight<-as.numeric(feature_vector3[2,1]/sum(feature_vector3[,1]))
RF3_value_weight<-as.numeric(feature_vector3[3,1]/sum(feature_vector3[,1]))

### 生成结果集
scorelist01<-data.table()
for (i in 1:dim(sample01)[1]) {
  mydata<-fread(paste0("Result",sample01[i,1],".csv"),encoding="UTF-8")
  cols<-dim(mydata)[2]
  #### 只用两个指标的数据集处理方式
  if(cols==4){
    mydata[,":="(score=F_value*F2_value_weight+MF_value*MF2_Value_weight)]
    mydata[,(3:4):=NULL]
  }
  #### 用三个指标的数据集处理方式
  if(cols==5){
    mydata[,":="(score=F_value*F3_value_weight+MF_value*MF3_value_weight+RF_value*RF3_value_weight)]
    mydata[,(3:5):=NULL]
  }
  mydata[,activity:=sample01[i,1]]
  colnames(mydata)<-c("userid","reg_day","score","activityID")
  scorelist01<-rbind(scorelist01,mydata)
  write.csv(mydata,paste0("D:/Project/20161027用户活动标签/csv/03结果归一/",sample01[i,1],".csv"),row.names = F)
  write.csv(scorelist01,"D:/Project/20161027用户活动标签/csv/03结果归一/score_valuelist01.csv",row.names = F)
}

#选取利用 invite_num 评价的活动数据
sample02<-sheet_data[which(sheet_data$dim1=="invite_num"),]

scorelist02<-data.table()
for (i in 1:dim(sample02)[1]) {
  mydata<-fread(paste0("Result",sample02[i,1],".csv"),encoding="UTF-8")
  mydata[,score:=invite_num]
  mydata[,activity:=sample02[i,1]]
  mydata[,2:=NULL]
  colnames(mydata)<-c("userid","reg_day","score","activityID")
  scorelist02<-rbind(scorelist02,mydata)
  write.csv(mydata,paste0("D:/Project/20161027用户活动标签/csv/03结果归一/",sample02[i,1],".csv"),row.names = F)
}

write.csv(scorelist02,"D:/Project/20161027用户活动标签/csv/03结果归一/score_valuelist02.csv",row.names = F)


#选取利用 waitdays 评价的活动数据
sample03<-sheet_data[which(sheet_data$dim1=="waitdays"),]

scorelist03<-data.table()
for (i in 1:dim(sample03)[1]) {
  mydata<-fread(paste0("Result",sample03[i,1],".csv"),encoding="UTF-8")
  mydata[,score:=1/(1/(waitdays+2))]
  mydata[,activity:=sample03[i,1]]
  mydata[,3:=NULL]
  colnames(mydata)<-c("userid","reg_day","score","activityID")
  scorelist03<-rbind(scorelist03,mydata)
  write.csv(mydata,paste0("D:/Project/20161027用户活动标签/csv/03结果归一/",sample03[i,1],".csv"),row.names = F)
}

write.csv(scorelist03,"D:/Project/20161027用户活动标签/csv/03结果归一/score_valuelist03.csv",row.names = F)

#汇总结果
score_final<-rbind(scorelist01,scorelist02,scorelist03)

#★将结果与活动的类型数据相拼接，获得总数据集，主键为“用户+活动ID”
setkey(score_final,activityID)
activityindex<-as.data.table(copy(sheet_data))
activityindex[,(5:dim(sheet_data)[2]):=NULL]
setkey(activityindex,Activity)
score_final<-score_final[activityindex,nomatch=NA,mult="all"]
score_final[,reg_day:=as.Date(reg_day)]

library(sqldf)
z1<-"select m.*,
		        n.*
from
(select	a.*,
        sum((b.Void_variable==1)) as Type1_should_in,
        sum((b.Void_variable==2)) as Type2_should_in,
        sum((b.Void_variable==3)) as Type3_should_in
from
(select userid,
        reg_day
from score_final
group by userid
)a		
left join activityindex b on b.Begin_Time>=a.reg_day
group by a.userid,a.reg_day
)m
left join 
(select userid,
        sum((Void_variable==1)) as Type1_actual_in,
        sum((Void_variable==2)) as Type2_actual_in,
        sum((Void_variable==3)) as Type3_actual_in,
        sum((Void_variable==1)*score) as score_1,
        sum((Void_variable==2)*score) as score_2,
        sum((Void_variable==3)*score) as score_3
from score_final
group by userid
)n on m.userid=n.userid"
score_final_type<-sqldf(z1)


##
attach(score_final_type)
score_final_type<-data.frame(userid,reg_day,Type1_should_in,Type1_actual_in,score_1,Type2_should_in,
                             Type2_actual_in,score_2,Type3_should_in,Type3_actual_in,score_3)
detach(score_final_type)
#按照活动编号对不同用户的 score 进行分类汇总，获得活动数据集（列联表），主键为用户
score_final_activity<-xtabs(score~userid+activityID,score_final)
#保存相应结果数据
write.csv(score_final,"D:/Project/20161027用户活动标签/csv/03结果归一/score_final.csv",row.names = F)
write.csv(score_final_type,"D:/Project/20161027用户活动标签/csv/03结果归一/score_final_type.csv",row.names = F)
write.csv(score_final_activity,"D:/Project/20161027用户活动标签/csv/03结果归一/score_final_activity.csv")