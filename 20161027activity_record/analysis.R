library(RODBC)
library(sqldf)
library(data.table)
conn<-odbcConnect("MySQL",uid="root",pwd="871226")
z1<-"select	a.userid,
		sum(if(b.type=1,b.weight,0)) as type1_part_ratio,
sum(if(b.type=1,b.weight*a.score,0)) as type1_favor_degree,
sum(if(b.type=2,b.weight,0)) as type2_part_ratio,
sum(if(b.type=2,b.weight*a.score,0)) as type2_favor_degree,
sum(if(b.type=3,b.weight,0)) as type3_part_ratio,
sum(if(b.type=3,b.weight*a.score,0)) as type3_favor_degree
from score_final a
join activity_info b on a.activityid=b.id
group by a.userid"
origindata<-as.data.table(sqlQuery(conn,z1))
z2<-"select * from activity_level"
level_data<-sqlQuery(conn,z2)
z3<-"select * from activity_info"
activity_info<-sqlQuery(conn,z3)


mydata<-copy(origindata)
mydata[,":="(type1_part_level=ifelse(type1_part_ratio==0,"",ifelse(type1_part_ratio<0.33,"3",ifelse(type1_part_ratio<0.67,"2","1"))),
             type1_favor_level=ifelse(type1_favor_degree==0,"",ifelse(type1_favor_degree<74,"3",ifelse(type1_favor_degree<320,"2","1"))),
             type2_part_level=ifelse(type2_part_ratio==0,"",ifelse(type2_part_ratio<0.33,"3",ifelse(type2_part_ratio<0.67,"2","1"))),
             type2_favor_level=ifelse(type2_favor_degree==0,"",ifelse(type2_favor_degree<37,"3",ifelse(type2_favor_degree<192,"2","1"))),
             type3_part_level=ifelse(type3_part_ratio==0,"",ifelse(type3_part_ratio<0.33,"3",ifelse(type3_part_ratio<0.67,"2","1"))),
             type3_favor_level=ifelse(type3_favor_degree==0,"",ifelse(type3_favor_degree<5.4,"3",ifelse(type3_favor_degree<22.5,"2","1")))
)]

type1_data<-mydata[type1_part_ratio>0,]
type2_data<-mydata[type2_part_ratio>0,]
type3_data<-mydata[type3_part_ratio>0,]

sqlSave(conn,origindata,rownames = F)
