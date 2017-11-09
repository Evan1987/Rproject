

lotterySummary = rawdata[deal_type%in%c("yonghuchoujiang","yonghuzhongjiang"),
                         c("userid","issuer","create_time","price","deal_type")]


choujiangList = lotterySummary[deal_type=="yonghuchoujiang"]
choujiangSummary = choujiangList[,.(num=uniqueN(userid)),by=.(create_time,issuer)]
