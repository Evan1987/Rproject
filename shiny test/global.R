library(data.table)
maindata<-fread("D:\\Project\\（进行中）20161220运营新分析模式\\redeem_summary.csv")
maindata$check_date<-as.Date(maindata$check_date)
