library(data.table)
library(magrittr)

path = "F:/meituan_Project/20170928金刚位复购优惠券推荐/"


# 用户对门店喜爱度的评价
preferEval<-function(m_0_1_cnt,
                     m_0_1_amt,
                     m_1_3_cnt,
                     m_1_3_amt,
                     m_3_6_cnt,
                     m_3_6_amt,
                     m_6_12_cnt,
                     m_6_12_amt,
                     total_cnt,
                     total_amt,
                     weight=c(1,0.8,0.5,0.3),class_weight=c(0.3,0.7)){
  cnt_score = (m_0_1_cnt*weight[1]+m_1_3_cnt*weight[2]+m_3_6_cnt*weight[3]+m_6_12_cnt*weight[4])/total_cnt
  amt_score = (m_0_1_amt*weight[1]+m_1_3_amt*weight[2]+m_3_6_amt*weight[3]+m_6_12_amt*weight[4])/total_amt
  score = cnt_score*class_weight[1]+amt_score*class_weight[2]
  return(score)
}
sample_data = fread(paste0(path,"sample_data.csv"))
sample_prefer = copy(sample_data)%>%
  .[,score:=preferEval(m_0_1_order_cnt,
                      m_0_1_order_amt,
                      m_1_3_order_cnt,
                      m_1_3_order_amt,
                      m_3_6_order_cnt,
                      m_3_6_order_amt,
                      m_6_12_order_cnt,
                      m_6_12_order_amt,
                      total_order_cnt,
                      total_order_amt)]%>%
  .[,c("mobile","tenant_id","poi_id","score")]%>%
  setorder(.,mobile,-score)

write.csv(sample_prefer,paste0(path,"sample_prefer.csv"),row.names = F)







