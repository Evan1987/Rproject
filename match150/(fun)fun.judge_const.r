fun.judge_const<-function(a,b,wait_asset,first_seq,second_seq,third_seq){
  
  m_a<-min(floor(a*wait_asset$avail_num),nrow(first_seq))
  m_b<-min(floor((wait_asset$avail_num-m_a)*b),nrow(second_seq))
  m_c<-min(wait_asset$avail_num-m_a-m_b,nrow(third_seq))
  
  judge_const<-
    (
      first_seq$unmatched_premium%>%
        head(.,m_a)%>%
        sum(.)
    )+
    (
      second_seq$unmatched_premium%>%
        head(.,m_b)%>%
        sum(.)
    )+
    (
      third_seq$unmatched_premium%>%
        head(.,m_c)%>%
        sum(.)
    )
  return(judge_const)
}