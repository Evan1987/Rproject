fun.spare_matrix_for_user<-function(target_data){
  x0<-target_data$user_id
  x<-factor(x0)%>%as.numeric(.)
  mat<-sapply(1:max(x),function(y) as.numeric(x==y))%>%t(.)
  return(mat)
}