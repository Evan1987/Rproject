stringbind<-function(vector){
  library(stringr)
  combind<-str_c(vector,collapse ="','")
  combind<-paste0("'",combind,"'")
  return(combind)
}
