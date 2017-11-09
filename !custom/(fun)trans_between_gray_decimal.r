gray2decimal<-function(x){
  library(GA)
  return(binary2decimal(gray2binary(x)))
}

decimal2gray<-function(x){
  library(GA)
  return(binary2gray(decimal2binary(x)))
}