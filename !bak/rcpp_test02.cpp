#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
double f1(NumericVector x){
  int n = x.size();
  double y =0;
  for(int i = 0; i<n; ++i){
    y+=x[i]/n;
  }
  return y;
}

#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
NumericVector f2(NumericVector x){
  int n = x.size();
  NumericVector out(n);
  
  out[0]=x[0];
  
  for(int i = 1; i<n;++i){
    out[i] = out[i-1]+x[i];
  }
  return out;
}


/***R
x=c(1,2,3,4)
f1(x)
f2(x)
*/