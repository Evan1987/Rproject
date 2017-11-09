#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector premiumCal(NumericVector premium_change, NumericVector days, double rate) {
  double s1 = 0;
  double s0;
  int n = premium_change.size();
  NumericVector result(n);
  for(int i=0;i<n;++i){
    s0 = s1 + premium_change[i];
    s1 = s0>0?s0*pow((1+rate),days[i]):s0;
    result[i] = s1;
  }
  return result;
}
