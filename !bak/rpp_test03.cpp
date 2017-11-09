#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double premiumCal(double premium_change, int days, double rate) {
  double result = premium_change>0?premium_change + pow((1+rate),days):premium_change;
  return result;
}
