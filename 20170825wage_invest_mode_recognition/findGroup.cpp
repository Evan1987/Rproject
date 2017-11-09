#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector findGroup(IntegerVector x)
{
  int n = x.size();
  IntegerVector index(n);
  int r = index[0] = 1;
  int before = x[0];
  int now;
  for (int i = 1; i < n; i++)
  {
    now = x[i];
    if(now-before==1)
    {
      index[i]=r;
    }else{
      r++;
      index[i] = r;
    }
    before = now;
  }
  return index;
}