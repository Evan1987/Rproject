// 寻找数组的峰点（极大值点）
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector findPeak(NumericVector x)
{
  int n = x.size();
  // 关键量：数组变化的趋势
  IntegerVector trend(n-1,0);
  IntegerVector peakPositions;
  // diff操作，获得变化趋势描述，上升为1，下降为-1，不变为0
  for (int i = 0; i < n-1; i++)
  {
   if(x[i+1]-x[i]>0)
     trend[i] = 1;
   else if(x[i+1]-x[i]<0)
     trend[i] = -1;
   else
     trend[i] = 0;
  }
  
  // 为了防止数据不变区域的平台影响极值的寻找，故从后向前对平台打上标签。
  // 这些标签与其之后的山坡趋势一致。
  for (int i = n-2;i>=0;i--)
  {
    if(trend[i]==0 && i==n-2)
      // 最后的点默认为上升
      trend[i] = 1;
    else if(trend[i]==0)
      {
        if(trend[i+1]>=0)
          trend[i] = 1;
        else
          trend[i] = -1;
      }
  }
  
  // 寻找极大值点（改点趋势为上升，之后为下降）
  for (int i = 0;i<n-1;i++)
  {
    if(trend[i+1]-trend[i] == -2)
      peakPositions.push_back(i+2);//for the convinience of use in R, transform the index;
  }
  return peakPositions;
}