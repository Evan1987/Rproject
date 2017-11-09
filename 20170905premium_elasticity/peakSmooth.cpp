// 峰点平滑去毛刺
#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]
IntegerVector peakSmooth(IntegerVector x,int lenLimit) {
  int n = x.size();
  int len = 0;
  // 极大值初始化
  int peakSus = -1;
  // sumNearby为随时记录该数据点前lenLimit天内的数据和
  int sumNearby = x[0];
  // 标记从何处开始进行均值化处理。
  int changeStart;
  for(int i=1;i!=n;i++){
    // 寻找极大值
    if(x[i]>x[i-1])
    {
      // 极大值更新，稳定长度初始化为1
      peakSus = x[i];
      len = 1;
    }
    // 稳定长度增加
    else if(x[i]==x[i-1])
    {
      len += 1;
    }
    // 如果稳定被打破，则检查稳定时长
    else
    {
      // 如果稳定时长低于lenLimit并且当时是有极大值的
      if(len<lenLimit && peakSus>0)
      {
        changeStart = i-lenLimit;
        // 对于当前数据点还不满lenLimit的情况
        if(changeStart<0)
        {
          changeStart = 0;
        }
        // 均值化处理
        for(int j=changeStart;j<=i;j++)
        {
          x[j] = sumNearby/(i-changeStart+1);
        }
        // 由于极大值点及稳定区域都被均化了，则极大值初始化
        peakSus = -1;
      }
    }
    // sumNearby的更新规则，在不满lenLimit个数据之前，逐个累加；如果满了，则去掉最后的，加上当前的。
    if(i>=lenLimit)
    {
      sumNearby += x[i]-x[i-lenLimit]; 
    }
    else
    {
      sumNearby += x[i];
    }
  }
  return x;
}

