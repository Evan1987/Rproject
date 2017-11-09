#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calCost(CharacterVector type,
                      CharacterVector deal_type,
                      NumericVector num,
                      NumericVector price,
                      NumericVector poundage,
                      NumericVector total_num,
                      double rs = 0.003,
                      double rb = 0.003) {
  int n = num.size();
  NumericVector cost(n);
  NumericVector total_cost(n);
  NumericVector balance_price(n);
  
  for (int i = 0; i < n; i++)
  {
    if(type[i]=="buy"){
      cost[i] = num[i]*price[i]+poundage[i];
    }
    
    if(type[i]=="sell"){
      if(deal_type[i]!="jiaoge"){
        cost[i] = -1*(num[i]*price[i]-poundage[i]);
      }else{
        cost[i] = -1*(num[i]*balance_price[i-1]*(1-rs));
      }
    }
    
    if(total_num[i]==0){
      total_cost[i]=0;
      balance_price[i]=0;
    }else{
      if(i==0){
        total_cost[i] = cost[i];
      }else{
        total_cost[i] = cost[i]+total_cost[i-1];
      }
      balance_price[i] = total_cost[i]/(total_num[i]*(1-rs));
    }
  }
  
  return total_cost;
}
