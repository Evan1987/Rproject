fun.ftype<-function(x){
  switch(x,
         "赎回"="redeem",
         "申购"="invest",
         "奖励"="reward",
         "新手专享"="new",
         "沃支付申购"="wzf_invest",
         "沃支付赎回"="wzf_redeem",
         "沃支付新手专享"="wzf_new",
         "NULL")
}

fun.ftype2<-function(x){
  switch(x,
         "调整"=1,
         "不调整"=0)
}

fun.premium<-function(x){
  switch(x,
         "invest"=1,
         "redeem"=-1)
}