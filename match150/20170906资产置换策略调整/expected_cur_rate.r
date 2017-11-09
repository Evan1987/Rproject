library(data.table)
library(magrittr)
library(stringr)
library(RMySQL)
source('~/rstudio/!custom/(fun)MySQL_conn_select.r', echo=TRUE)

# data select date: 2017-09-05
premium_regular = 21.4728e+8
premium_cur = 57e+8
cur_rate = 3.5
asset_conn<-MySQL_conn_select("asset_product")

z1<-"select	id,
corpusAmount,
aunualInterestRate as rate
from ast_loan_asset
WHERE yn = 0 AND STATUS IN(400,600) and corpusAmount>0"
res<-dbSendQuery(asset_conn,z1)
asset_data<-dbFetch(res,n=-1)%>%as.data.table(.)
dbClearResult(res)

dbDisconnect(asset_conn)

asset_data<-setorder(asset_data,-rate)%>%
  .[,cumAmount:=cumsum(corpusAmount)]%>%
  .[,judge:=cumAmount-premium_regular]%>%
  .[,usedAmount:=corpusAmount-pmax(judge,0)]%>%
  .[,restAmount:=corpusAmount-pmax(usedAmount,0)]


cur_asset = asset_data[restAmount>0,]

# ans: 5.64
cur_target_rate = (sum(cur_asset$rate*cur_asset$restAmount)+
                     (premium_cur-sum(cur_asset$restAmount))*cur_rate)/premium_cur




