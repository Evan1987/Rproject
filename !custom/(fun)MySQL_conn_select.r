
MySQL_conn_select<-function(connect_name,db_name){
  library(RMySQL)
  avail_list = c("dev2","local","asset_product","cash_product","test")
  if(!connect_name%in%avail_list){
    stop("available connect options are 'dev2','local','asset_product','cash_product','test'!")
  }
  switch(tolower(connect_name),
         "dev2"=dbConnect(MySQL(),
                          host = "192.168.199.45",
                          port = 3306,
                          user="xman_dev",
                          password='f234567890',
                          dbname=ifelse(missing(db_name),"jianlc_asset",db_name)),
         "local"=dbConnect(MySQL(),
                           host = "127.0.0.1",
                           port = 3306,
                           user="root",
                           password='871226',
                           dbname=ifelse(missing(db_name),"yinker",db_name)),
         "asset_product"=dbConnect(MySQL(),
                                   host = "120.27.167.74",
                                   port = 80,
                                   user="datacenter_read",
                                   password='Zjy-yinker20150309',
                                   dbname=ifelse(missing(db_name),"jianlc_asset",db_name)),
         "cash_product"=dbConnect(MySQL(),
                                  host = "120.55.176.18",
                                  port = 5306,
                                  user="xmanread",
                                  password='LtLUGkNbr84UWXglBFYe4GuMX8EJXeIG',
                                  dbname=ifelse(missing(db_name),"product",db_name)),
         "test"=dbConnect(MySQL(),
                          host = "10.1.12.157",
                          port = 3306,
                          user="xman_dev",
                          password='f234567890',
                          dbname=ifelse(missing(db_name),"xman_test",db_name)),
         "DataCenter_test"=dbConnect(MySQL(),
                                     host = "10.1.5.220",
                                     port = 3306,
                                     user = "zhangjiayi",
                                     password = "Zjy@yinker20150309",
                                     dbname=ifelse(missing(db_name),"test",db_name))
         )
}