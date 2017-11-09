library(dplyr)
library(tibble)
library(lazyeval)
xx<-iris%>%
  tbl_df(.)%>%{
    colname<-colnames(.)
    vars<-colname[1:length(colname)]
    select(.,one_of(vars))%>%
      select(.,contains("Specie"),everything())
  }%>%
  rownames_to_column(.,"id")%>%
  rename_(.,.dots=setNames(c("Sepal.Length","Sepal.Width"),c("LengthOfSepal","WidthOfSepal")))%>%
  rename_(.,.dots=setNames(c("Petal.Length","Petal.Width"),c("LengthOfPetal","WidthOfPetal")))%>%
  select_(.,.dots=quote(-c(LengthOfPetal,WidthOfPetal)))%>%
  arrange(.,LengthOfSepal,desc(id))%>%
  filter(.,between(LengthOfSepal,4,4.5))%>%
  mutate(.,y=case_when(.$WidthOfSepal>3~"large",.$WidthOfSepal<2.5~"small",T~"medium"))%>%
  mutate_(.,.dots=(cummean=cunmean(WidthOfSepal)))

sql_1<-build_sql("select * from xx")
sql(sql_1)





f<-function(x){if(x==1) stop("Error") else 1}
safef<-failwith("it's error",f)
safef(1)

fs<-c(m1="min",m2="max",m3="mean")
fs_list<-funs_(fs,dots=c(10,20,30))


####### lazy function #####
f<-function(x){
  z<-100
  ~x+z#=>An expression
}
f(10)#not compute
lazy_eval(f(10))#return result

