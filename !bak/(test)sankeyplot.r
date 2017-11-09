library(d3Network)
library(riverplot)
library(data.table)
library(magrittr)
library(stringr)
library(RColorBrewer)
edges = data.table(N1 = paste0(rep(LETTERS[1:4], each = 4), rep(1:5, each = 16)),  
                   N2 = paste0(rep(LETTERS[1:4], 4), rep(2:6, each = 16)),  
                   Value = runif(80, min = 2, max = 5) * rep(c(1, 0.8, 0.6, 0.4, 0.3), each = 16),  
                   stringsAsFactors = F) 
select_list<-sample(c(T,F),size = nrow(edges),replace = T,prob = c(0.8,0.2))

sample_edges<-edges[select_list,]
####################### riverplot ###########################
nodes = data.table(ID = unique(c(sample_edges$N1, sample_edges$N2)), stringsAsFactors = FALSE)  
nodes[,":="(x=as.integer(substr(ID,2,2)),y=sapply(substr(ID,1,1),charToRaw)%>%as.integer(.)-65)]

rownames(nodes)<-nodes$ID
palette = paste0(brewer.pal(uniqueN(nodes$y),"Set1"),60)

styles = lapply(nodes$y, function(n) {  
  list(col = palette[n+1], lty = 0, textcol = "black")  
})%T>%{names(.)<-nodes$ID} 

rp<-list(nodes = nodes,edges=sample_edges,styles=styles)

class(rp)<-c(class(rp),"riverplot")

plot(rp,plot_area=0.95,yscale=0.06)


######################## d3NetWork ########################
d3links<-copy(sample_edges)
d3nodes<-data.table(name = unique(c(sample_edges$N1, sample_edges$N2)), stringsAsFactors = FALSE)  
d3nodes[,seq:=0:(nrow(d3nodes)-1)]

d3links<-d3links[d3nodes,on=c("N1"="name"),source:=i.seq]%>%
  .[d3nodes,on=c("N2"="name"),target:=i.seq]%T>%
  {
    setnames(.,names(.),tolower(names(.)))
  }%>%
  .[,.SD,.SDcols=c("source","target","value")]

class(d3links)<-c("data.frame",class(d3links))
class(d3nodes)<-c("data.frame",class(d3nodes))

d3Sankey(Links = d3links,
         Nodes = d3nodes,
         Source = "source",
         Target = "target",
         Value = "value",
         NodeID = "name",
         fontsize = 12,
         nodeWidth = 30,
         file = "TestSankey.html")










