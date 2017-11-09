library(arules)
library(arulesViz)
library(RColorBrewer)
groceries<-read.transactions("apriori.csv",format = "basket",sep=",")
summary(groceries)
basketSize<-size(groceries)
itemFreq<-itemFrequency(groceries)  ##item的支持度
itemCount<-itemFreq/sum(itemFreq)*sum(basketSize)  ##每个item出现的次数

itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=10,horiz=T)
#选取购物篮大于1个物品的样例，这样才能进行关联分析
groceries_use<-groceries[basketSize>1]
dim(groceries_use) #查看总样例大小
inspect(groceries_use[1:5]) #查看具体样例内容

image(sample(groceries_use,100)) #查看商品的稀疏矩阵情况，纵坐标为样例，横坐标为商品
groceryrules<-apriori(groceries,parameter = list(support=0.006,confidence=0.25,minlen=2))

summary(groceryrules)

inspect(groceryrules[1:5])

ordered_groceryrules<-sort(groceryrules,by="lift")
inspect(ordered_groceryrules[1:5])

yogurtrules<-subset(groceryrules,items%in%c("yogurt"))  # lhs or rhs have "yogurt"
inspect(yogurtrules)

fruitrules<-subset(groceryrules,lhs%pin%c("fruit"))
inspect(fruitrules)

byrules <- subset(groceryrules, items %ain% c("berries", "yogurt")) 
inspect(byrules)


fruitrules<-subset(groceryrules,items%pin%c("fruit")&lift>2)
inspect(fruitrules)


qualityMeasures<-interestMeasure(groceryrules,
                                 measure = c("coverage","fishersExactTest","conviction", "chiSquared"),
                                 transactions=groceries) 
summary(qualityMeasures)

quality(groceryrules)<-cbind(quality(groceryrules),qualityMeasures)
inspect(head(sort(groceryrules,by="conviction",decreasing = F)))


berriesInLHS<-apriori(groceries,
                      parameter = list(support=0.001,confidence=0.1),
                      appearance = list(lhs=c("berries"),default="rhs"))


inspect(berriesInLHS)

inspect(head(rhs(berriesInLHS),5))

groceryrules_df<-as(groceryrules,"data.frame")
###########################################################################################################
data("Groceries")
itemInfo(Groceries) #data.frame
levels(itemInfo(Groceries)[["level2"]])
levels(itemInfo(Groceries)[["level1"]])


itemFrequencyPlot(Groceries,
                  support=0.025,
                  cex.names=0.8,
                  xlim=c(0,0.3),
                  type="relative",
                  horiz=T,
                  col="dark red",
                  las=1,
                  xlab=paste("Proportion of Market Baskets Containing Item",
                             "\n(Item Relative Frequency or Support)"))



second.rules<-apriori(Groceries,
                      parameter = list(support=0.025,confidence=0.05))

plot(second.rules,shading = "lift",control = list(jitter=2,col=rev(brewer.pal(9,"Greens")[4:9])))

plot(second.rules,method = "grouped",control=list(col=rev(brewer.pal(9,"Greens")[4:9])))

plot(second.rules,measure = "confidence",method = "graph",control=list(type="items",shading="lift"))
