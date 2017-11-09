
#对某资产的匹配过程进行动态展示
#ID：资产编号；match_record：匹配过程的记录日志；asset_data：记录资产信息的原始表

match_result_animation<-function(assetid,match_record,asset_data){
  library(data.table)
  library(magrittr)
  library(RColorBrewer)
  library(animation)
  library(ggplot2)
  newpalette<-colorRampPalette(brewer.pal(12,"Set3"))(5)
  
  xx<-match_record[asset_id==assetid,]%T>%
    setorder(.,asset_id)

  total_amount<-asset_data[id==assetid,]$amount
  
  saveHTML(
    {
      #Set delay between frames when replaying
      ani.options(interval=.5)
      # Set up a vector of colors for use below 
      col.range <- heat.colors(15)
      for (i in 1:nrow(xx)) {
        temp<-head(xx,i)%>%
          .[,matched_ratio:=amount/total_amount]%>%
          .[,type:=factor(type,level=rev(c("regular","tPlus","current")))]
        match_num<-uniqueN(temp$userid)
        tt<-temp[,.(matched_ratio=sum(matched_ratio)),by=.(type,asset_id)]%>%
          .[,cum_ratio:=cumsum(matched_ratio)]%T>%
          setorder(.,type)
        p<-ggplot(data = temp,aes(x=asset_id,weight=matched_ratio,fill=type))+
          geom_bar(position = "stack")+ylim(0,1)+
          geom_text(data=tt,aes(x=asset_id,y=cum_ratio,label=round(matched_ratio,2),vjust=2))+
          geom_text(aes(x=asset_id,y=1,label=paste("match_index:",i,"\n","match_users:",match_num),
                        hjust=-2))+
          scale_fill_manual(values=newpalette)
        print(p)
        ani.pause()
      }
    },ani.height = 600, ani.width = 800,htmlfile = paste0(assetid,".html")
  )
}