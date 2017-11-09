library(data.table)
library(RColorBrewer)
library(ggplot2)
function(input,output){
  
  output$userplot<-renderPlot({
    if(input$Usertype!="all"){
      qplot(data=maindata[type==input$Usertype,],x=check_date,y=redeem_amount_total,
            xlab="date",ylab="redeem_amount",main=paste0("user type: ",input$Usertype),geom = "line")
    }
    else{
      ggplot(data=maindata,aes(x=check_date,y=redeem_amount_total,colour=maindata$type))+geom_line()+scale_colour_brewer(palette = "Spectral")+labs(list(x="date",y="redeem_amount",title=paste0("user type: ",input$Usertype)))
    }
  })
}