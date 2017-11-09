library(data.table)
library(gglot2)
maindata<-as.data.table(redeem_summary)
function(input,output){
  output$userplot<-renderPlot({
    qplot(data=maindata[type==input$Usertype,],x=check_date,y=redeem_amount_total,
          xlab="date",ylab="redeem_amount",main=paste0("user type: ",input$Usertype))
  })
}