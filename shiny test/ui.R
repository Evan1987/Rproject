fluidPage(
  titlePanel("The Redeem Action of different type of users"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Usertype","Usertype:",choices = c(levels(factor(maindata$type)),"all")),
      hr(),
      helpText("Data from Redeem_summary")
    ),
    mainPanel(
      plotOutput("userplot")
    )
  )
)