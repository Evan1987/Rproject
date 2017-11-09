library(shiny)
shinyUI(pageWithSidebar(
  headerPanel("Minimal example"),
  sidebarPanel(textInput(inputId = "comment",
                         label = "Say something?",
                         value = "")),
  mainPanel(
    h3("This is you saying it"),textOutput("textDisplay")
  )
))