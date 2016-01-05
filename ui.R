library(shiny)
shinyUI(fluidPage(
  textInput("text", label = h2("Next Word Predictor"), value = "Hello how are"),
  submitButton(text = "Predict next word..."),
  hr(),
  fluidRow((verbatimTextOutput("value")))
  
))
