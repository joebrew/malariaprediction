library(shiny)
library(ggplot2)
library(malariaprediction)

shinyUI(fluidPage(
  titlePanel("Malaria Eradication Prediction Market"),
  
  sidebarLayout(position = "left",
                sidebarPanel(uiOutput("in.user"),
                             uiOutput("in.pss"),
                             uiOutput("in.clr"),
                             uiOutput("in.titl"),
                             uiOutput("in.cnt"),
                             uiOutput("in.seed")
                             
                ),
                mainPanel(h3("main panel"),
                          textOutput('echo'),
                          plotOutput('stdplot')
                )
  )
))
