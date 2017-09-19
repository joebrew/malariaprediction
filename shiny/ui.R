library(shiny)
library(ggplot2)
library(malariaprediction)

shinyUI(fluidPage(
  titlePanel("Malaria Eradication Prediction Market"),
  
  sidebarLayout(position = "left",
                sidebarPanel(uiOutput("in.user"),
                             uiOutput("in.pss"),
                             uiOutput("in.event"),
                             h4(textOutput('event_full_header')),
                             helpText(textOutput('event_full_statement')),
                             uiOutput("in.yesno"),
                             helpText(textOutput('yesno_explanation')),
                             uiOutput("in.prob"),
                             uiOutput("in.shares"),
                             uiOutput('in.action'),
                             uiOutput('in.offer')),
                mainPanel(
                  fluidRow(
                    column(6,
                           h3(textOutput('market_overview')),
                           plotOutput('market_plot')),
                    column(6,
                           h3(textOutput('current_offers_text')),
                           tableOutput('current_offers'))),
                  fluidRow(column(12,
                                  h4(textOutput('user_information')),
                                  tableOutput('this_user_table')))))))
