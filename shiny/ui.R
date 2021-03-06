library(dplyr)
library(Hmisc)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(googlesheets)
library(highcharter)

# Define the sidebar
sidebar <-
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", icon = icon("th"), tabName = "what",
               badgeLabel = "get started", badgeColor = "green"),
      menuItem("Buy and sell", tabName = "buy_and_sell", icon = icon("dashboard"),
               badgeLabel = "place bets", badgeColor = "red"),
      menuItem('Data analysis', icon = icon('th'), tabName = 'analysis',
               badgeLabel = 'explore', badgeColor = 'blue')),

    uiOutput("in.user"),
    uiOutput("in.pss"),
    uiOutput('in.action'),
    h4(textOutput('go_forward')),
    menuItem("Source code", icon = icon("file-code-o"),
             href = "https://github.com/rstudio/shinydashboard/"))

# Define the body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'buy_and_sell',
            fluidRow(column(5,
                            uiOutput("in.event"),
                            #h4(textOutput('event_full_header')),
                            uiOutput("in.yesno"),
                            uiOutput('in.offer')),
                     column(7,
                            uiOutput("in.prob"),
                            uiOutput("in.shares"))),
            fluidRow(column(12,
                            helpText(textOutput('yesno_explanation')))),
            fluidRow(h2(textOutput('event_full_statement'))),
            fluidRow(
              column(6,
                     h3(textOutput('market_overview')),
                     plotOutput('market_plot')),
              column(6,
                     h3(textOutput('spread_text')),
                     plotOutput('spread_plot_1'))),
            fluidRow(shinydashboard::valueBox(textOutput('time_left'),
                                              subtitle = 'Seconds until market resolution', icon = icon("fa-flag"),
                            color = 'red'),
                            shinydashboard::valueBox(textOutput('this_user_money'),
                                                     subtitle = 'Amount available for investing', icon = icon("fa-stethoscope"),
                            color = 'yellow'),
                            shinydashboard::valueBox(textOutput('this_user_invested'),
                                                     subtitle = 'Amount already invested', icon = icon("fa-binoculars"),
                            color = 'green')),
            fluidRow(
              column(6,
                     h3(textOutput('current_offers_text')),
                     tableOutput('current_offers')),
              column(6,
                     h3(textOutput('user_information')),
                     tableOutput('this_user_table'))
            )),
    tabItem(tabName = 'what',
            includeMarkdown("include.md")),
    tabItem(tabName = 'analysis',
            h2('All statements'),
            fluidRow(
              column(12,
                     plotOutput('all_markets_trajectories'))
            ),
            h2('Market spread'),
            fluidRow(column(12,
                            plotOutput('spread_plot'))),
            h2('Markets description'),
            fluidRow(column(12,
                            dataTableOutput('all_markets_table'))),
            h2('Raw data'),
            fluidRow(column(3,
                            downloadButton("downloadData", "Download")),
                     column(9,
                            dataTableOutput('raw_data_table'))))
  )
)

dashboardPage(
  dashboardHeader(title = 'The market'),
  sidebar,
  body)
