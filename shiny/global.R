library(shiny)
library(dplyr)
library(ggthemes)
library(malariaprediction)

users <- load_table('users')
events <- load_table('events')
events <- events %>%
  arrange(short_statement)
transactions <- load_table('transactions')
transactions$timestamp <- as.POSIXct(transactions$timestamp)
offers <- load_table('offers')
