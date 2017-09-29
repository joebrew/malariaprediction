library(shiny)
library(malariaprediction)
library(dplyr)

users <- load_table('users')
events <- load_table('events')
events <- events %>%
  arrange(short_statement)
transactions <- load_table('transactions')
transactions$timestamp <- as.POSIXct(transactions$timestamp)
offers <- load_table('offers')