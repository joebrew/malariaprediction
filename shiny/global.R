library(shiny)
library(malariaprediction)
library(dplyr)

users <- load_table('users')
events <- load_table('events')
events <- events %>%
  arrange(short_statement)
transactions <- load_table('transactions')
transactions$time_stamp <- as.POSIXct(transactions$time_stamp)
offers <- load_table('offers')