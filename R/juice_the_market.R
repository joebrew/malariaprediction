#' Juice the market
#' 
#' Make a bunch of random offers from one person to get the market started
#' @param n The number of random offers to make
#' @param ids the user id(s)
#' @param overwrite Whether to overwrite or add
#' @return A row added to the offers table
#' @export

juice_the_market <- function(n = 1000,
                               ids = c(0, -1, -2),
                               overwrite = FALSE) {
	require(googlesheets)
  require(dplyr)
  require(readr)
  # Get a fresh users table
  users <- load_table('users') 
  # Get a fresh events table
  events <- load_table('events')
  # Get a fresh offers table
  offers <- load_table('offers')
  
  # Create a new offers table
  prices <- rnorm(mean = 50, sd = 20, n = n)
  prices <- round(prices)
  prices <- ifelse(prices > 99, 99,
                   ifelse(prices < 1, 1,
                   prices))
  times <- as.POSIXct('2017-09-25 23:52:15')
  times <- times - sample(1:2000000, n, replace = TRUE)
  times <- as.character(times)
  ids <- sample(ids, n, replace = TRUE)
  new_offers <- 
    data_frame(user_id = ids,
               event_id = sample(events$event_id, n, replace = TRUE),
               price = prices,
               yes = sample(0:1, n, replace = TRUE),
               valid = 1,
               timestamp = times)
  if(overwrite){
    menu(c("Yes", "No"), title="You are about to overwrite the offers table. Are you sure you want to do this?")
    offers <- new_offers
  } else {
    offers <- bind_rows(offers, new_offers)
  }
  
  # Overwrite the corresponding offers table
  readr::write_csv(offers,
                   'temp.csv')
  gs_upload(file = 'temp.csv',
            sheet_title = paste0('offers', '_malariaprediction'),
            overwrite = TRUE)
  file.remove('temp.csv')
  
  # Transact
  transact(upload = FALSE,
           spread_out_time = TRUE)
}
