#' Transact
#' 
#' Enact all possible transactions
#' @param upload Whether to upload each new row. Alternatively, finishes all the
#' transactions and then bulk uploads transactions, overwriting the old table
#' @param spread_out_time Whether to spread out the time a little bit (useful for juicing the market, and
#' only appliable if upload = FALSE)
#' @return transactions, users and offers tables updated
#' @export

transact <- function(upload = TRUE,
                     spread_out_time = FALSE) {
  require(googlesheets)
  require(dplyr)
  # Get some tables
  offers <- load_table('offers')
  transactions <- load_table('transactions')
  users <- load_table('users')
  # Update users
  users <- update_users(users = users,
                        transactions = transactions,
                        offers = offers)
  # Create a temporary id in offers
  offers$temp_id <- 1:nrow(offers)
  # Remove all old offers
  offers <- offers %>% filter(valid == 1)
  # See if there are any yes shares greater than or equal to the inverse of their corresponding no shares
  offers$yes_price <- ifelse(offers$yes == 1, 
                             offers$price,
                             100 - offers$price)
  event_ids <- sort(unique(offers$event_id))
  ever_deal <- c()
  out_list <- list()
  counter <- 0
  for (i in 1:length(event_ids)){
    message('Event id ', i)
    # If the no is greater than the yes make the transaction
    suppressWarnings(
      any_deals <- 
        max(offers$yes_price[offers$event_id == event_ids[i] & 
                           offers$yes == 1 &
                           offers$valid == 1]) >=
        min(offers$yes_price[offers$event_id == event_ids[i] & 
                           offers$yes == 0 &
                           offers$valid == 1]))
    ever_deal <- c(ever_deal, any_deals)
    while(any_deals){
      
      message('Doing a deal for event id ', i)
      # Identify the deal
      # Get the data for this event only
      this_event <- offers %>%
        filter(event_id == event_ids[i],
               valid == 1)
      # Identify all the yeses
      ys <- this_event %>% filter(yes == 1)
      # Identify all the nos
      ns <- this_event %>% filter(yes == 0)
      # Identify the deal
      deal_no <- ns$temp_id[ns$yes_price <= max(ys$yes_price)][1]
      deal_yes <- ys$temp_id[ys$yes_price >= min(ns$yes_price)][1]
      deal_no_id <- offers$user_id[offers$temp_id == deal_no]
      deal_yes_id <- offers$user_id[offers$temp_id == deal_yes]
      # Make the transaction at the price of the yes
      # first, invalidate the offers (since they're no longer going to be relevant)
      offers$valid[offers$temp_id %in% c(deal_no,
                                                 deal_yes)] <- 0
      # Then, create a transaction
      tid <- nrow(transactions) + 1
      the_price <- ys$yes_price[ys$temp_id == deal_yes]
      the_data <- data_frame(transaction_id = tid,
                             event_id = event_ids[i],
                             to = deal_yes_id,
                             from = deal_no_id,
                             price = the_price,
                             timestamp = Sys.time())
      if(upload){
        malariaprediction::add_row(data = the_data,
                                   table = 'transactions')
      } else {
        counter <- counter + 1
        message(counter)
        out_list[[counter]] <- the_data
      }
      
  # See if there are any more deals to make
      suppressWarnings(
        any_deals <- 
          max(offers$yes_price[offers$event_id == event_ids[i] & 
                                 offers$yes == 1 &
                                 offers$valid == 1]) >=
          min(offers$yes_price[offers$event_id == event_ids[i] & 
                                 offers$yes == 0 &
                                 offers$valid == 1]))
      
    }
  }  
  # Having modified the offers tables
  # replace it  on google sheets
  # (don't need to do this to transactions, since they were just
  # added to, not modified)
  # (don't need to do this to users, since they can be updated in memory
  # using update_users)
  if(any(ever_deal)){ # only overwrite if changes were made
    offers$temp_id <- NULL
    offers$yes_price <- NULL
    overwrite_these <- c('offers')
    for (i in 1:length(overwrite_these)){
      message('Overwriting the ', overwrite_these[i], ' table, since deals have been made.')
      # Write a temporary file
      readr::write_csv(get(overwrite_these[i]),
                       'temp.csv')
      gs_upload(file = 'temp.csv',
                sheet_title = paste0(overwrite_these[i], '_malariaprediction'),
                overwrite = TRUE)
      file.remove('temp.csv')
    }
  } else {
    message('No deals were made. Not overwriting anything.')
  }
  if(!upload){
    out <- bind_rows(out_list)
    # Combine with old transactions
    transactions <- bind_rows(transactions, out)
    if(spread_out_time){
      transactions$timestamp <- 
        max(transactions$timestamp) - sample(1:2000000, nrow(transactions), replace = TRUE)
    }
    # Write a temporary file
    readr::write_csv(transactions,
                     'temp.csv')
    gs_upload(file = 'temp.csv',
              sheet_title = paste0('transactions', '_malariaprediction'),
              overwrite = TRUE)
    file.remove('temp.csv')
  }
  message('Done with transacting.')
}
