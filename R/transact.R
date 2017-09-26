#' Transact
#' 
#' Enact all possible transactions
#' @return transactions, users and offers tables updated
#' @export

transact <- function() {
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
  # See if there are any yes shares greater than their corresponding no shares
  event_ids <- sort(unique(offers$event_id))
  ever_deal <- c()
  for (i in 1:length(event_ids)){
    message('Event id ', i)
    # If the no is greater than the yes make the transaction
    suppressWarnings(
      any_deals <- 
        max(offers$price[offers$event_id == event_ids[i] & 
                           offers$yes == 0 &
                           offers$valid == 1]) >=
        min(offers$price[offers$event_id == event_ids[i] & 
                           offers$yes == 1 &
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
      deal_no <- ns$temp_id[ns$price >= min(ys$price)][1]
      deal_yes <- ys$temp_id[ys$price <= max(ns$price)][1]
      deal_no_id <- offers$user_id[offers$temp_id == deal_no]
      deal_yes_id <- offers$user_id[offers$temp_id == deal_yes]
      # Make the transaction at the price of the yes
      # first, invalidate the offers (since they're no longer going to be relevant)
      offers$valid[offers$temp_id %in% c(deal_no,
                                                 deal_yes)] <- 0
      # Then, create a transaction
      the_price <- ys$price[ys$temp_id == deal_yes]
      malariaprediction::add_row(data = data_frame(transaction_id = max(transactions$transaction_id) + 1,
                                event_id = event_ids[i],
                                to = deal_yes_id,
                                from = deal_no_id,
                                price = the_price,
                                timestamp = Sys.time()),
              table = 'transactions')
      # And update the users table accordingly
      these_changes <- users$user_id %in% c(deal_no_id,
                                      deal_yes_id)
      users <- users %>%
        mutate(amount_invested = 
                 ifelse(these_changes,
                        amount_invested + the_price,
                        amount_invested)) %>%
        mutate(amount_offered = ifelse(these_changes,
                                       amount_offered - the_price,
                                       amount_offered))

      suppressWarnings(
        any_deals <- 
          max(offers$price[offers$event_id == event_ids[i] & 
                             offers$yes == 0 &
                             offers$valid == 1]) >=
          min(offers$price[offers$event_id == event_ids[i] & 
                             offers$yes == 1 &
                             offers$valid == 1]))
      
    }
  }  
  # Having modified the offers tables
  # replace it  on google sheets
  # (don't need to do this to transactions, since they were just
  # added to, not modified)
  if(any(ever_deal)){ # only overwrite if changes were made
    offers$temp_id <- NULL
    overwrite_these <- c('offers')
    for (i in 1:length(overwrite_these)){
      message('Overwriting the ', overwrite_these[i], ' table.')
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
  message('Done with transaction.')
}
