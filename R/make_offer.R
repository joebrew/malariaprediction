#' Make an offer to sell or buy a statement
#' 
#' Make an offer to sell or buy a statement.
#' @param users The users table
#' @param user_id A user id
#' @param event_id An event id
#' @param shares Number of shares
#' @param price The price per share
#' @param yes Boolean. Whether the offer is for or against
#' @return A row added to the offers table
#' @export

make_offer <- function(users = NULL,
                       transactions = NULL,
                       user_id = 1,
                       event_id = 1,
                       shares = 1,
                       price = 50,
                       yes = 1) {
	require(googlesheets)
  require(dplyr)
  # Get a fresh users table
  this_user <- user_id
  if(is.null(users)){
    users <- load_table('users') 
  }
  if(is.null(transactions)){
    transactions <- load_table('transactions') 
  }
  x <- users %>%
    filter(user_id == this_user)
  # update users
  x <- update_users(users = x,
                    transactions = transactions)
  # Ensure that there is enough to offer
  available <- x$amount - x$amount_invested - x$amount_offered
  if(available >= (price * shares)){
    # Put the data in a dataframe
    data <- data_frame(user_id,
                       event_id,
                       price,
                       yes,
                       valid = 1,
                       timestamp = Sys.time())
    if(shares > 1){
      new_data <- list()
      for(i in 1:shares){
        new_data[[i]] <- data
      }
      data <- bind_rows(new_data)
    }
    
    
    # Grab the Google Sheet
    sheet <- gs_title('offers_malariaprediction')
    # Add the new offer
    for (i in 1:nrow(data)){
      message('Adding row ', i, ' of ', nrow(data))
      gs_add_row(ss = sheet, 
                 input = data[i,])
    }
  } else {
    message('Offer not made. Insufficient funds.')
    
  }
  
}
