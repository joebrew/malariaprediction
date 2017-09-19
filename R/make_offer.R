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
  users <- users %>%
    filter(user_id == this_user)
  # Ensure that there is enough to offer
  available <- users$amount - users$amount_invested - users$amount_offered
  if(available < (price * shares)){
    stop('Not enough available funds to make this offer.')
  }
  # Put the data in a dataframe
  data <- data_frame(user_id,
                     event_id,
                     price,
                     yes,
                     valid = 1)
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
}
