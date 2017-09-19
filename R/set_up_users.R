#' Set up the users table
#' 
#' Populate the users table. Should only be run once
#' @return The users spreadsheet will be populated
#' @export

set_up_users <- function() {
  require(dplyr)
  require(progress)
  
  # Ensure that this is intentional
  if(menu(c("Yes", "No"), title="Are you sure you want to set up the users table?") != 1){
    stop('Cancelling')
  } else {
    message('Okay, lets do this.')
  }
  
  # Get the data on each author
  email_df <- malariaprediction::email_df
  
  # Add each user to the database
  results_list <- list()
  pb <- progress_bar$new(total = nrow(email_df))
  message('Getting everyones email address and password')
  for (i in 1:nrow(email_df)){
    pb$tick()
    this_person <- data_frame(user_id = i,
                    name = email_df$author[i],
                    email = email_df$email[i],
                    username = email_df$user_name[i],
                    password = email_df$password[i],
                    amount = 1000,
                    amount_invested = 0,
                    amount_offered = 0)
    results_list[[i]] <- this_person
    # add_user(data = this_person)
  }
  results <- bind_rows(results_list)
  add_user(data = results)
}
