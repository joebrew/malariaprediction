#' Add a user to the users table
#' 
#' Add a row to the google sheet of 
#' @param data A dataframe
#' @param user_id A user id number
#' @param name A name
#' @param email An email
#' @param username A username
#' @param password A password
#' @param amount An amount
#' @return A row added to the corresponding google sheet
#' @export

add_user <- function(data = NULL,
                    user_id = 1,
                    name = 'John Doe',
                    email = 'joebrew@gmail.com',
                    username = 'username',
                    password = 'password',
                    amount = 100) {
	require(googlesheets)
  require(dplyr)
  # If a dataframe is required, use it
  # Otherwise, create one
  if(is.null(data)){
    data <- data_frame(user_id,
                       name,
                       email,
                       username,
                       password,
                       amount)
  }
  # Conform the format
  if(!all(names(data) == c('user_id',
                   'name',
                   'email',
                   'username',
                   'password',
                   'amount'))){
    stop('The data provided is not in the correct form')
  }
  add_row(data = data,
          table = 'users')
}
