#' Add a row to one of the tables in the "database"
#' 
#' Add a row to the google sheet of 
#' @param data A dataframe
#' @param table One of "credentials", "events", "offers", "transactions" or "users"
#' @return A row added to the corresponding google sheet
#' @export

add_row <- function(data, table = 'events') {
	require(googlesheets)
  possible_tables <- c("credentials", "events", "offers", "transactions", "users")
  if(!table %in% possible_tables){
    stop("table must be one of ", 
      paste0(possible_tables, collapse = ', '))
  }
  # Append the suffix
  table <- paste0(table, '_malariaprediction')
  # Grab the Google Sheet
  sheet <- gs_title(table)
  # Add the data as a new row
  for (i in 1:nrow(data)){
    message('Adding row ', i, ' of ', nrow(data))
    gs_add_row(ss = sheet, 
               input = data[i,])
  }
  
}
