#' Read an entire table from the "database"
#' 
#' Read an entire google sheet
#' @param data A dataframe
#' @param table One of "credentials", "events", "offers", "transactions" or "users"
#' @return A row added to the corresponding google sheet
#' @export

load_table <- function(table = 'events') {
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
  # Read the data
  gs_read_csv(sheet)
}