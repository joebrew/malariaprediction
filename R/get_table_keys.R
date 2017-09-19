#' Get table keys
#' 
#' Get all the table keys into a dataframe
#' @return A dataframe
#' @export

get_table_keys <- function(){
  if('table_keys.RData' %in% dir()){
    load('table_keys.RData')
    return(table_keys)
  } else {
    # Get table keys
    require(dplyr)
    require(googlesheets)
    possible_tables <- c("credentials", "events", "offers", "transactions", "users")
    
    df <- data_frame(table = possible_tables,
                     table_name = paste0(possible_tables, '_malariaprediction'),
                     key = NA)
    for (i in 1:nrow(df)){
      x <- gs_title(paste0(df$table_name[i]))
      df$key[i] <- x$sheet_key
    }
  }
  table_keys <- df
  save(table_keys,
       file = 'table_keys.RData')
  return(table_keys)
}
