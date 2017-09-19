#' Read all tables from the "database"
#' 
#' Read all the google sheets for the malaria prediction market project
#' @return Several tables read into memory
#' @export

load_all_data <- function() {
	require(googlesheets)
  tables <- c("credentials", "events", "offers", "transactions", "users")
  for (i in 1:length(tables)){
    this_table_name <- tables[i]
    this_table <- load_table(table = this_table_name)
    assign(this_table_name,
           this_table,
           envir = .GlobalEnv)
  }
}