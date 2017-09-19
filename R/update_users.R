#' Update users
#' 
#' Update the users table as a function of transactions
#' This does not actually modify the users datafame; it simply gives an in
#' memory update
#' @param users A users dataset
#' @param transactions A transactions dataset
#' @param offers An offers dataset
#' @return transactions, users and offers tables updated
#' @export

update_users <- function(users = NULL,
                         transactions = NULL,
                         offers = NULL) {
  require(googlesheets)
  require(dplyr)
  if(is.null(users)){
    users <- load_table('users')
  }
  if(is.null(transactions)){
    transactions <- load_table('transactions')
  } 
  if(is.null(offers)){
    offers <- load_table('offers')
  } 
  # Group by transaction and get how much has been invested
  invested <- transactions %>%
    group_by(user_id = to) %>%
    summarise(amount_invested =  sum(price)) %>%
    bind_rows(transactions %>%
              group_by(user_id = from) %>%
                summarise(amount_invested = sum(price))) %>%
    group_by(user_id) %>%
    summarise(amount_invested = sum(amount_invested))
  # Get how much is currently offered
  offered <-
    offers %>%
    filter(valid == 1) %>%
    group_by(user_id) %>%
    summarise(amount_offered = sum(price))
  # Join both to users
  users <-
    left_join(x = users,
              y = invested,
              by = 'user_id') %>%
    left_join(offered,
              by = 'user_id')
  # Fill up the nas, etc.
  users <- users %>%
    mutate(amount_invested = ifelse(is.na(amount_invested), 0, amount_invested),
           amount_offered = ifelse(is.na(amount_offered), 0, amount_offered)) %>%
    mutate(amount_available = amount - amount_invested - amount_offered)
  
}
