#' Make an offer to sell or buy a statement and then transact
#' 
#' Make an offer to sell or buy a statement and then transact if applicable.
#' This is just a wrapper for the combined use of make_offer and transact
#' @param users The users table
#' @param user_id A user id
#' @param event_id An event id
#' @param shares Number of shares
#' @param price The price per share
#' @param yes Boolean. Whether the offer is for or against
#' @return A row added to the offers table
#' @export

make_offer_and_transact <- function(users = NULL,
                       user_id = 1,
                       event_id = 1,
                       shares = 1,
                       price = 50,
                       yes = 1) {
  message('Making offer --------------')
  make_offer(users = users,
             user_id = user_id,
             event_id = event_id,
             shares = shares,
             price = price,
             yes = yes)
  message('Transacting --------------')
  transact()
}
