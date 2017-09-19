#' Make random offers from random users for random event ids
#' 
#' Make random offers from random users for random events. This is used for testing only.
#' @param n The number of random offers to make
#' @return A row added to the offers table
#' @export

make_random_offers <- function(n = 20) {
	require(googlesheets)
  require(dplyr)
  # Get a fresh users table
  users <- load_table('users') 
  # Get a fresh events table
  events <- load_table('events')
  
  # Make the offers
  for (i in 1:n){
    try({
      message('Making random offer number ', i, ' of ', n)
      user_id <- sample(users$user_id, 1)
      event_id <- sample(events$event_id, 1)
      shares <- sample(1:10, 1)
      price <- sample(0:100, 1)
      yes <- sample(0:1, 1)
      message('--- User ', user_id, ' is offering ', 
              shares, ' shares at ', price, ' cents ',
              ifelse(yes == 1, 'for', 'against'),
              ' event number ',
              event_id)
      make_offer(users = NULL, 
                 user_id = user_id, 
                 event_id = event_id, 
                 shares = shares,
                 price = price, 
                 yes = yes)
    })

  }
}
