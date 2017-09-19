#' Plot transactions for an event
#' 
#' Plot transactions for a certain event
#' @param event_id
#' @return A row added to the corresponding google sheet
#' @export

plot_transactions <- function(event_id,
                              transactions = NULL){
  if(is.null(transactions)){
    transactions <- load_table('transactions')
  }
  eid <- event_id
  df <- transactions %>%
    filter(event_id == eid)
  if(nrow(df) > 0){
    g <- ggplot(data = df,
                aes(x = time_stamp,
                    y = price)) +
      geom_point() +
      geom_line() +
      theme_publication() +
      labs(x = 'Date',
           y = 'Percentage',
           title = paste0(round(last(df$price)), '%'),
           subtitle = 'Current market-estimated probability')
  } else {
    g <- ggplot() +
      theme_publication() +
      labs(title = 'No trades yet for this event')
  }
  return(g)
}