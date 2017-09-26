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
                    y = price,
                    group = 1)) +
      geom_point(alpha = 0.8) +
      geom_line(alpha = 0.7) +
      theme_publication() +
      labs(x = 'Date',
           y = 'Percentage',
           title = paste0(round(last(df$price)), '%'),
           subtitle = 'Current market-estimated probability') +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_x_datetime(breaks =df$time_stamp , labels = format(df$time_stamp, "%Y-%m-%d %H:%M"))
  } else {
    g <- ggplot() +
      theme_publication() +
      labs(title = 'No trades yet for this event')
  }
  return(g)
}