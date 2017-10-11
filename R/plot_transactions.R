#' Plot transactions for an event
#'
#' Plot transactions for a certain event
#' @param event_id an event id
#' @param transactions a transactions table
#' @return A row added to the corresponding google sheet
#' @export

plot_transactions <- function(event_id,
                              transactions = NULL){
  if(is.null(transactions)){
    transactions <- load_table('transactions')
  }
  eid <- event_id
  df <- transactions %>%
    filter(event_id == eid) %>%
    mutate(timestamp = as.POSIXct(timestamp)) %>%
    arrange(timestamp)
  if(nrow(df) > 0){
    g <- ggplot(data = df,
                aes(x = timestamp,
                    y = price,
                    group = 1)) +
      geom_point(alpha = 0.8) +
      geom_line(alpha = 0.7) +
      geom_line(stat="smooth",
                method = "lm",
                formula = y ~ 0 + I(1/x) + I((x-1)/x),
                size = 1.5,
                color = 'blue',
                alpha = 0.5) +
      theme_publication() +
      labs(x = 'Date',
           y = 'Percentage',
           title = paste0(round(last(df$price)), '%'),
           subtitle = 'Current market-estimated probability') +
      theme(axis.text.x = element_text(angle = 90)) +
      ylim(0, 100)
      # scale_x_datetime(breaks =df$time_stamp , labels = format(df$time_stamp, "%Y-%m-%d %H:%M"))
  } else {
    g <- ggplot() +
      theme_publication() +
      labs(title = 'No trades yet for this event')
  }
  return(g)
}
