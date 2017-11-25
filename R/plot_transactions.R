#' Plot transactions for an event
#'
#' Plot transactions for a certain event
#' @param event_id an event id
#' @param transactions a transactions table
#' @return A row added to the corresponding google sheet
#' @export

plot_transactions <- function(event_id,
                              transactions = NULL){
  require(dplyr)
  require(ggplot2)
  require(stringr)
  require(highcharter)
  if(is.null(transactions)){
    transactions <- load_table('transactions')
  }
  eid <- event_id
  df <- transactions %>%
    filter(event_id == eid) %>%
    mutate(timestamp = as.POSIXct(timestamp)) %>%
    arrange(timestamp)
  if(nrow(df) > 0){
    t <- 'areasplinerange'
    dont_rm_high_and_low <- c("arearange", "areasplinerange",
                                   "columnrange", "errorbar")

    is_polar <- stringr::str_detect(t, "polar")

    t <- str_replace(t, "polar", "")

    if(!t %in% dont_rm_high_and_low) df <- df %>% select(-e, -low, -high)

    x <- df %>%
      arrange(timestamp) %>%
      mutate(date = as.Date(timestamp)) %>%
      group_by(date) %>%
      summarise(Open = dplyr::first(price),
                High = max(price),
                Low = min(price),
                Close = dplyr::last(price))
    highchart() %>%
      hc_add_series_ohlc()
      hc_add_series(data = df[,c('timestamp', 'price')],
                    type = 'spline')
      hc_xAxis(title = list(text = "x Axis at top"),
               opposite = TRUE,
               plotLines = list(
                 list(label = list(text = "This is a plotLine"),
                      color = "#'FF0000",
                      width = 2,
                      value = 5.5)))
      hc_title(text = paste(ifelse(is_polar, "polar ", ""), t),
               style = list(fontSize = "15px")) %>%
      hc_chart(type = t,
               polar = is_polar) %>%
      hc_xAxis(categories = df$timestamp) %>%
      hc_add_series(df$price, name = "Fruit Consumption", showInLegend = FALSE)
    # g <- ggplot(data = df,
    #             aes(x = timestamp,
    #                 y = price,
    #                 group = 1)) +
    #   geom_point(alpha = 0.8) +
    #   geom_line(alpha = 0.7) +
    #   geom_line(stat="smooth",
    #             method = "lm",
    #             formula = y ~ 0 + I(1/x) + I((x-1)/x),
    #             size = 1.5,
    #             color = 'blue',
    #             alpha = 0.5) +
    #   theme_publication() +
    #   labs(x = 'Date',
    #        y = 'Percentage',
    #        title = paste0(round(last(df$price)), '%'),
    #        subtitle = 'Current market-estimated probability') +
    #   theme(axis.text.x = element_text(angle = 90)) +
    #   ylim(0, 100)
      # scale_x_datetime(breaks =df$time_stamp , labels = format(df$time_stamp, "%Y-%m-%d %H:%M"))
  } else {
    g <- highchart() #ggplot() +
      # theme_publication() +
      # labs(title = 'No trades yet for this event')
  }
  return(g)
}
