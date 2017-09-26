#' Get spread
#' 
#' Get the current spread of all offers
#' @param offers An offers table
#' @param events An events table
#' @param plot Whether to plot or not
#' @return A row added to the corresponding google sheet
#' @export

get_spread <- function(offers = NULL,
                       events = NULL,
                       plot = TRUE){
  require(ggplot2)
  require(dplyr)
  require(tidyr)
  
  if(is.null(offers)){
    offers <- load_table('offers')
  }
  if(is.null(events)){
    events <- load_table('events')
  }
  out <- offers %>%
    filter(valid == 1) %>%
    arrange(timestamp) %>%
    group_by(event_id, yes) %>%
    summarise(last_offer = dplyr::last(price)) %>%
    ungroup
  # Get an expanded grid
  left <- expand.grid(event_id = sort(unique(events$event_id)),
                      yes = c(0, 1))
  out <- left_join(left, out, by = c('event_id', 'yes'))  
  out <-
    out %>% group_by(event_id) %>%
    summarise(last_yes = last_offer[yes == 1],
              last_no = last_offer[yes == 0]) %>%
    left_join(events %>% dplyr::select(event_id, short_statement),
              by = 'event_id')
  if(plot){
    plot_data <- out %>%
      gather(key, value, last_yes:last_no) %>%
      mutate(key = gsub('last_', '', key))
    out <-
      ggplot(data = plot_data) +
      geom_point(aes(x = short_statement,
                     y = value,
                     color = key),
                 alpha = 0.6) +
      geom_linerange(data = out,
                     aes(x = short_statement,
                         ymax = last_yes,
                         ymin = last_no),
                     alpha = 0.6) +
      labs(x = '',
           y = 'Points') +
      theme_publication() +
      theme(axis.text.x = element_text(angle = 90)) +
      scale_color_manual(name = '',
                         values = c('red', 'green'),
                         guide = guide_legend(ncol = 1,
                                              reverse = TRUE)) +
      theme(legend.position = 'right')
  
  }
  return(out)

}