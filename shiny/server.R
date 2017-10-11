library(shiny)
library(dplyr)
library(ggplot2)

pok <- F
event_id <- function(){'hey'}
shinyServer(function(input, output, session){

  output$in.pss   <-
    renderUI({ input$action;  #input$pss;
      if (pok) return(NULL) else #Sys.sleep(3);
        return(textInput("pss","Password:","")) })

  output$in.user   <-
    renderUI({ input$action;  #input$pss;
      if (pok) return(NULL) else return(textInput("user","Username:","")) })

  output$in.action <-
    renderUI({
      input$action # observing self? wohhhh
      if(pok){
        return(NULL)
      } else {
        actionButton("action", "Submit")
      }

    })

  output$in.offer <-
    renderUI({ input$action;
      if(pok){
        return(actionButton("offer", "Make buy/sell offer"))
      } else {
        return(NULL)
      }
    })

  output$in.event   <-
    renderUI({ input$action;  #input$pss;
      if (pok) return(selectInput("event","Event:",
                                  events$short_statement)) else return(NULL) })

  output$event_full_header <-
    renderText({
      input$action;  #input$pss;
      if(pok){
        return('Full statement: ')
      } else {
        return(NULL)
      }
    })

  output$event_full_statement <-
    renderText({
      input$action;  #input$pss;
      if(pok){
        return(events$statement[events$short_statement == input$event])
      } else {
        return(NULL)
      }
    })

  output$user_information <-
    renderText({
      input$action;
      if(pok){
        return('User information: ')
      } else {
        return(NULL)
      }
    })

  output$yesno_explanation <-
    renderText({
      input$action;  #input$pss;
      if(pok){
        return(paste0("Select 'yes' above to make an offer to bet that the event will occur. Select 'no' to make an offer to bet that the event will NOT occur. After that, select your perceived probability (which corresponds to the price of your offer) below. For example, if you select 'yes' and 60%, this means you are willing to bet 60 points that the event will occur; if you select 'no' and 82%, this means that you are willing to bet 82 points that the event will not occur. After the event resolution (ie, it occurs or not), you will be given 100 points if correct, and 0 if incorrect."))
      } else {
        return(NULL)
      }
    })

  output$market_overview <-
    renderText({
      input$action;  #input$pss;
      if(pok){
        return('Market overview')
      } else {
        return(NULL)
      }
    })

  output$spread_text <-
    renderText({
      input$action;  #input$pss;
      if(pok){
        return('Market spread')
      } else {
        return(NULL)
      }
    })

  output$current_offers_text <-
    renderText({
      input$action;  #input$pss;
      if(pok){
        return('Current offers from others')
      } else {
        return(NULL)
      }
    })

  output$in.yesno  <-
    renderUI({
      input$action;  #input$pss;
      if (pok) return(radioButtons("yesno","Yes or No",
                                   choices = c('Yes', 'No'))) else return(NULL) })

  output$in.prob   <-
    renderUI({ input$action;  #input$pss;
      if (pok) return(sliderInput("prob","Probability (price):",0,100,50,1)) else return(NULL) })

  output$in.shares   <-
    renderUI({ input$action;  #input$pss;
      if (pok) return(sliderInput("n_shares","Number of shares:",0,50,1,1)) else return(NULL) })

  observe({
    input$action;  #input$pss;
    if (!pok) {
      password <- input$pss
      user <- input$user
      if (!is.null(password) && !is.null(user)){
        this_user <- users %>%
          filter(username == user)
        if(nrow(this_user) == 1){
          if(this_user$password == password){
            pok <<- TRUE
            # pok <- TRUE
          }
        }
      }
    }
  })

  # Create reactive user based inputs
  user_id <- reactive({
    input$action
    if(pok){
      return(users$user_id[users$username == input$user])
    } else {
      return(0)
    }
  })
  event_id <- reactive({
    input$action
    if(pok){
      out <-
        events %>%
        filter(short_statement == input$event) %>%
        .$event_id
      return(out)
    } else {
      return(0)
    }
  })
  shares <- reactive({
    input$action
    if(pok){
      return(input$n_shares)
    } else {
      return(1)
    }
  })
  price <- reactive({
    input$action
    if(pok){
      return(input$prob)
    } else {
      return(50)
    }
  })
  yes <- reactive({
    input$action
    if(pok){
      return(ifelse(input$yesno == 'Yes', 1, 0))
    } else {
      return(0)
    }
  })


  output$market_plot <- renderPlot({
    g <- ggplot() +
      theme_publication() +
      labs(title = '')
    input$action
    if(pok){
      if(!is.null(input$event)){
        eid <- event_id()
      } else {
        eid <- 999
      }

      g <- plot_transactions(event_id = eid,
                             transactions = NULL) # to get fresh
      return(g)
    } else {
      return(NULL)
    }
  })

  output$all_markets_trajectories <- renderPlot({
      plot_data <- transactions %>%
        left_join(events %>% dplyr::select(event_id, short_statement)) %>%
        arrange(timestamp)
      plot_data$short_statement <-
        gsub(' malaria ', ' ', plot_data$short_statement)
      plot_data$short_statement <-
        gsub(' in ', ' ', plot_data$short_statement)
      plot_data$short_statement <-
        gsub(' by ', ' ', plot_data$short_statement)
      plot_data$short_statement <-
        gsub('Malaria elimination ',
             '',
             plot_data$short_statement)
      plot_data <- plot_data %>% filter(!is.na(short_statement))
      g <- ggplot(data = plot_data,
                  aes(x = timestamp,
                      y = price)) +
        geom_line() +
        theme_publication() +
        facet_wrap(~short_statement) +
        labs(x = '',
             y = '%') +
        geom_smooth() +
        theme(axis.text.x = element_text(angle = 90)) +
        ylim(0, 100)
      return(g)
  })

    output$spread_plot <- renderPlot({
        g <- get_spread(offers = offers,
                        events = events,
                        plot = TRUE)
        return(g)
    })

    output$spread_plot_1 <- renderPlot({
      g <- ggplot() +
        theme_publication() +
        labs(title = '')
      input$action
      if(pok){
        if(!is.null(input$event)){
          eid <- event_id()
        } else {
          eid <- 999
        }

        g <- get_spread(offers = offers,
                        events = events,
                        eid = eid)
        return(g)
      } else {
        return(NULL)
      }
    })

    # Create offer if need be
    observeEvent(eventExpr = input$offer,
                 {
                   message('User is ', user_id())
                   message('Event is ', event_id())
                   message('Shares are ', shares())
                   message('Price is ', price())
                   message('Yes is ', yes())
                   handlerExpr = make_offer_and_transact(user_id = user_id(),
                                                         event_id = event_id(),
                                                         shares = shares(),
                                                         price = price(),
                                                         yes = yes())})

    # Create a reactive users object
    all_users <- reactive({
      input$action
      if(pok){
        x <- update_users()
        return(x)
      } else {
        return(NULL)
      }
    })


    # Create a reactive user object
    this_user <- reactive({
      input$action
      if(pok){
        x <- all_users()
        uid <- users$user_id[users$username == input$user]
        out <- x %>%
          filter(user_id == uid)
        return(out)
      } else {
        return(NULL)
      }
    })

    # Create a table of this user
    output$this_user_table <- renderTable({
      input$action
      if(pok){
        x <- this_user()
        x
      } else {
        return(NULL)
      }
    })

    # Create a table of current offers
    output$current_offers <- renderTable({
      input$action
      if(pok){
        if(!is.null(input$event)){
          eid <- event_id()
        } else {
          eid <- 999
        }
        offs <- get_spread(offers = offers,
                        events = events,
                        eid = eid,
                        plot = FALSE)
        offs <- offs %>%
          tidyr::gather(key,
                        value,
                        last_yes:last_no)
        offs <- offs %>%
          mutate(x = ifelse(key == 'last_yes', value,
                            ifelse(key == 'last_no', 100-value, NA))) %>%
          summarise(`Highest buy offer` = x[key == 'last_yes'],
                    `Lowest sell offer` = x[key == 'last_no'])
        return(offs)
      } else {
        return(NULL)
      }
    })

    # Create a table of all markets
    output$all_markets_table <- renderDataTable({
        out <- get_spread(events = events, plot = FALSE)
        if(nrow(out) == 0){
          out <- data_frame(`All markets` = 'None')
        }
        return(out)
    })

    # Create a table for export
    raw_data <- reactive({
      has_transactions <- FALSE
      if(exists('transactions')){
        if(!is.null(transactions)){
          has_transactions <- TRUE
        }
      }
      if(!exists('events')){
        has_transactions <- FALSE
      }
      if(has_transactions){
        out <- transactions %>%
          dplyr::select(event_id,
                        price,
                        timestamp) %>%
          left_join(events %>%
                      dplyr::select(event_id,
                                    short_statement,
                                    statement,
                                    close_date),
                    by = 'event_id') %>%
          arrange(event_id,
                  timestamp)
        return(out)
      } else {
        return(NULL)
      }
    })


    output$downloadData <- downloadHandler(
      filename = function() {
        paste('raw_data', ".csv", sep = "")
      },
      content = function(file) {
        write.csv(raw_data(),
                  file,
                  row.names = FALSE)
      }
    )

    output$raw_data_table <- renderDataTable({
      raw_data()
    })

    # Countdown timer
    end_date <- reactive({
      input$action
      if(pok){
        return(events$close_date[events$short_statement == input$event][1])
      } else {
        return(NULL)
      }
    })
    output$time_left <- renderText({
      invalidateLater(1000, session)
      round(difftime(end_date(), Sys.time(), units='secs'))
    })

  })
