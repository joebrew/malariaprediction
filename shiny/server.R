pok <- F
shinyServer(function(input, output) 
{
  output$in.pss   <- 
    renderUI({ input$pss; if (pok) return(NULL) else return(textInput("pss","Password:","")) })
  
  output$in.user   <- 
    renderUI({ input$pss; if (pok) return(NULL) else return(textInput("user","Username:","")) })
  
  output$in.clr   <- 
    renderUI({ input$pss; if (pok) return(selectInput("clr","Color:",c("red","blue"))) else return(NULL) })
  
  output$in.titl  <- 
    renderUI({ input$pss; if (pok) return(textInput("titl","Title:","Data")) else return(NULL) })
  
  output$in.cnt   <- 
    renderUI({ input$pss; if (pok) return(sliderInput("cnt","Count:",100,1000,500,5)) else return(NULL) })
  
  output$in.seed  <- 
    renderUI({ input$pss; if (pok) return(numericInput("seed","Seed:",1234,1,10000,1)) else return(NULL) })
  
  histdata <- reactive(
    {
      input$pss;
      validate(need(input$cnt,"Need count"),need(input$seed,"Need seed"))
      set.seed(input$seed)
      df <- data.frame(x=rnorm(input$cnt))
    }
  )
  observe({
    if (!pok) {
      password <- input$pss
      if (!is.null(password) && password == "pass") {
        pok <<- TRUE
      }
    }
  }
  )
  output$echo = renderText(
    {
      if (pok) {
        s <- sprintf("the %s is %s and has %d rows and uses the %d seed",
                     input$ent,input$clr,nrow(histdata()),input$seed)
      } else {
        s <- ""
      }
      return(s)
    }
  )
  output$stdplot = renderPlot(
    {
      input$pss
      if (pok) {
        return(qplot(data = histdata(),x,fill = I(input$clr),binwidth = 0.2,main=input$titl))
      } else {
        return(NULL)
      }
    }
  )
  
 
  
})