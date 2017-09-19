#' Invite participants to participate in the market
#' 
#' Add a row to the google sheet of 
#' @param data A dataframe
#' @param table One of "credentials", "events", "offers", "transactions" or "users"
#' @return A row added to the corresponding google sheet
#' @export

invite_participants <- function() {
  

  
  require(mailR)
  
  connection <- file('credentials/password.txt')
  connection2 <- file('credentials/password2.txt')
  password <- readLines(connection)
  password2 <- readLines(connection2)
  close(connection)
  close(connection2)
  # Define function for sending emails
  sendify <- function(df){
    
    # Define body
    body <- paste0(
      'Dear Dr. ', 
      df$last_name,
      '\n\n',
      'I am writing to you as the author of "',
      df$title,
      '", published in ',
      df$year,
      '.\n\n',
      'For my PhD research on the economics of malaria, I have created a ',
      '"malaria eradication marketplace" with the aim of quantifying ',
      'the likelihood and timeframe of malaria elimination in certain countries and eradication globally. ', 
      'Given your research/publication history, ',
      'I was hoping that you would consider participating in the prototype ',
      'bla bla bla',
      
      'Thank you very much for your time. If you have any questions, please do not ',
      'hesitate to contact me.\n\n',
      'Best,\n\n',
      'Joe Brew\n',
      'Barcelona Institute for Global Health (www.isglobal.org)\n\n',
      '(P.S. If you want more details on this research, visit ',
      'https://github.com/joebrew/malaria_survey#can-we-do-it-a-survey-of-research-professionals-on-the-timeline-and-obstacles-to-eliminating-malaria .)'
      
      
    )
    
    # Define subject
    the_subject <- paste0(
      'Hi Dr. ',
      df$last_name,
      ' - predicting malaria elimination and eradication'
    )
    
    # Define sender
    if(i %% 2 == 1){
      sender <- 'joebrew@gmail.com'
    } else {
      sender <- 'joe.brew@isglobal.org'
    }
    
    # Define passowrd
    if(sender == 'joebrew@gmail.com'){
      password <- password
    } else {
      password <- password2
    }
    send.mail(from = sender,
              to = as.character(df$email),
              subject = the_subject,
              body = body,
              smtp = list(host.name = "smtp.gmail.com", 
                          port = 465, 
                          # user.name="joebrew@gmail.com", 
                          user.name = sender,
                          passwd=password, 
                          ssl=TRUE),
              authenticate = TRUE,
              # attach.files = '../in_kind_proposal.pdf',
              send = TRUE)
  }
  # 
  # start_at <- read.csv('~/Desktop/number.csv')$number
  # for (i in start_at:nrow(email_df)){
  #   message(paste0(i, ' of ', nrow(email_df)))
  #   sendify(df = email_df[i,])
  #   z <- data.frame(number = i)
  #   write.csv(z, '~/Desktop/number.csv')
  #   sleeper <- abs(sample(rnorm(mean = 5, n = 1000, sd = 0), 1))
  #   Sys.sleep(sleeper)
  # }

}
