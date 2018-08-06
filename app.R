# install.packages('purrr')
# install.packages('purrrlyr')

library(shiny)
library(dplyr)
library(purrr)
library(purrrlyr)

my_choice <- function(){
  if(exists("new_issues") == FALSE){
    return("Empty")
  }
  else{
    ch = unique(new_issues[which(new_issues$resolved_flag == "N"),"Questions"])
    return(ch)   
  }
}

#session_chat_count = 0
source("Analyse_input.R")
if(!exists("chat_db")){
  chat_db <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("user", "text", "time"))
  #assign("chat_db",chat_db,.GlobalEnv)
  print("check chat")
  print(chat_db)
}


get_random_username <- function() {
  #chat_db <<- chat_db[0,]
  #chat_db = chat_db[0]
  #assign("chat_db",chat_db,.GlobalEnv)
  paste0("User", round(runif(1, 10000, 99999)))
}

render_msg_divs <- function(collection) {
  div(class = "ui very relaxed list",
      collection %>%
        by_row(~ div(class = "item",
                     a(class = "header", .$user),
                     div(class = "description", .$text)
        )) %>% {.$.out}
  )
  
}



ui <- shinyUI(fluidPage(tabsetPanel(type = "tabs",
                                    
                                    tabPanel("Application",
                                             tags$head(
                                               tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                                               tags$script(src = "script.js"),
                                               tags$style(HTML("chatbox {
                                                               padding: .5em;
                                                               border: 1px solid #777;
                                                               height: 300px;
                                                               overflow-y: scroll;
                                                               }"))
        ),
        titlePanel("Chat app"),
        div(style = "display:inline-block",
            actionButton("new_issue", "New Issue")),
        div(textInput("username_field", "Username", width = "200px")),
        uiOutput("chatbox"),
        div(style = "display:inline-block",
            textInput("message_field", "Your message", width = "500px")),
        div(style = "display:inline-block",
            actionButton("send", "Send"))
                                               ),
        tabPanel("MAIN_UI",
                 actionButton("train", "Train Model"),
                 actionButton("retrain", "Re-Train Model"),
                 actionButton("reinforcement", "Re-inforcement Learning")
        ),
        tabPanel("L1_support",
                 #selectInput("select1", "select Ticket:", choices = unique(new_issues[which(new_issues$resolved_flag == "N"),"Questions"])),
                 actionButton("refresh", "Refresh"),
                 selectInput("select1", "select Ticket:", choices = my_choice()),
                 uiOutput("checkbox"),
                 textOutput("question"),
                 textAreaInput("solution", "", "Solution", width = "600px", height = "300px"),
                 actionButton("submit", "Submit")
                 
                 
        )
                                               )
                                    ))

server <- shinyServer( function(input, output, session) {
  
  output$checkbox <- renderUI({
    # choice <-  unique(new_issues[new_issues$Questions %in% input$select1, "Answers"])
    choice <- (tagPOS(input$select1))$entity
    checkboxGroupInput("checkbox","Select entities", choices = choice, selected = choice[1])
    
  })
  #updateSelectInput(session, "select1",choices = unique(new_issues[which(new_issues$resolved_flag == "N"),"Questions"]))
  observeEvent(input$refresh,{updateSelectInput(session, "select1",choices = my_choice())})
  #updateSelectInput(session, "select1",choices = my_choice())
  observeEvent(input$submit,{
    print(input$solution)
    new_issues[new_issues$Questions %in% input$select1, "Answers"] = input$solution
    new_issues[new_issues$Questions %in% input$select1, "resolved_flag"] = "Y"
    assign('new_issues',new_issues,envir=.GlobalEnv)
    IT_keys = rbind(IT_keys,input$checkbox)
    assign('IT_keys',IT_keys,envir=.GlobalEnv)
    # remove the Ticket that is solved
    # move it to retrain_db
    # stopApp()
    updateSelectInput(session, "select1",choices = my_choice())
  })
  output$question <- renderText({
    paste("User Issue: ", input$select1)
  })
  observeEvent(input$train,{
    source("NLP.R")
    create_model(0)
  })
  observeEvent(input$retrain,{
    create_model(1)
  })
  
  observeEvent(input$reinforcement,{
    reinforcement_training()
  })
  updateTextInput(session, "username_field",value = get_random_username())
  observeEvent(input$new_issue, {
    chat_db = chat_db[0]
    assign("chat_db",chat_db,.GlobalEnv)
    assign("session_chat_count", 0,.GlobalEnv)
    checkk <<-0
    output$chatbox <- renderUI({
      if (0 == 0) {
        render_msg_divs(data.frame(chat_db))
      } else {
        tags$span("Empty chat")
      }
      Sys.sleep(1)
      # call the NLP
      # again render
    })
  })
  observeEvent(input$send, {
    # new_message <- list(user = input$username_field,
    #                     text = input$message_field,
    #                     time = Sys.time())
    print(chat_db)
    chat_db = rbind(chat_db,data.frame(user = input$username_field,text = input$message_field,time = Sys.time()))
    assign("chat_db",chat_db,.GlobalEnv)
    print(chat_db)
    #shiny.collections::insert(chat, new_message)
    # updateTextInput(session, "message_field", value = "Hi There")
    #call the function
    output$chatbox <- renderUI({
      if (0 == 0) {
        render_msg_divs(data.frame(chat_db))
      } else {
        tags$span("Empty chat")
      }
      Sys.sleep(20)
      # call the NLP
      # again render
    })
    updateTextInput(session, "message_field", value = "")
    chat_db = analyse(input$message_field)
    Sys.sleep(1)
    output$chatbox <- renderUI({
      render_msg_divs(data.frame(chat_db))
    })
    #print("checking ................................. chat")
    #print(chat_db)
  }) 
})
shinyApp(ui = ui, server = server)



