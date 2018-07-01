library(shiny)
library(dplyr)
library(purrr)
library(purrrlyr)

source("Analyse_input.R")
session_chat_count = 0
chat_db = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("user", "text", "time"))

get_random_username <- function() {
  chat_db <<- chat_db[0,]
  session_chat_count <<- 0
  paste0("User", round(runif(1, 10000, 99999)))
}

render_msg_divs <- function(collection) {
  div(class = "ui very relaxed list",
      collection %>%
        arrange(time) %>% tail(10)%>%
        by_row(~ div(class = "item",
                     a(class = "header", .$user),
                     div(class = "description", .$text)
        )) %>% {.$.out}
  )
  
}

ui <- shinyUI(fluidPage(
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
  titlePanel("Chat app (shiny.collections demo)"),
  div(textInput("username_field", "Username", width = "200px")),
  uiOutput("chatbox"),
  div(style = "display:inline-block",
      textInput("message_field", "Your message", width = "500px")),
  div(style = "display:inline-block",
      actionButton("send", "Send"))
    ))

server <- shinyServer( function(input, output, session) {
  updateTextInput(session, "username_field",value = get_random_username())
  observeEvent(input$send, {
    # new_message <- list(user = input$username_field,
    #                     text = input$message_field,
    #                     time = Sys.time())
    chat_db = rbind(chat_db,data.frame(user = input$username_field,text = input$message_field,time = Sys.time()))
    assign("chat_db",chat_db,.GlobalEnv)
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
  })
  # output$chatbox <- renderUI({
  #   if (0 == 0) {
  #     render_msg_divs(data.frame("user" = "BOT",
  #                                "text" = "Hello",
  #                                "time" = Sys.time()))
  #   } else {
  #     tags$span("Empty chat")
  #   }
  # })
  
})


shinyApp(ui = ui, server = server)
