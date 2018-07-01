library(shiny)
library(DT)
source("NLP.R")

shinyApp(
  ui = fluidPage(
    selectInput("select1", "select Ticket:", choices = unique(new_issues[which(new_issues$resolved_flag == "N"),"Questions"])),
    uiOutput("checkbox"),
    textOutput("question"),
    textAreaInput("solution", "", "Solution", width = "600px", height = "300px"),
    actionButton("submit", "Submit")
  ),
  server = function(input, output) {
    # take the answer
    # call the POStag method
    # shoe the entities in checkbox
    output$checkbox <- renderUI({
      # choice <-  unique(new_issues[new_issues$Questions %in% input$select1, "Answers"])
      choice <- (tagPOS(input$select1))$entity
      checkboxGroupInput("checkbox","Select entities", choices = choice, selected = choice[1])
      
    })
    observeEvent(input$submit,{
      print(input$solution)
      new_issues[new_issues$Questions %in% input$select1, "Answers"] = input$solution
      new_issues[new_issues$Questions %in% input$select1, "resolved_flag"] = "Y"
      assign('new_issues',new_issues,envir=.GlobalEnv)
      IT_keys = rbind(IT_keys,input$checkbox)
      assign('IT_keys',IT_keys,envir=.GlobalEnv)
      # remove the Ticket that is solved
      # move it to retrain_db
      stopApp()
      })
    
    output$question <- renderText({ 
      paste("User Issue: ", input$select1)
    })
    
  }
)

