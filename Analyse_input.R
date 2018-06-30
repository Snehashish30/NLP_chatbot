source("NLP.R")
responses = 0
response_no = 0
val = character()
val1 = character()
main_query = character()
analyse <- function(query){
  #chat_db = rbind(chat_db,data.frame(user = "Bot",text = "Testing",time = Sys.time()))
  if(session_chat_count == 0){
    val <<- val[-0]
    val1 <<- val1[-0]
    chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Welcome to Innominds IT solution helpdesk",time = Sys.time()))
    chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Please explain your Issue: ",time = Sys.time()))
    
  }
  else if(typeof(query) == "character" && query %in% c("0","1")){
    # Provide the answers
    if(query == "1"){
      chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = val1[response_no],time = Sys.time()))
      #reward the solution
      temp_learning[,response_no + 1] = 1
      assign('temp_learning',temp_learning,envir=.GlobalEnv)
      learning = rbind(learning,temp_learning)
      assign('learning',learning,envir=.GlobalEnv)
    }
    else{
      response_no <<- response_no +1
      if(response_no > responses){
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "I can't find a solution. Issue escalated to L1. ",time = Sys.time()))
        raise_ticket(main_query)
        #chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = query ,time = Sys.time()))
      }
      else{
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = val[response_no],time = Sys.time()))
      }
      
    }
    
    
  }
  else if(typeof(query) == "character"){
    if(query == ""){
      chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "No input ",time = Sys.time()))
    }
    else{
      # we will get the query from the UI
      # call the NLP to process the query
      server_response = ai(query)
      main_query<<-query
      # val <<- append(val,ai(query))
      print(server_response$Questions)
      val <<- append(val,server_response$Questions)
      val1 <<- append(val1,server_response$Answers)
      responses <<- length(val)
      response_no <<- 1
      
      # so val is a dataframe
      # now we can interact with the user based on this response dataframe
      if(val == "0"){
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "I can't find a solution. Issue escalated to L1.",time = Sys.time()))
        raise_ticket(main_query)
      }
      else{
        if(response_no == 1){
          chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "As per your query, here are few solutions: ",time = Sys.time()))
        }
        
        # give it 1 by 1
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = val[response_no],time = Sys.time()))
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Are u looking for this solution? press 1 for Yes , else 0 ",time = Sys.time()))
        #response_no = response_no +1
        #chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = val,time = Sys.time()))
      }
    }
    
  }
  # else if(typeof(query) == "double"){
  #   # Provide the answers
  #   if(query == "1"){
  #     chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Answer for that ",time = Sys.time()))
  #   }
  #   else{
  #     response_no = response_no +1
  #     if(response_no > responses){
  #       #chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "I can't find a solution. Issue escalated to L1. ",time = Sys.time()))
  #       chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = query ,time = Sys.time()))
  #     }
  #   }
  #   
  #   
  # }
  assign("chat_db",chat_db,.GlobalEnv)
  assign("session_chat_count", session_chat_count + 1,.GlobalEnv)
  return(chat_db)
}
