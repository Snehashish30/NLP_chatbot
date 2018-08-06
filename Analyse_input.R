#source("NLP.R")



##################################################################################################

# This part comes under manual ticket solving 
# This when the issue was escalated to L1 and L1 has given a acceptable response
# Re train on all the unresolved query
# If found any IT keyword , go for re train

possible_entity = character()
possible_intent = character()

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  noun_pos <- which(POStags %in% c("NN","NNS","NNP","NNPS"))
  verb_pos <- which(POStags %in% c("VB","VBP","VBD","VBN","VBG","VBZ"))
  possible_entity = append(possible_entity,s[a3w][noun_pos])
  possible_intent = append(possible_intent,s[a3w][verb_pos])
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
  if(length(possible_entity) == 0){
    return(0)
  }
  return(list("entity" = possible_entity,"intent" = possible_intent))
}

checkk =0;
session_chat_count = 0
responses = 0
response_no = 0
val = character()
val1 = character()
val2 = numeric()
val3 = numeric()
main_query = character()
analyse <- function(query){
  #chat_db = rbind(chat_db,data.frame(user = "Bot",text = "Testing",time = Sys.time()))
  if(session_chat_count == 0){
    val <<- val[-0]
    val1 <<- val1[-0]
    val2 <<- val2[-0]
    val3 <<- val3[-0]
    chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Welcome to Innominds IT solution helpdesk",time = Sys.time()))
    chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Please explain your Issue: ",time = Sys.time()))
    
  }
  else if(typeof(query) == "character" && query %in% c("0","1")){
    # Provide the answers
    if(query == "1"){
      print(val1)
      if(checkk == 1){
        popular_sol[val2[response_no],val3[response_no]] <<- popular_sol[val2[response_no],val3[response_no]]+1;
        #assign('popular_sol',popular_sol,envir=.GlobalEnv)
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Thank you for using IT service helpdesk. ",time = Sys.time()))
        
      }
      else{
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = val1[response_no],time = Sys.time()))
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Is this the valid solution? press 1 for Yes , else 0 ",time = Sys.time()))
        checkk <<- 1
        #print(checkk)
        #reward the solution
        temp_learning[,response_no + 1] = 1
        assign('temp_learning',temp_learning,envir=.GlobalEnv)
        learning = rbind(learning,temp_learning)
        assign('learning',learning,envir=.GlobalEnv)
      }     
      # chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = val1[response_no],time = Sys.time()))
      # chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Is this the valid solution? press 1 for Yes , else 0 ",time = Sys.time()))
      # checkk <<- 1
      # print(checkk)
      # #reward the solution
      # temp_learning[,response_no + 1] = 1
      # assign('temp_learning',temp_learning,envir=.GlobalEnv)
      # learning = rbind(learning,temp_learning)
      # assign('learning',learning,envir=.GlobalEnv)
    }
    else{
      if(checkk == 1){
        checkk<<- 0
      }
      response_no <<- response_no +1
      if(response_no > responses){
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "I can't find a solution. Issue escalated to L1. ",time = Sys.time()))
        raise_ticket(main_query)
        #chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = query ,time = Sys.time()))
      }
      else{
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = val[response_no],time = Sys.time()))
        chat_db = rbind(chat_db,data.frame(user = "Innominds Bot",text = "Are u looking for this solution? press 1 for Yes , else 0 ",time = Sys.time()))
        
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
      #View(server_response)
      #print(server_response$Question_number)
      val <<- append(val,server_response$Questions)
      val1 <<- append(val1,server_response$Answers)
      val2 <<- append(val2,server_response$Question_number)
      val3 <<- append(val3,server_response$score)
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
  print("print from analyse")
  #print(chat_db)
  return(chat_db)
}

