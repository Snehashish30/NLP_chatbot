# Natural Language Processing
# Importing the dataset


dataset_original = read.csv('FAQ.csv', stringsAsFactors = FALSE)
db = read.csv('QuestionBank.csv', stringsAsFactors = FALSE)
learning = setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Entity", "Sol1", "Sol2","Sol3","Sol4","Sol5"))
temp_learning = setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Entity", "Sol1", "Sol2","Sol3","Sol4","Sol5"))
learningRank = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("Solutions", "Rank", "Entity"))
training_set = dataset_original
v_training_set = training_set
intent = read.csv('Verb_Keywords.csv', stringsAsFactors = FALSE)
IT_keys = read.csv('Keyword.csv', stringsAsFactors = FALSE)
#learningRank must be reset during re-inforcement training

new_issues =subset(db, FALSE)
new_issues$resolved_flag = character(0)
# Cleaning the texts
# install.packages('tm')
# install.packages('SnowballC')
# install.packages('openNLP')
# install.packages('NLP')
# install.packages('dplyr')
library(tm)
library(SnowballC)
library(dplyr)
require("openNLP")
require("NLP")
library(hunspell)
library(stringi)
library(stringr)

create_model <- function(x){
  if(x == 1){
    print("Re-training")
    #move the new_issues to db
    #remove those entries from new_issues
    db = rbind(db,new_issues[which(new_issues$resolved_flag == "Y"),c(1,2)])
    new_issues = new_issues[-(which(new_issues$resolved_flag == "Y")),]
    assign('new_issues',new_issues,envir=.GlobalEnv)
    assign('db',db,envir=.GlobalEnv)
    dataset_original = db
    assign('dataset_original',dataset_original,envir=.GlobalEnv)
  }
  else{
    print("First Training")
    dataset_original = read.csv('QuestionBank.csv', stringsAsFactors = FALSE)
    assign('dataset_original',dataset_original,envir=.GlobalEnv)
    IT_keys = read.csv('Keyword.csv', stringsAsFactors = FALSE)
    intent = read.csv('Verb_Keywords.csv', stringsAsFactors = FALSE) 
  }
  dataset_original$Questions <- iconv(enc2utf8(dataset_original$Questions),sub="byte")
  corpus = VCorpus(VectorSource(dataset_original$Questions))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords())
  corpus = tm_map(corpus, stemDocument)
  corpus = tm_map(corpus, stripWhitespace)
  print("running")
  # Creating the Bag of Words model
  
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, 0.999)
  dataset = as.data.frame(as.matrix(dtm))
  #dataset = dataset[,-which(colnames(dataset)== "comput")]
  training_set = dataset
  
  ##################################################################################################################################
  ##################################################################################################################################
  
  # we will use the IT keyword first to get the broad category and then will use the verb keyword to get the closest match question
  # IT keyword is entity
  # verb keyword is intent
  
  
  #IT_keys = read.csv('Keyword.csv', stringsAsFactors = FALSE)
  assign('IT_keys',IT_keys,envir=.GlobalEnv)
  keys = IT_keys
  corpus = VCorpus(VectorSource(keys$Keywords))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords())
  corpus = tm_map(corpus, stemDocument)
  corpus = tm_map(corpus, stripWhitespace)
  
  # Creating the Bag of Words model
  
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, 0.999)
  keys = as.data.frame(as.matrix(dtm))
  
  #creating an empty numeric vector to hold the matching col number
  a <- numeric()
  
  for(i in 1:ncol(keys)){
    col_number = which(colnames(training_set) == colnames(keys)[i])
    #print(col_number)
    if(!(identical(col_number, integer(0)))){
      a = append(a,col_number)
      #print(col_number)
    }
  }
  
  # now we can multiclass label the db questions
  training_set = training_set[,a]
  assign('training_set',training_set,envir=.GlobalEnv)
  # multiclass label complete
  
  v_training_set = dataset
  # intent = read.csv('Verb_Keywords.csv', stringsAsFactors = FALSE)
  assign('intent',intent,envir=.GlobalEnv)
  v_keys = intent
  corpus = VCorpus(VectorSource(v_keys$Keywords))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords())
  corpus = tm_map(corpus, stemDocument)
  corpus = tm_map(corpus, stripWhitespace)
  
  # Creating the Bag of Words model
  
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, 0.999)
  v_keys = as.data.frame(as.matrix(dtm))
  
  #creating an empty numeric vector to hold the matching col number
  a <- numeric()
  for(i in 1:ncol(v_keys)){
    col_number = which(colnames(v_training_set) == colnames(v_keys)[i])
    #print(col_number)
    if(!(identical(col_number, integer(0)))){
      a = append(a,col_number)
      #print(col_number)
    }
  }
  
  # now we can multiclass label the db questions
  
  v_training_set = v_training_set[,a]
  assign('v_training_set',v_training_set,envir=.GlobalEnv)
  # multiclass intent label complete
  
  
  
} 


# 





# take the question
# take a copy of keywords grid
# keep the matching keywords 

#query = read.delim('query.csv', quote = '', stringsAsFactors = FALSE)

ai = function(query){
  # do spellcheck here
  query = toString(query)
  num_words = sapply(strsplit(query, " "), length)
  for(i in 1 :num_words){
    if(hunspell_check(word(query, i)) == FALSE){
      tmp = hunspell_suggest(word(query, i))[[1]]
      if(length(tmp)>0){
        tmp = tmp[[1]]
        tmp = tmp[1]
        query = str_replace(query,word(query, i),tmp)
      }
      
    }
    
  }
  corpus = VCorpus(VectorSource(query))
  corpus = tm_map(corpus, content_transformer(tolower))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords())
  corpus = tm_map(corpus, stemDocument)
  corpus = tm_map(corpus, stripWhitespace)
  
  dtm = DocumentTermMatrix(corpus)
  dtm = removeSparseTerms(dtm, 0.999)
  quest_set = as.data.frame(as.matrix(dtm))
  
  
  a = which (colnames(training_set) %in% intersect(colnames(training_set),colnames(quest_set)))
  print("print check")
  print(a)
  # we have the entity ids here 
  # put these entities to learning dataframe for interactive learning
  
  
  # we got the keywords
  # filter the questions db for these columns and with value 
  
  if(length(a) > 0)
  {
    recommend_set = data.frame(training_set[,a])
    for(n in 1:length(a)){
      temp_learning <<- temp_learning[0,]
      temp_learning = rbind(temp_learning,data.frame(Entity = a[n],Sol1 = 0,Sol2 = 0,Sol3 = 0,Sol4 = 0, Sol5= 0))
    }
    assign("temp_learning",temp_learning,.GlobalEnv)
  }
  else{
    print("No Recommendation")
    #add_to_unresolved(query)
    return(data.frame(Questions = "0", Answers = "0",stringsAsFactors = FALSE))
  }
  a = which (colnames(v_training_set) %in% intersect(colnames(v_training_set),colnames(quest_set)))
  intent_set = character()
  if(length(a) != 0){
    intent_set = data.frame(v_training_set[,a])
  }
  else{
    intent_set = data.frame(intent_set)
  }
  c = numeric()
  d = numeric()
  for(i in 1:ncol(recommend_set)){
    row_number = which(recommend_set[,i] == 1)
    if(!(identical(row_number, integer(0)))){
      c = append(c,row_number)
      if(ncol(intent_set)>0){
        for(j in 1:length(row_number)){
          if(length(which(intent_set[row_number[j],] == 1))>0){
            d = append(d,rep(row_number[j],length(which(intent_set[row_number[j],] == 1))))
          }
        }
      }
    }
  }
  
  c = as.data.frame(c)
  colnames(c) = "rows"
  d = as.data.frame(d)
  colnames(d) = "intent_rows"
  test <<- c
  test1 <<- d
  c = c%>%group_by(rows)%>%summarize(cnt = length(rows))
  d = d%>%group_by(intent_rows)%>%summarize(cnt = length(intent_rows))
  c = c%>%left_join(d, by = c("rows" = "intent_rows"))
  #c = apply(c, 2, function(x){replace(x, is.na(x), 0)})
  c = data.frame(c)
  c[is.na(c$cnt.y),"cnt.y"] = 0
  c <- c%>%arrange(desc(cnt.x,cnt.y))
  recommend = unique(c$rows)
  print(db[recommend,1])
  #return("1")
  return(db[recommend,])
  
}

##################################################################################################
# Run and check for the problems that are already avaialble
#query = "Unlock USB port"
# query = "I can't receive any email attachments"
# status = ai(query)
raise_ticket <- function(query){
  print("Raising L1 Ticket")
  new_quest = data.frame(Questions = query,Answers = "",resolved_flag = "N",stringsAsFactors = FALSE)
  new_issues = rbind(new_issues,new_quest)
  assign("new_issues",new_issues,.GlobalEnv)
  print("Issue Escalated")
}
# if(status == 0){
#   new_quest = data.frame(query,"")
#   names(new_quest) = c("Questions","Answers")
#   new_issues = rbind(new_issues,new_quest)
#   print("Issue Escalated")
# }

##################################################################################################


# IT keywords matching complete
# Now we can try to match the verbs that goes along with some sentences




# ##################################################################################################
# # This part comes under manual ticket solving 
# 
# # This when the issue was escalated to L1 and L1 has given a acceptable response
# # Re train on all the unresolved query
# # If found any IT keyword , go for re train
# 
# possible_entity = character()
# possible_intent = character()
# tagPOS <-  function(x, ...) {
#   s <- as.String(x)
#   word_token_annotator <- Maxent_Word_Token_Annotator()
#   a2 <- Annotation(1L, "sentence", 1L, nchar(s))
#   a2 <- annotate(s, word_token_annotator, a2)
#   a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
#   a3w <- a3[a3$type == "word"]
#   POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
#   noun_pos <- which(POStags %in% c("NN","NNS","NNP","NNPS"))
#   verb_pos <- which(POStags %in% c("VB","VBP","VBD","VBN","VBG","VBZ"))
#   possible_entity = append(possible_entity,s[a3w][noun_pos])
#   possible_intent = append(possible_intent,s[a3w][verb_pos])
#   POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
#   list(POStagged = POStagged, POStags = POStags)
#   if(length(possible_entity) == 0){
#     return(0)
#   }
#   return(list("entity" = possible_entity,"intent" = possible_intent))
# }
# 



#####################################################################################################
#####################################################################################################
# Interactive/Re inforcement Learning

#####################################################################################################

#learning = setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Entity", "Sol1", "Sol2","Sol3","Sol4","Sol5"))

# Implementing UCN
reinforcement_training <- function(){
  learningRank = learningRank[-0]
  values = unique(learning$Entity)
  for(entities in 1:length(unique(learning$Entity))){
    Rank_sol = Rank_sol[-0]
    UCB_dataset = learning%>%filter(Entity == values[entities])
    N = nrow(UCB_dataset)
    #N = length(which(learning$Entity == values[entities]))
    d = 5
    sol_selected = integer(0)
    numbers_of_selections = integer(d)
    sums_of_rewards = integer(d)
    total_reward = 0
    for (n in 1:N) {
      sol = 0
      max_upper_bound = 0
      for (i in 1:d) {
        if (numbers_of_selections[i] > 0) {
          average_reward = sums_of_rewards[i] / numbers_of_selections[i]
          delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
          upper_bound = average_reward + delta_i
        } else {
          upper_bound = 1e400
        }
        if (upper_bound > max_upper_bound) {
          max_upper_bound = upper_bound
          sol = i
        }
      }
      sol_selected = append(sol_selected, sol)
      numbers_of_selections[sol] = numbers_of_selections[sol] + 1
      reward = UCB_dataset[n, sol +1]
      sums_of_rewards[sol] = sums_of_rewards[sol] + reward
      total_reward = total_reward + reward
    }
    Rank_sol <- (data.frame(Solutions = sol_selected))%>%group_by(Solutions)%>%summarize(Rank = length(Solutions))
    Rank_sol <- Rank_sol%>%arrange(desc(Rank))
    Rank_sol$Entity <- rep(values[entities],nrow(Rank_sol))
    learningRank = rbind(learningRank,Rank_sol)
    assign("learningRank",learningRank,.GlobalEnv)
  }
  
  
}

