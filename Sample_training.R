train_data = read.delim('chatbot_train.txt', quote = '', stringsAsFactors = FALSE)
source("NLP.R")
new_entities = character()
new_intents = character()
for(i in 1:nrow(train_data)){
  #we just want all the entities from these training data
  new_values = tagPOS(train_data$Questions)
  new_entities = append(new_entities,new_values$entity)
  new_intents = append(new_intents,new_values$intent)
}
# ran for 45 sec
# We will do manual inspection and then we will add all the new entities to IT_KEYS and 
# will save it for persistence
# we will add the questions and answers to DB and save it for persistence

# entities
new_entities = unique(new_entities)
new_entities = data.frame(Keywords = new_entities,stringsAsFactors = FALSE)

# all 77 entries looks fine
# adding it to IT_KEYS
IT_keys = rbind(IT_keys,new_entities)
assign('IT_keys',IT_keys,envir=.GlobalEnv)
write.csv(IT_keys,"Keyword.csv",row.names = FALSE)

#intents
new_intents = unique(new_intents)
new_intents = data.frame(Keywords = new_intents,stringsAsFactors = FALSE)
# all 30 entries looks fine
# adding it to intent
intent = rbind(intent,new_intents)
assign('intent',intent,envir=.GlobalEnv)
write.csv(intent,"Verb_Keywords.csv",row.names = FALSE)


# adding the questions and solutions to Question Bank 
db = rbind(db,train_data)
assign('db',db,envir=.GlobalEnv)
write.csv(db,"QuestionBank.csv",row.names = FALSE)

