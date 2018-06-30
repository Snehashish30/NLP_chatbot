# Natural Language Processing
# Importing the dataset

dataset_original = read.csv('FAQ.csv', stringsAsFactors = FALSE)
db = read.csv('QuestionBank.csv', stringsAsFactors = FALSE)
new_issues =subset(db, FALSE)
new_issues$resolved_flag = character(0)
# Cleaning the texts
# install.packages('tm')
# install.packages('SnowballC')
library(tm)
library(SnowballC)
library(dplyr)
require("openNLP")
require("NLP")

corpus = VCorpus(VectorSource(dataset_original$Questions))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

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


IT_keys = read.csv('Keyword.csv', stringsAsFactors = FALSE)
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
# multiclass label complete

v_training_set = dataset
intent = read.csv('Verb_Keywords.csv', stringsAsFactors = FALSE)
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
# multiclass intent label complete





# take the question
# take a copy of keywords grid
# keep the matching keywords 

#query = read.delim('query.csv', quote = '', stringsAsFactors = FALSE)

ai = function(query){
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
  
  
  
  # we got the keywords
  # filter the questions db for these columns and with value 
  
  if(length(a) > 0)
  {
    recommend_set = data.frame(training_set[,a])
  }
  else{
    print("No Recommendation")
    #add_to_unresolved(query)
    return(data.frame(Questions = "0", Answers = "0",stringsAsFactors = FALSE))
  }
  a = which (colnames(v_training_set) == intersect(colnames(v_training_set),colnames(quest_set)))
  if(length(a) != 0){
    intent_set = data.frame(v_training_set[,a])
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
  c = c%>%group_by(rows)%>%summarize(cnt = length(rows))
  d = d%>%group_by(intent_rows)%>%summarize(cnt = length(intent_rows))
  c = c%>%left_join(d, by = c("rows" = "intent_rows"))
  c = apply(c, 2, function(x){replace(x, is.na(x), 0)})
  c = data.frame(c)
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

for(i in 1:nrow(new_issues)){
  entity <- tagPOS(new_issues[i,1])
}

str(entity)
entity$entity
entity$intent

# We have got the entity and intent
# Show the entity to L1 agent
# If he approves, add it to the training 
write_flag = FALSE
good = 1
if(good == 1){
  for(i in 1:length(entity$entity)){
    IT_keys = rbind(IT_keys,entity$entity[i])
    write_flag = TRUE
    #Check if the verb or one of its synonym is already present or not in the intent
    #intent = rbind(intent,entity$intent[i])
  }
}

# New entities added. On next training this keyword will help to answer the question
# Now add the question and the answer to the dB


db = rbind(db,c("Questions" = query,"Answers" = "new Answer"))

# This will form an array of possible IT keywords
# We will re check the Keywords for their authenticity of IT keyword from web
# we will add these words to our earlier keys and will re train the new questions
# We will add these as training set
#####################################################################################################
#####################################################################################################

# For persistence , I will save it to local disk
of(write_flag == TRUE){
  write.csv(IT_keys,"Keyword.csv")
  write.csv(db,"FAQ.csv")
}
