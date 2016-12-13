saveRDS(two_words, "./ngram_app/two.rds")
saveRDS(three_words, "./ngram_app/three.rds")
saveRDS(four_words, "./ngram_app/four.rds")
saveRDS(five_words, "./ngram_app/five.rds")
saveRDS(six_words, "./ngram_app/six.rds")


### 1. Load Libraries
library(tm)
library(quanteda)
library(stringr)

### 2. Read Data
setwd("./Coursera_DS/Capstone_Project_NLP/")
tw<-readLines("./final/en_US/en_US.twitter.txt", 50000)
blog<-readLines("./final/en_US/en_US.blogs.txt", 50000)
news<-readLines("./final/en_US/en_US.news.txt", 50000)

test_text <- c(blog[1:10000], news[1:20000], tw[1:20000])
train_text <- c(blog[10001:50000], news[10001:50000], tw[10001:50000])
rm(blog, news, tw)

### 3. Tokenize
two_words<-token_df(train_text, 2)
three_words<-token_df(train_text, 3)
four_words<-token_df(train_text, 4)
five_words<-token_df(train_text, 5)
six_words<-token_df(train_text, 6)
seven_words<-token_df(train_text, 7)

### 4. Evaluate prediction accuracy
accuracy1<-model_eval(test_text, 1)
accuracy2<-model_eval(test_text, 2)
accuracy3<-model_eval(test_text, 3)
accuracy4<-model_eval(test_text, 4)
accuracy5<-model_eval(test_text, 5)
accuracy6<-model_eval(test_text, 6)



################### Functions ############################
### tokenize
token_df<-function(documents, n){
  token_dfm<-dfm(documents, removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, 
                 removeTwitter = TRUE, removeSeparators = TRUE, removeHyphens = TRUE, ngrams = n, 
                 concatenator=" ") 
  words_freq<-topfeatures(token_dfm, dim(token_dfm)[2])
  words_df<-data.frame(Words=names(words_freq), Frequency=words_freq, row.names = NULL)
  
  regexp<-paste("^((?:\\S+\\s+){", n-2,"}\\S+).*" ,sep="")
  words_df$Hints <- sub(regexp,"\\1", words_df$Words, perl = TRUE) # extract first n-1 words
  words_df$Prediction <- sub("^.* ([[:alnum:]]+)$","\\1",words_df$Words) # extract the last word
  
  return(words_df)
}

### clean input sentence
cleanstring<-function(string){
  corp<-VectorSource(string)
  corp<-Corpus(corp)
  corp<-tm_map(corp, tolower)
  corp<-tm_map(corp, removePunctuation)
  corp<-tm_map(corp, removeNumbers)
  #corp<-tm_map(corp, stemDocument) # remove common word endings (e.g., “ing”, “es”, “s”)
  corp<-tm_map(corp, stripWhitespace) # stripping unnecesary whitespace
  corp<-tm_map(corp, PlainTextDocument)
  return(as.character(corp[[1]]))
}

### predict (for shiny app)
ngram_model<-function(sentence, n){
  if (n>6){
    print("You can use at most 6 words to predict")
  } else{
    clean_sent<-cleanstring(sentence)
    hint<-word(clean_sent, -n, -1)
    if (n==1) {
      result <- two_words[which(two_words$Hints == hint), c("Prediction","Frequency")]
    } else if (n==2) {
      result <- three_words[which(three_words$Hints == hint), c("Prediction","Frequency")]
    } else if (n==3) {
      result <- four_words[which(four_words$Hints == hint), c("Prediction","Frequency")]
    } else if (n==4) {
      result <- five_words[which(five_words$Hints == hint), c("Prediction","Frequency")]
    } else if (n==5) {
      result <- six_words[which(six_words$Hints == hint), c("Prediction","Frequency")]
    } else if (n==6) {
      result <- seven_words[which(seven_words$Hints == hint), c("Prediction","Frequency")]
    } 
    if (dim(result)[1]==0){
      print("the")
    } else{
      print(result,row.names = FALSE) 
    }
  }
}

### Model Evaluation
model_eval<-function(text, n){
  test_tokens<-tokenize(text,removeNumbers = TRUE, removePunct = TRUE, removeSymbols = TRUE, 
                        removeSeparators = TRUE, removeTwitter = TRUE, removeHyphens = TRUE, 
                        concatenator=" ", ngrams = (n+1))
  tmp<-sapply(test_tokens,function(x){x[length(x)]}) # extract the last token
  if (n==1){
    n_gram<-two_words
  } else if (n==2) {
    n_gram<-three_words
  } else if (n==3) {
    n_gram<-four_words
  } else if (n==4) {
    n_gram<-five_words
  } else if (n==5) {
    n_gram<-six_words
  } else if (n==6) {
    n_gram<-seven_words
  } 
  ans<-tmp %in% n_gram$Words ## True, False, True , False
  accuracy<-sum(ans)/length(tmp)
  return(accuracy)
}

