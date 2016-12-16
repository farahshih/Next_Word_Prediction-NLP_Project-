
two_words<-readRDS("two.rds")
three_words<-readRDS("three.rds")
four_words<-readRDS("four.rds")
five_words<-readRDS("five.rds")
#six_words<-readRDS("six.rds")

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
  if (length(strsplit(sentence,' ')[[1]])<4){
    print("Please enter a sentence with at least four words")
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
    }  
    if (dim(result)[1]==0){
      print("the")
    } else{
      print(result,row.names = FALSE) 
    }
  }
}
