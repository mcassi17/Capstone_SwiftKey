tokenizeFile <- function(file, badWords) {
      words <- VCorpus(
            VectorSource(file), 
            readerControl = list(language = "en"))
      words <- tm_map(words, removePunctuation)
      words <- tm_map(words, removeNumbers)
      words <- tm_map(words, content_transformer(tolower))
      words <- tm_map(words, removeWords, badWords)
      words <- tm_map(words, stripWhitespace)
      words <- tm_map(words, PlainTextDocument)
      return(words)
}

tokenizeText <- function(input){
  tokenized <- tokenizeFile(input, badWords)
  backString <- as.character(tokenized[[1]])
  return(backString)
}