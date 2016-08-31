tokenizeFile <- function(file, badWords) {
      words <- VCorpus(
            VectorSource(file), 
            readerControl = list(language = "en"))
      words <- tm_map(words, removePunctuation)
      words <- tm_map(words, removeNumbers)
      words <- tm_map(words, tolower)
      words <- tm_map(words, removeWords, badWords)
      words <- tm_map(words, stripWhitespace)
      words <- tm_map(words, PlainTextDocument)
      return(words)
}