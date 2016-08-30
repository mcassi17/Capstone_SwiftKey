tokenizeFile <- function(file, badWordFile) {
      words <- VCorpus(
            VectorSource(file), 
            readerControl = list(language = "en"))
      words <- tm_map(words, removePunctuation)
      words <- tm_map(words, removeNumbers)
      words <- tm_map(words, tolower)
      badWords <- readLines("/Users/mcassi17/Desktop/Coursera/capstone/sampleFiles/badwords.txt")
      words <- tm_map(words, removeWords, badWords)
      words <- tm_map(words, stripWhitespace)
      words <- tm_map(words, PlainTextDocument)
      return(words)
}