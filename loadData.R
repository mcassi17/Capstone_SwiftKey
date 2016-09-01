loadData <- function(){
  library(stringi)
  library(tm)
  library(ngram)
  library(ggplot2)
  library(wordcloud)
  library(NLP)
  library(openNLP)
  library(qdap)
  library(RWeka)
  library(slam)
  library(knitr)
  library(tidyr)
  twitter <- readLines('en_US.twitter.txt')
  blogs <- readLines('en_US.blogs.txt')
  news <- readLines('en_US.news.txt')
  set.seed(2112)
  smallTwitter <- sample(twitter, length(twitter)*.0025)
  smallBlogs <- sample(blogs, length(blogs)*.0025)
  smallNews <- sample(news, length(news)*.0025)
  remove(twitter)
  remove(blogs)
  remove(news)
  smallCombo <- paste(smallBlogs, smallTwitter, smallNews, collapse = "")
  badWords <- readLines("./badwords.txt")
  Encoding(smallCombo) <- "latin1"
  smallCombo <- iconv(smallCombo, "latin1", "ASCII", sub="")
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  sent_token_annotator
  smallCombo <- as.String(smallCombo)
  a1 <- NLP::annotate(smallCombo, sent_token_annotator)
  smallCombo <- smallCombo[a1]
  tokenCombo <- tokenizeFile(smallCombo, badWords)
  tdmComboQuadgram <- TermDocumentMatrix(Corpus(VectorSource(tokenCombo)), 
                                         control = list(tokenize = QuadgramTokenizer))
  freqComboQuadgram <- row_sums(tdmComboQuadgram)
  df4 <- data.frame(term=names(freqComboQuadgram),occurrences=freqComboQuadgram, 
                    row.names = NULL)
  df4$term <- as.character(df4$term)
  df4$end <- df4$term
  df4 <- df4 %>% separate(col = end, into = c('first', 'second', 'third', 'end'), sep = "\\s")
  df4 <- df4 %>% unite(start, first, second, third, sep = " ")
  return(df4)
}