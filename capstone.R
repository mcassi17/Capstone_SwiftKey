library(tm)
library(ngram)
library(ggplot2)
library(wordcloud)
library(NLP)
library(RWeka)

twitter <- readLines('en_US.twitter.txt')
blogs <- readLines('en_US.blogs.txt')
news <- readLines('en_US.news.txt')

length(twitter)
length(blogs)
length(news)

set.seed(1936)
smallTwitter <- sample(twitter, length(twitter)*.02)
smallBlogs <- sample(blogs, length(blogs)*.02)
smallNews <- sample(news, length(news)*.02)

remove(twitter)
remove(blogs)
remove(news)

head(smallTwitter)
head(smallBlogs, n=3)
head(smallNews, n=2)

smallCombo <- paste(smallBlogs, smallTwitter, smallNews)
head(smallCombo)

## https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/blob/master/en
badWords <- readLines("./badwords.txt")
length(badWords)
class(badWords)

Encoding(smallCombo) <- "latin1"
smallCombo <- iconv(smallCombo, "latin1", "ASCII", sub="")

tokenCombo <- tokenizeFile(smallCombo, badWords)

dtmCombo <- DocumentTermMatrix(tokenCombo)

freqCombo <- colSums(as.matrix(dtmCombo))

ordCombo <- order(freqCombo,decreasing=TRUE)
freqCombo[head(ordCombo)]
freqCombo[tail(ordCombo)]

findFreqTerms(dtmCombo,lowfreq=1000)

comboDF <- data.frame(term=names(freqCombo),occurrences=freqCombo) +
p <- ggplot(subset(comboDF, freqCombo>2500), aes(term, occurrences)) + 
      geom_bar(aes(fill = "blue"), stat="identity") +
      theme(axis.text.x=element_text(angle=45, hjust=1)) +
      ggtitle("Most Frequent Words")
p
wordcloud(names(freqCombo),freqCombo,min.freq=1000,
          colors=brewer.pal(3,"Set1"))

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))

tdmComboBigram <- TermDocumentMatrix(Corpus(VectorSource(tokenCombo)), 
                          control = list(tokenize = BigramTokenizer))

tdmComboTrigram <- TermDocumentMatrix(Corpus(VectorSource(tokenCombo)), 
                               control = list(tokenize = TrigramTokenizer))