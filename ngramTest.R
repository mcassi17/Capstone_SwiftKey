library(stringi)
library(tm)
library(slam)
library(stylo)
library(tidyr)
library(dplyr)
twitter <- readLines('en_US.twitter.txt')
blogs <- readLines('en_US.blogs.txt')
news <- readLines('en_US.news.txt')
smallTwitter <- sample(twitter, length(twitter)*.25)
smallBlogs <- sample(blogs, length(blogs)*.25)
smallNews <- sample(news, length(news)*.25)
remove(twitter)
remove(blogs)
remove(news)
smallCombo <- c(smallBlogs, smallTwitter, smallNews)
remove(smallTwitter)
remove(smallBlogs)
remove(smallNews)
badWords <- readLines("./badwords.txt")
badWords <- paste("\\b",badWords,"\\b", sep="")
Encoding(smallCombo) <- "latin1"
smallCombo <- iconv(smallCombo, "latin1", "ASCII", sub="")

indSent <- unlist(strsplit(smallCombo, "(?<!Mr|Mrs|Dr|Ms|St)[?!.]", perl = TRUE))

hasProfanity <- rep(FALSE,length(indSent)) 
for (s in 1:length(indSent)) {
  for (p in 1:length(badWords)) {
    if (grepl(badWords[p],indSent[s])) {
      hasProfanity[s] <- TRUE
      break
    }
  }
}

indSent <- indSent[!hasProfanity]
indSent <- indSent[nchar(indSent)>0]

words <- VCorpus(VectorSource(indSent))
words <- tm_map(words, content_transformer(tolower))
words <- tm_map(words, removePunctuation)
words <- tm_map(words, removeNumbers)
words <- tm_map(words, stripWhitespace)
remove(smallCombo)
remove(badWords)

wordContent <- character(length(words$content))
for(i in 1:length(words)) wordContent[[i]] <- words[[i]]$content
corpus <- txt.to.words(wordContent)


numExamples <- length(corpus)
numTrain <- round(0.9 * numExamples)
numTest <- numExamples - numTrain
s <- sample(numExamples)

indicesTrain <- s[1:numTrain]
trainCorpus <- corpus[indicesTrain]

indicesTest <- s[(numTrain+1):numExamples]
testCorpus <- corpus[indicesTest]

unigrams<-make.ngrams(trainCorpus, ngram.size = 1)
df1 <- data.frame(table(unigrams))
remove(unigrams)
df1 <- df1[order(df1$Freq, decreasing = TRUE),]
df1 <- df1 %>% mutate(prob = Freq/sum(Freq))
saveRDS(df1, "unigrams.RDS")
remove(df1)

bigrams<-make.ngrams(trainCorpus, ngram.size = 2)
df2 <- data.frame(table(bigrams))
df2 <- df2[order(df2$Freq,decreasing = TRUE),]
remove(bigrams)
df2$bigrams <- as.character(df2$bigrams)
df2$end <- df2$bigrams
df2 <- df2 %>% separate(col = end, into = c('start', 'end'), sep = "\\s")
df2 <- df2 %>% group_by(start) %>% mutate(total = sum(Freq))
df2 <- df2 %>% mutate(prob = Freq/total)
saveRDS(df2, "bigrams.RDS")
remove(df2)

trigrams<-make.ngrams(trainCorpus, ngram.size = 3)
df3 <- data.frame(table(trigrams))
remove(trigrams)
df3 <- df3[order(df3$Freq, decreasing = TRUE),]
df3$trigrams <- as.character(df3$trigrams)
df3$end <- df3$trigrams
df3 <- df3 %>% separate(col = end, into = c('first', 'second', 'third'), sep = "\\s")
df3 <- df3 %>% unite(start, first, second, sep = " ")
df3 <- df3 %>% group_by(start) %>% mutate(total = sum(freq))
df3 <- df3 %>% mutate(prob = freq/total)
saveRDS(df3, "trigrams.RDS")
remove(df3)

quadgrams<-make.ngrams(trainCorpus, ngram.size = 4)
df4 <- data.frame(table(quadgrams))
remove(quadgrams)
df4 <- df4[order(df4$Freq, decreasing = TRUE),]
df4$quadgrams <- as.character(df4$quadgrams)
df4$end <- df4$quadgrams
df4 <- df4 %>% separate(col = end, into = c('first', 'second', 'third', 'end'), sep = "\\s")
df4 <- df4 %>% unite(start, first, second, third, sep = " ")
df4 <- df4 %>% group_by(start) %>% mutate(total = sum(freq))
df4 <- df4 %>% mutate(prob = freq/total)
saveRDS(df4, "quadgrams.RDS")
remove(df4)

fivegrams<-make.ngrams(trainCorpus, ngram.size = 5)
df5 <- data.frame(table(fivegrams))
remove(fivegrams)
df5 <- df5[order(df5$Freq, decreasing = TRUE),]
df5$fivegrams <- as.character(df5$fivegrams)
df5$end <- df5$fivegrams
df5 <- df5 %>% separate(col = end, into = c('first', 'second', 'third', 'fourth', 'end'), sep = "\\s")
df5 <- df5 %>% unite(start, first, second, third, fourth, sep = " ")
saveRDS(df5, "fivegrams.RDS")
remove(df5)

