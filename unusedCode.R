Encoding(smallTwitter) <- "latin1"
smallTwitter <- iconv(smallTwitter, "latin1", "ASCII", sub="")
Encoding(smallBlogs) <- "latin1"
smallBlogs <- iconv(smallBlogs, "latin1", "ASCII", sub="")
Encoding(smallNews) <- "latin1"
smallTwitter <- iconv(smallNews, "latin1", "ASCII", sub="")

tokenTwitter <- tokenizeFile(smallTwitter, badWords)
tokenBlogs <- tokenizeFile(smallBlogs, badWords)
tokenNews <- tokenizeFile(smallNews, badWords)

dtmTwitter <- DocumentTermMatrix(tokenTwitter)
dtmBlogs <- DocumentTermMatrix(tokenBlogs)
dtmNews <- DocumentTermMatrix(tokenNews)

freqTwitter <- colSums(as.matrix(dtmTwitter))
freqBlogs <- colSums(as.matrix(dtmBlogs))
freqNews <- colSums(as.matrix(dtmNews))

ordTwitter <- order(freqTwitter,decreasing=TRUE)
freqTwitter[head(ordTwitter)]
freqTwitter[tail(ordTwitter)]

ordBlogs <- order(freqBlogs,decreasing=TRUE)
freqBlogs[head(ordBlogs)]
freqBlogs[tail(ordBlogs)]

ordNews <- order(freqNews,decreasing=TRUE)
freqNews[head(ordNews)]
freqNews[tail(ordNews)]

findFreqTerms(dtmTwitter,lowfreq=80)
findFreqTerms(dtmBlogs,lowfreq=80)
findFreqTerms(dtmNews,lowfreq=80)

