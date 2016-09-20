

unigramToken <- NGramTokenizer(corpus, Weka_control(min = 1, max = 1))
bigramTokenizer <- NGramTokenizer(corpus, Weka_control(min = 2, max = 2))
trigramTokenizer <- NGramTokenizer(corpus, Weka_control(min = 3, max = 3))
quadgramTokenizer <- NGramTokenizer(corpus, Weka_control(min = 4, max = 4))

df1 <- as.data.frame(table(unigramToken))
df2 <- as.data.frame(table(bigramToken))
df3 <- as.data.frame(table(trigramToken))
df4 <- as.data.frame(table(quadgramToken))