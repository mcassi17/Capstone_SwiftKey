testPred <- function(sentence){
  input <- strsplit(sentence, " ")
  input <- unlist(input)
  len <- length(input)
  
  count = 0
  
  words4 <- paste(input[len-3], input[len-2], input[len-1])
  words4 <- tokenizeText(words4)
  endWord <- input[len]
  endWord <- tokenizeText(endWord)
  
  pred <- prediction3(sentence, df4, df3, df2, df1)
  
  if(endWord == pred){
    count <- count + 1
  }
  
  return(count)
  
}

count <- 0

for(i in sentences){
  correct <- testPred(i)
  count <- count + correct
}

len <- length(sentences)

print(count)
print(count/len)