prediction <- function(input, dfquad, dftri, dfbi, dfword){
  input <- strsplit(input, " ")
  input <- unlist(input)
  len <- length(input)
  words3 <- ""
  words2 <- ""
  words1 <- ""
  words <- ""
  
  if(len >= 3){
    newInput <- paste(input[len-2], input[len-1], input[len])
    words3 <- tokenizeText(newInput)
    newInput2 <- paste(input[len-1], input[len])
    words2 <- tokenizeText(newInput2)
    words1 <- input[len]
    words1 <- tokenizeText(words1)
  } else if(len >= 2){
    newInput2 <- paste(input[len-1], input[len])
    words2 <- tokenizeText(newInput2)
    words1 <- input[len]
    words1 <- tokenizeText(words1)
  } else if(len >= 1){
    words1 <- input[len]
    words1 <- tokenizeText(words1)
  } else{
    words <- tokenizeText(input)
  }
  
  print(words3)
  print(words2)
  print(words1)
  print(words)
  
  dfquad1 <- dplyr::filter(dfquad, start == words3)
  dfquad1 <- dfquad1[with(dfquad1, order(-prob)), ]
  if(nrow(dfquad1) >= 1){
    print(dfquad1$end[1])
  }
  if(nrow(dfquad1) < 1){
    dftri1 <- dplyr::filter(dftri, start == words2)
    dftri1 <- dftri1[with(dftri1, order(-prob)), ]
    if(nrow(dftri1) >= 1){
      print(dftri1$end[1])
    }
  }
  if(nrow(dftri1) < 1){
    dfbi1 <- dplyr::filter(dfbi, start == words1)
    dfbi1 <- dfbi1[with(dfbi1, order(-prob)), ]
      if(nrow(dfbi1) >= 1) {
        print(dfbi1$end[1])
      }
  }
  if(nrow(dfbi1) < 1){
    print(dfword$unigrams[1])
  }
}