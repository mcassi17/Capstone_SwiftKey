prediction3 <- function(input, dfquad, dftri, dfbi, dfword){
  input <- strsplit(input, " ")
  input <- unlist(input)
  len <- length(input)
  words3 <- ""
  words2 <- ""
  words1 <- ""
  words <- ""
  
  if(len >= 3){
    newInput <- paste(input[len-3], input[len-2], input[len-1])
    words3 <- tokenizeText(newInput)
    newInput2 <- paste(input[len-2], input[len-1])
    words2 <- tokenizeText(newInput2)
    words1 <- input[len-1]
    words1 <- tokenizeText(words1)
  } else if(len >= 2){
    newInput2 <- paste(input[len-2], input[len-1])
    words2 <- tokenizeText(newInput2)
    words1 <- input[len-1]
    words1 <- tokenizeText(words1)
  } else if(len >= 1){
    words1 <- input[len-1]
    words1 <- tokenizeText(words1)
  } else{
    words <- tokenizeText(input)
  }
  
  dfquad1 <- dplyr::filter(dfquad, start == words3)
  dfquad1 <- dfquad1[with(dfquad1, order(-prob)), ]
  
  dftri1 <- dplyr::filter(dftri, start == words2)
  dftri1$prob <- 0.4*dftri1$prob
  dftri1 <- dftri1[with(dftri1, order(-prob)), ]

  dfbi1 <- dplyr::filter(dfbi, start == words1)
  dfbi1$prob <- 0.4*0.4*dfbi1$prob
  dfbi1 <- dfbi1[with(dfbi1, order(-prob)), ]

  results <- rbind(dfquad1, dftri1, dfbi1)
  results <- results %>% select(end, prob)
  results <- results[with(results, order(-prob)), ]
  results <- results[!duplicated(results$end),]
  return(results[1,])
}