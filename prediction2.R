prediction2 <- function(input, dfquad, dfword){
  input <- strsplit(input, " ")
  input <- unlist(input)
  len <- length(input)
  words <- ""
  
  if(len >= 3){
    newInput <- paste(input[len-2], input[len-1], input[len])
    words <- tokenizeText(newInput)
    dfquad1 <- dplyr::filter(dfquad, start == words)
    dfquad1 <- dfquad1[with(dfquad1, order(-freq)), ]
    if(nrow(dfquad1) >= 1){
      print(dfquad1$end[1])
    }
    
    if(nrow(dfquad1) < 1){
      print(dfword$unigrams[1])
    }
  } else{
    print("Input not long enough")
  }

}