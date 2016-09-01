prediction <- function(input){
  input <- strsplit(input, " ")
  input <- unlist(input)
  len <- length(input)
  newInput <- paste(input[len-2], input[len-1], input[len])
  words <- tokenizeText(newInput)
  df4 <- loadData()
  dfFilt <- df4 %>% filter(start == words) %>% order_by(occurrences)
  dfFilt
}