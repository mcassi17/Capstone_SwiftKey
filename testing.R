for(i in uniqueStart){
  df2.testsmall <- filter(df2.test, start == i)
  df2.testsmall <- mutate(df2.testsmall, prob = freq/sum(freq))
}