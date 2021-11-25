library(tidyverse); library(tidytext); library(glue); library(stringr); library(writexl); library(textdata)

setwd("FILE PATH GOES HERE")
files <- list.files("FILE PATH GOES HERE")

# Function that takes the name of a file and returns the # of postive and negative sentiment words, the difference, & the normalized difference
GetSentiment <- function(file){
 
  fileText <- scan(file, what = "character")
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  
  sentiment <- tokens %>%
    inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
    mutate(file = file) # add the name of our file

  return(sentiment)
}

# file to put our output in
sentiments <- data_frame()

# get the sentiments for each file in our dataset
for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i)) }

write_xlsx(sentiments, "NRC_Sentiments.xlsx")

