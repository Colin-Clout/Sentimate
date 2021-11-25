library(tidyverse); library(tidytext); library(glue); library(stringr)

setwd("/Users/nate/Desktop/Genre/Sentiment Corpus")
files <- list.files("/Users/nate/Desktop/Genre/Sentiment Corpus")

# Function that takes the name of a file and returns the # of postive and negative sentiment words, the difference, & the normalized difference
GetSentiment <- function(file){
  # get the file
  fileName <- glue("/Users/nate/Desktop/Genre/Sentiment Corpus", file, sep = "")
  # get rid of any sneaky trailing spaces
  fileName <- trimws(fileName)
  
  # read in the new file
  fileText <- glue(read_file(fileName))
  # remove any dollar signs (they're special characters in R)
  fileText <- gsub("\\$", "", fileText) 
  
  # tokenize
  tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
  
  # get the sentiment from the first text: 
  sentiment <- tokens %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) %>% # # of positive words - # of negative owrds
    mutate(file = file) %>% # add the name of our file
    mutate(year = as.numeric(str_match(file, "\\d{4}"))) %>% # add the year
    mutate(president = str_match(file, "(.*?)_")[2]) # add president
  
  # return our sentiment dataframe
  return(sentiment)
}


# file to put our output in
sentiments <- data_frame()

# get the sentiments for each file in our datset
for(i in files){
  sentiments <- rbind(sentiments, GetSentiment(i))
}

SentimentDF <- as.data.frame(sentiments)

SentimentDF <- SentimentDF %>% separate(file,into=c("Source","Month"),sep = "_")
SentimentDF <- SentimentDF %>% separate(Month,into=c("Day","Month"),sep = "-")
SentimentDF$Month <- gsub("\\..*", "", SentimentDF$Month)
SentimentDF <- SentimentDF[,-7]
SentimentDF <- SentimentDF[,-7]

SentimentDF$Day <- as.numeric(as.character(SentimentDF$Day))
SentimentDF$Month <- as.numeric(as.character(SentimentDF$Month))

SentimentDF <- SentimentDF[with(SentimentDF, order(Month, Day)),]
SentimentDF$ID <- seq.int(nrow(SentimentDF))

library(xlsx)
write.xlsx(SentimentDF, file = "Sentiments.xlsx")
write.csv(SentimentDF, file = "Sentiments.csv")
##############################################

fix(SentimentDF)


qplot(ID, sentiment, data=SentimentDF) + geom_line() + geom_smooth(model=lm)
ggplot(data=SentimentDF, aes(x=ID, y=sentiment))

ggplot(SentimentDF, aes(x= ID, y = sentiment))

# plot of sentiment over time & automatically choose a method to model the change
ggplot(SentimentDF, aes(x = ID, y = sentiment)) + 
  geom_point(aes(color = sentiment))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model


# plot of sentiment over time & automatically choose a method to model the change
ggplot(sentiments, aes(x = as.numeric(year), y = sentiment)) + 
  geom_point(aes(color = president))+ # add points to our plot, color-coded by president
  geom_smooth(method = "auto") # pick a method & fit a model


ggplot(SentimentDF, aes(x = ID, y = sentiment, color = sentiment)) + 
  geom_line() + theme_grey()# draw a boxplot for each president 

