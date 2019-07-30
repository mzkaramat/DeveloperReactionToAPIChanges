setwd('/Users/zeeshan/Desktop/Sandbox/DeveloperAPIopinionMining')

library(data.table)
library(dplyr)
library(tidytext)
library(formattable)

data <- fread('data/transformed_tweets_data.csv')




new_sentiments <- sentiments %>% #From the tidytext package
  filter(lexicon != "loughran") %>% #Remove the finance lexicon
  mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                             ifelse(lexicon == "AFINN" & score < 0,
                                    "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()





#breaking sentence that is body of the post into single word messages
twitter_tidy_df <- data %>%
  unnest_tokens(word, text)


#choosing the appropriate lexicon based on the join of number of words 
temp_df <- twitter_tidy_df %>%
  mutate(words_in_lyrics = n_distinct(word)) 




twitter_tidy_df %>%
  mutate(words_in_lyrics = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_lyrics, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word))


twitter_tidy_sub <- twitter_tidy_df %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

twitter_tidy_bing <- twitter_tidy_df %>%
  inner_join(get_sentiments("bing"))
twitter_tidy_nrc <- twitter_tidy_df %>%
  inner_join(get_sentiments("nrc"))

dim(twitter_tidy_bing)


library(sentimentr)

head(data)
dim(data)


data$sentiment_score <-sentiment(data$text)$sentiment


fwrite(data,'twitter_analyzed.csv')
