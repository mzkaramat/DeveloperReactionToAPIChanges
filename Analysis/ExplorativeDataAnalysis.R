#packages installation if necessary
if(F){
  install.packages("plyr")
  install.packages("ggplot2")
  install.packages("wordcloud")
  install.packages("RColorBrewer")
  install.packages("tm")
  install.packages("SnowballC")
  library(devtools)
  install_github("ndphillips/yarrr")
  install.packages('widyr')
  install.packages('ggrepel')
  install.packages('gridExtra')
  install.packages('knitr')
  install.packages('kableExtra')
  install.packages('formattable')
  install.packages('circlize')
  install.packages('memery')
  install.packages('magick')
  install.packages('radarchart')
  install.packages('igraph')
  install.packages('ggraph')
}

#Cleaning the working directory
rm(list = ls())

#setting the working directory
setwd("D:\\DeveloperAPIopinionMining")

#loading libraries 
library(tm)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(stringr)
library(dplyr)
library("yarrr")
library(tidytext)




#loading libraries by following different tutorial
library(dplyr) #Data manipulation (also included in the tidyverse package)
library(tidytext) #Text mining
library(tidyr) #Spread, separate, unite, text mining (also included in the tidyverse package)
library(widyr) #Use for pairwise correlation

#Visualizations!
library(ggplot2) #Visualizations (also included in the tidyverse package)
library(ggrepel) #`geom_label_repel`
library(gridExtra) #`grid.arrange()` for multi-graphs
library(knitr) #Create nicely formatted output tables
library(kableExtra) #Create nicely formatted output tables
library(formattable) #For the color_tile function
library(circlize) #Visualizations - chord diagram
library(memery) #Memes - images with plots
library(magick) #Memes - images with plots (image_read)
library(yarrr)  #Pirate plot
library(radarchart) #Visualizations
library(igraph) #ngram network diagrams
library(ggraph) #ngram network diagrams
library(lubridate)

#Define some colors to use throughout
my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00", "#D65E00")

#Customize ggplot2's default theme settings
#This tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(plot.title = element_text(hjust = 0.5), #Center the title
        axis.ticks = aticks, #Set axis ticks to on or off
        panel.grid.minor = pgminor, #Turn the minor grid lines on or off
        legend.title = lt, #Turn the legend title on or off
        legend.position = lp) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(bootstrap_options = c("striped", "condensed", "bordered"),
                  full_width = FALSE)
}


#ingesting the twitter data
twitter_df <- read.csv('data/dataset.csv')


#initial exploration of the data
head(twitter_df)
dim(twitter_df)
colnames(twitter_df)
summary(twitter_df)
lapply(twitter_df,typeof)

#data transformation
twitter_df$Title <- as.character(twitter_df$Title)
twitter_df$Body <- as.character(twitter_df$Body)
#merging the body into the title as this information will be useful and later doing analysis on the complete body column
twitter_df$Body <- paste(twitter_df$Title, twitter_df$Body)



#preprocessing, removing the html code characters

twitter_df$Body <- gsub("<.*?>", "", twitter_df$Body)

corpus <- (VectorSource(twitter_df$Body))
corpus <- Corpus(corpus)
summary(corpus)


#showing first 6 corpuses
for (i in 1:6) print (corpus[[i]]$content)
corpus[[1]]$content



#1st lowering the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content

#2nd remove the stopwords
corpus <- tm_map(corpus, content_transformer(removeWords), 
                 stopwords("english"))
corpus[[1]]$content

#3rd remove the punctuations
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus[[1]]$content

#4th stem words
corpus <- tm_map(corpus, stemDocument)
corpus[[1]]$content

#5th trip white spaces
corpus <- tm_map(corpus, stripWhitespace) 
corpus[[1]]$content

df <- data.frame(text = get("content", corpus),stringsAsFactors = F)


df$text[grepl('api.*.1(\\.(0|1))*.',df$text)][1]
# "custom code second part use httpsdevtwittercomdocsapi1getstatusesusertimelin get latest tweet specif user write code search within respons object 1 hashtag searchkey"

df$ver <- 1
df[grepl('api.*.1(\\.(0|1))*.',df$text),]$ver <- 1
df[grepl('api.*.2(\\.(0|1))*.',df$text),]$ver <- 2
df[grepl('api.*.3(\\.(0|1))*.',df$text),]$ver <- 3

write.csv(df,'transformed_tweets_data.csv',quote = F, row.names = F, na = '')

word_summary <-df%>%
  group_by(ver) %>%
  mutate(word_count = lengths(gregexpr("\\W+", text)) + 1)%>%
  ungroup()




pirateplot(formula =  word_count ~ ver, #Formula
           data = word_summary, #Data frame
           xlab = NULL, ylab = "Word counts", #Axis labels
           main = "Word counts per post by twitter API versions", #Plot title
           pal = "google", #Color scheme
           point.o = .2, #Points
           avg.line.o = 1, #Turn on the Average/Mean line
           theme = 0, #Theme
           point.pch = 16, #Point `pch` type
           point.cex = 1.5, #Point size
           jitter.val = .1, #Turn on jitter to see the songs better
           cex.lab = .9, cex.names = .7) #Axis label size


twitter_df$cleaned_text <- df$text
twitter_df$ver <- df$ver

twitter_df$CreationDate <- as.POSIXct(as.character(twitter_df$CreationDate),format="%Y-%m-%d %H:%M:%S")

temp_df <- twitter_df[,c('cleaned_text','ver','CreationDate')]
colnames(temp_df) <- c('text','ver','post_date')
temp_df$post_date <- as.Date(temp_df$post_date)
write.csv(temp_df,'data/transformed_tweets_data.csv',quote = F, row.names = F, na='')


ts_df <- temp_df%>%group_by(post_date)%>%summarise(number_posts = n())%>%arrange(desc(post_date))
ggplot(ts_df,aes(x=post_date, y = number_posts))+ geom_line()+
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 


plt <- ggplot(twitter_df, aes(x=CreationDate, y= ver))+geom_point()
plt





new_sentiments <- sentiments %>% #From the tidytext package
  filter(lexicon != "loughran") %>% #Remove the finance lexicon
  mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                             ifelse(lexicon == "AFINN" & score < 0,
                                    "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()

new_sentiments %>%
  group_by(lexicon, sentiment, words_in_lexicon) %>%
  summarise(distinct_words = n_distinct(word)) %>%
  ungroup() %>%
  spread(sentiment, distinct_words) %>%
  mutate(lexicon = color_tile("lightblue", "lightblue")(lexicon),
         words_in_lexicon = color_bar("lightpink")(words_in_lexicon)) %>%
  my_kable_styling(caption = "Word Counts Per Lexicon")


#breaking sentence that is body of the post into single word messages
twitter_tidy_df <- twitter_df %>%
  unnest_tokens(word, cleaned_text)


#choosing the appropriate lexicon based on the join of number of words 
temp_df <- twitter_tidy_df %>%
  mutate(words_in_lyrics = n_distinct(word)) 

#NRC wins out of all others
twitter_tidy_df %>%
  mutate(words_in_lyrics = n_distinct(word)) %>%
  inner_join(new_sentiments) %>%
  group_by(lexicon, words_in_lyrics, words_in_lexicon) %>%
  summarise(lex_match_words = n_distinct(word)) %>%
  ungroup() %>%
  mutate(total_match_words = sum(lex_match_words), #Not used but good to have
         match_ratio = lex_match_words / words_in_lyrics) %>%
  select(lexicon, lex_match_words,  words_in_lyrics, match_ratio) %>%
  mutate(lex_match_words = color_bar("lightpink")(lex_match_words),
         lexicon = color_tile("lightgreen", "lightgreen")(lexicon)) %>%
  my_kable_styling(caption = "Lyrics Found In Lexicons")




twitter_tidy_bing <- twitter_tidy_df %>%
  inner_join(get_sentiments("bing"))
twitter_tidy_nrc <- twitter_tidy_df %>%
  inner_join(get_sentiments("nrc"))
twitter_tidy_sub <- twitter_tidy_df %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))







nrc_plot <- twitter_tidy_nrc %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  #Use `fill = -word_count` to make the larger bars darker
  ggplot(aes(sentiment, word_count, fill = -word_count)) +
  geom_col() +
  guides(fill = FALSE) + #Turn off the legend
  theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  scale_y_continuous(limits = c(0, 15000)) + #Hard code the axis limit
  ggtitle("Prince NRC Sentiment") +
  coord_flip()

nrc_plot



bing_plot <- twitter_tidy_bing %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  coord_flip()
bing_plot






ver_sentiment_nrc <- twitter_tidy_nrc %>%
  group_by(ver, sentiment) %>%
  count(ver, sentiment) %>%
  select(ver, sentiment, sentiment_ver_count = n)

#Get the total count of sentiment words per year (not distinct)
total_sentiment_ver <- twitter_tidy_nrc %>%
  count(ver) %>%
  select(ver, ver_total = n)

#Join the two and create a percent field
year_radar_chart <- ver_sentiment_nrc %>%
  inner_join(total_sentiment_ver, by = "ver") %>%
  mutate(percent = sentiment_ver_count / ver_total * 100 ) %>%
  select(-sentiment_ver_count, -ver_total) %>%
  spread(ver, percent) %>%
  chartJSRadar(showToolTipLabel = TRUE,
               main = "Twitter API Versions Radar")

year_radar_chart



twitter_tidy_bing$year <- year(twitter_tidy_bing$CreationDate)

twitter_polarity_year <- twitter_tidy_bing %>%
  count(sentiment, year) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)

polarity_over_time <- twitter_polarity_year %>%
  ggplot(aes(year, polarity, color = ifelse(polarity >= 0,my_colors[5],my_colors[4]))) +
  geom_col() +
  geom_smooth(method = "loess", se = FALSE) +
  geom_smooth(method = "lm", se = FALSE, aes(color = my_colors[1])) +
  theme_lyrics() + theme(plot.title = element_text(size = 11)) +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Polarity Over Time")
polarity_over_time







plot_words_94_96 <- twitter_tidy_nrc %>%
  group_by(sentiment) %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  slice(seq_len(8)) %>% #consider top_n() from dplyr also
  ungroup()

plot_words_94_96 %>%
  #Set `y = 1` to just plot one variable and use word as the label
  ggplot(aes(word, 1, label = word, fill = sentiment )) +
  #You want the words, not the points
  geom_point(color = "transparent") +
  #Make sure the labels don't overlap
  geom_label_repel(forection = "y",
                   box.padding = 0.04,
                   segment.color = "transparent",
                   size = 3) +
  facet_grid(~sentiment) +
  theme_lyrics() +
  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
        axis.title.x = element_text(size = 6),
        panel.grid = element_blank(), panel.background = element_blank(),
        panel.border = element_rect("lightgray", fill = NA),
        strip.text.x = element_text(size = 9))  +
  xlab(NULL) + ylab(NULL) +
  ggtitle("Common words for each sentiment") +
  coord_flip()








a
temp_df2$mnth <- as.factor(format(temp_df2$post_date,'%B'))
temp_df2$mnth_cr <- format(temp_df2$post_date,'%m')

temp_df3<- temp_df2%>%group_by(mnth,mnth_cr)%>%summarise(post_count = n())%>%arrange(mnth_cr)


ggplot(temp_df3,aes(x=mnth,y=post_count))+geom_bar()
