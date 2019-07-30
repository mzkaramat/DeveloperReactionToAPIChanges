setwd("D:\\University\\DeveloperAPIopinionMining\\data")


library(data.table)
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



orig_data <- fread('apsnetwebapi.csv')

orig_data$CreationDate <- as.Date(orig_data$creationdate)

table(year(orig_data$CreationDate))




orig_data$version <- ifelse(orig_data$CreationDate < as.Date('2006/04/10'),4, 
                            ifelse(orig_data$CreationDate < as.Date('2013/10/17'),4.5,
                                   ifelse(orig_data$CreationDate < as.Date('2015/07/10'),4.6,
                                          ifelse(orig_data$CreationDate < as.Date('2016/08/02'),4.7,
                                                 ifelse(orig_data$CreationDate < as.Date('2017/10/17'),4.7,5)
                                                 )
                                          )
                                   )
                            
                            )


table(orig_data$version)



orig_data <- orig_data[orig_data$version > 0, ]



table(orig_data$version)


#text,ver,post_date





#data transformation
twitter_df <- orig_data
#merging the body into the title as this information will be useful and later doing analysis on the complete body column
twitter_df$body <- paste(twitter_df$title, twitter_df$body)



#preprocessing, removing the html code characters

twitter_df$body <- gsub("<.*?>", "", twitter_df$body)

corpus <- (VectorSource(twitter_df$body))
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



#text,ver,post_date

orig_data$text <- df$text
orig_data$title <- NULL
orig_data$body <- NULL
orig_data$tags<- NULL
colnames(orig_data) <- c("post_date",'ver','text')

fwrite(orig_data,'aspnet_transformed_data.csv')
