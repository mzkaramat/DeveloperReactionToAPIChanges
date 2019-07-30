#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#setting the working directory
setwd("D:\\DeveloperAPIopinionMining\\TextMining\\data")

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#setting the working directory
#setwd("D:\\University\\DeveloperAPIopinionMining\\data")

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
library(plotly)



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
my_colors <-
  c("#E69F00",
    "#56B4E9",
    "#009E73",
    "#CC79A7",
    "#D55E00",
    "#D65E00")

#Customize ggplot2's default theme settings
#This tutorial doesn't actually pass any parameters, but you may use it again in future tutorials so it's nice to have the options
theme_lyrics <- function(aticks = element_blank(),
                         pgminor = element_blank(),
                         lt = element_blank(),
                         lp = "none")
{
  theme(
    plot.title = element_text(hjust = 0.5),
    #Center the title
    axis.ticks = aticks,
    #Set axis ticks to on or off
    panel.grid.minor = pgminor,
    #Turn the minor grid lines on or off
    legend.title = lt,
    #Turn the legend title on or off
    legend.position = lp
  ) #Turn the legend on or off
}

#Customize the text tables for consistency using HTML formatting
my_kable_styling <- function(dat, caption) {
  kable(dat, "html", escape = FALSE, caption = caption) %>%
    kable_styling(
      bootstrap_options = c("striped", "condensed", "bordered"),
      full_width = FALSE
    )
}




options(shiny.maxRequestSize = 100 * 1024 ^ 2)



widget_style <-
  "display: inline-block;
vertical-align: text-top;
padding: 50px;
border: solid;
border-width: 1px;
border-radius: 2px;
border-color: #CCC;"

# Define UI for application that draws a histogram
ui <- fluidPage(tabsetPanel(
  id = "tabs",
  tabPanel("upload",
           # Sidebar with a slider input for number of bins
           fluidRow(br(),
                    column(
                      10,
                      offset = 2,
                      div(
                        style = widget_style,
                        
                        fluidRow(column(
                          4,
                          offset = 4,
                          h3('Some guidlines for data format'),
                          
                          p('1. Data need to have text column'),
                          p('2. Data need to have pre-labelled api version'),
                          br(),
                          fileInput(
                            "file1",
                            "Choose the data file",
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")
                          )
                        ))
                        ,
                        # Horizontal line ----
                        tags$hr(),
                        fluidRow(
                          column(3,
                                 # Input: Select separator ----
                                 radioButtons(
                                   "sep",
                                   "Separator",
                                   choices = c(
                                     Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"
                                   ),
                                   selected = ","
                                 )),
                          column(3,
                                 # Input: Select quotes ----
                                 radioButtons(
                                   "quote",
                                   "Quote",
                                   choices = c(
                                     None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"
                                   ),
                                   selected = '"'
                                 )),
                          column(
                            3,
                            # Input: Select number of rows to display ----
                            radioButtons(
                              "disp",
                              "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head"
                            )
                          ),
                          column(3,
                                 # Input: Checkbox if file has header ----
                                 checkboxInput("header", "Header", TRUE))
                        ),
                        
                        
                        
                        # Horizontal line ----
                        br(),
                        hr(),
                        
                        
                        fluidRow(column(
                          7,
                          h4('Data preview'),
                          br(),
                          br(),
                          tableOutput("contents")
                        ),
                        column(
                          5,
                          h4('Data summary'),
                          br(),
                          br(),
                          tableOutput("contents_summary")
                        )),
                        br(),
                        hr(),
                        fluidRow(column(
                          2,
                          offset = 5,
                          br(),
                          actionButton(inputId = "submitInfo", label = "Analyze")
                        ))
                      )
                    ))),
  tabPanel("Analyze", 
           h3('Quick Summary:'),
           br(),
           
           fluidRow(column(
             5,offset = 1,
             
             plotOutput("analysis_summary_count")
           ),
           column(
             5, plotlyOutput('analysis_ts_lenght')
           )
           ),
           br(),
           fluidRow(
             column(5,offset = 1, 
                    plotlyOutput('analysis_version_counts')
             ),
             column(5, 
                    plotlyOutput('analysis_monthly_counts')
             )
           ),
           hr(),
           fluidRow(column(
             2,
             offset = 5,
             br(),
             actionButton(inputId = "insights_button", label = "Get Insights")
           ))
  ),
  tabPanel("Insights", 
           br(),
           br(),
           
           fluidRow(column(
             6,
             chartJSRadarOutput(outputId ='sentiment_score_plt')
           ),
           column(
             6,
             plotOutput('sentiment_common_words_plt')
           )
           )
  ),
  tabPanel("Summary", 
           br(),
           br(),
           br(),
           br(),
           
           
           fluidRow(
             column(
               6,
               plotlyOutput('summary_plt')
             ),
             column(
               6, 
               plotlyOutput('summary_plt_post')
             )
           )
  )
))

l_text_df <- data.frame(text = character(),
                        ver = numeric())

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      df <- read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    if (input$disp == "head") {
      tempdf <- data.frame(df)
      tempdf$text <- strtrim(tempdf$text, 70)
      tempdf$text <- paste(tempdf$text , '...')
      return(head(tempdf))
    }
    else {
      tempdf <- data.frame(df)
      tempdf$text <- strtrim(tempdf$text, 70)
      tempdf$text <- paste(tempdf$text , '...')
      return(head(tempdf))
    }
    
  })
  
  output$contents_summary <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      df <- read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
      df$text <- as.character(df$text)
      df$post_date <- as.Date(df$post_date)
      l_text_df <<- data.frame(df)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
    
    temp_processing_df <- data.frame(df)
    temp_processing_df$text <- as.character(temp_processing_df$text)
    
    
    api_versions <- length(unique(temp_processing_df$ver))
    mean_text_lenght <-
      round(mean(nchar(temp_processing_df$text)), 2)
    med_text_lenght <- median(nchar(temp_processing_df$text))
    
    temp_summary_keys <-
      c('API Versions', 'Mean Text lenghts', 'Median Text lenghts')
    temp_summary_vals <-
      c(api_versions, mean_text_lenght, med_text_lenght)
    
    temp_summary_df <-
      data.frame(temp_summary_keys = temp_summary_keys,
                 temp_summary_vals = temp_summary_vals)
    
    colnames(temp_summary_df) <- c('Summary keys',
                                   'Summary Values')
    
    if (input$disp == "head") {
      return(temp_summary_df)
    }
    else {
      return(temp_summary_df)
    }
    
  })
  
  observeEvent(input$submitInfo, {
    print(dim(l_text_df))
    if (nrow(l_text_df) > 0)
      updateTabsetPanel(session = session,
                        inputId = "tabs",
                        selected = "Analyze")
    else
      showNotification("Please select data file first.",duration = 2)
  })
  
  
  observeEvent(input$insights_button, {
    updateTabsetPanel(session = session,
                      inputId = "tabs",
                      selected = "Insights")
  })
  
  
  
  output$analysis_summary_count <- renderPlot({
    word_summary <-l_text_df%>%
      group_by(ver) %>%
      mutate(word_count = lengths(gregexpr("\\W+", text)) + 1)%>%
      ungroup()
    
    pirateplot(formula =  word_count ~ ver, #Formula
               data = word_summary, #Data frame
               xlab = NULL, ylab = "Word counts", #Axis labels
               main = "Word counts per post by API versions", #Plot title
               pal = "google", #Color scheme
               point.o = .2, #Points
               avg.line.o = 1, #Turn on the Average/Mean line
               theme = 0, #Theme
               point.pch = 16, #Point `pch` type
               point.cex = 1.5, #Point size
               jitter.val = .1, #Turn on jitter to see the songs better
               cex.lab = .9, cex.names = .7) #Axis label size
  })
  
  output$analysis_ts_lenght <- renderPlotly({
    ts_df <- l_text_df%>%group_by(post_date)%>%summarise(number_posts = n())%>%arrange(post_date)
    plt <- ggplot(ts_df,aes(x=post_date, y = number_posts))+ geom_line()+
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank()) +
      xlab('Time')+
      ylab('Number of posts')+
      ggtitle('Number of posts by time')
    ggplotly(plt)
  })
  
  output$analysis_version_counts <- renderPlotly({
    l_text_df$ver <- as.factor(l_text_df$ver)
    plt <- ggplot(l_text_df, aes(x = ver))+geom_bar() +
      xlab('Versions')+
      ylab('Count of posts')+
      ggtitle('Number of posts per Version')+
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5))
    ggplotly(plt)
  })
  
  
  output$analysis_monthly_counts <- renderPlotly({
    
    l_text_df$mnth <- as.factor(format(l_text_df$post_date,'%B'))
    l_text_df$mnth_cr <- format(l_text_df$post_date,'%m')
    
    temp_df3<- l_text_df%>%group_by(mnth,mnth_cr)%>%summarise(post_count = n())%>%arrange(mnth_cr)
    
    
    
    plt <- ggplot(temp_df3,aes(x=mnth,y=post_count))+geom_bar(stat = "identity")+
      xlab('MonthsCou')+
      ylab('Count of posts')+
      ggtitle('Number of posts per Version')+
      theme_bw() +
      theme(axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5)
      )
    ggplotly(plt)
  })
  
  output$sentiment_score_plt <- renderChartJSRadar({
    
    twitter_tidy_df <- l_text_df %>%
      unnest_tokens(word, text)
    
    twitter_tidy_bing <- twitter_tidy_df %>%
      inner_join(get_sentiments("bing"))
    twitter_tidy_nrc <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc"))
    twitter_tidy_sub <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc")) %>%
      filter(!sentiment %in% c("positive", "negative"))
    
    
    
    
    
    ver_sentiment_nrc <- twitter_tidy_nrc %>%
      group_by(ver, sentiment) %>%
      count(ver, sentiment) %>%
      select(ver, sentiment, sentiment_ver_count = n)
    
    print(ver_sentiment_nrc)
    
    #Get the total count of sentiment words per year (not distinct)
    total_sentiment_ver <- twitter_tidy_nrc %>%
      count(ver) %>%
      select(ver, ver_total = n)
    
    print(total_sentiment_ver)
    
    year_radar_chart <- ver_sentiment_nrc %>%
      inner_join(total_sentiment_ver, by = "ver") %>%
      mutate(percent = sentiment_ver_count / ver_total * 100 ) %>%
      select(-sentiment_ver_count, -ver_total) %>%
      spread(ver, percent) %>%
      chartJSRadar(showToolTipLabel = TRUE,
                   main = "API Versions Radar")
    
    year_radar_chart
    
  })
  
  
  output$sentiment_common_words_plt <- renderPlot({
    twitter_tidy_df <- l_text_df %>%
      unnest_tokens(word, text)
    
    twitter_tidy_bing <- twitter_tidy_df %>%
      inner_join(get_sentiments("bing"))
    twitter_tidy_nrc <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc"))
    twitter_tidy_sub <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc")) %>%
      filter(!sentiment %in% c("positive", "negative"))
    
    
    
    
    plot_words_94_96 <- twitter_tidy_nrc %>%
      group_by(sentiment) %>%
      count(word, sort = TRUE) %>%
      arrange(desc(n)) %>%
      slice(seq_len(8)) %>% #consider top_n() from dplyr also
      ungroup()
    
    
    plt <- plot_words_94_96 %>%
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
      ggtitle("Most Common words for each sentiment") +
      coord_flip()
    
    plt
    
  })
  
  output$summary_plt <- renderPlotly({
    
    twitter_tidy_df <- l_text_df %>%
      unnest_tokens(word, text)
    
    twitter_tidy_bing <- twitter_tidy_df %>%
      inner_join(get_sentiments("bing"))
    twitter_tidy_nrc <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc"))
    twitter_tidy_sub <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc")) %>%
      filter(!sentiment %in% c("positive", "negative"))
    
    
    
    
    
    ver_sentiment_nrc <- twitter_tidy_bing %>%
      group_by(ver, sentiment) %>%
      count(ver, sentiment) %>%
      select(ver, sentiment, sentiment_ver_count = n)
    
    print(ver_sentiment_nrc)
    
    #Get the total count of sentiment words per year (not distinct)
    total_sentiment_ver <- twitter_tidy_bing %>%
      count(ver) %>%
      select(ver, ver_total = n)
    
    print(total_sentiment_ver)
    
    year_radar_chart <- ver_sentiment_nrc %>%
      inner_join(total_sentiment_ver, by = "ver") %>%
      mutate(percent = sentiment_ver_count / ver_total * 100 ) %>%
      select(-sentiment_ver_count, -ver_total)
    
    print(year_radar_chart)
    
    year_radar_chart <- year_radar_chart[year_radar_chart$sentiment == 'negative',]
    print(year_radar_chart)
    
    year_radar_chart$percent <- round(year_radar_chart$percent)
    
    plt <- ggplot(data=year_radar_chart, aes(x=ver, y=percent, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle('Negative Sentiment Progression')+
      xlab('API Version Number') + 
      ylab('Sentiment Score')
    
    plt
    
  })
  
  
  
  
  output$summary_plt_post <- renderPlotly({
    
    twitter_tidy_df <- l_text_df %>%
      unnest_tokens(word, text)
    
    twitter_tidy_bing <- twitter_tidy_df %>%
      inner_join(get_sentiments("bing"))
    twitter_tidy_nrc <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc"))
    twitter_tidy_sub <- twitter_tidy_df %>%
      inner_join(get_sentiments("nrc")) %>%
      filter(!sentiment %in% c("positive", "negative"))
    
    
    
    
    
    ver_sentiment_nrc <- twitter_tidy_bing %>%
      group_by(ver, sentiment) %>%
      count(ver, sentiment) %>%
      select(ver, sentiment, sentiment_ver_count = n)
    
    print(ver_sentiment_nrc)
    
    #Get the total count of sentiment words per year (not distinct)
    total_sentiment_ver <- twitter_tidy_bing %>%
      count(ver) %>%
      select(ver, ver_total = n)
    
    print(total_sentiment_ver)
    
    year_radar_chart <- ver_sentiment_nrc %>%
      inner_join(total_sentiment_ver, by = "ver") %>%
      mutate(percent = sentiment_ver_count / ver_total * 100 ) %>%
      select(-sentiment_ver_count, -ver_total)
    
    print(year_radar_chart)
    
    year_radar_chart <- year_radar_chart[year_radar_chart$sentiment == 'positive',]
    print(year_radar_chart)
    
    year_radar_chart$percent <- round(year_radar_chart$percent)
    
    plt <- ggplot(data=year_radar_chart, aes(x=ver, y=percent, group=1)) +
      geom_line()+
      geom_point()+
      ggtitle('Positive Sentiment Progression')+
      xlab('API Version Number') + 
      ylab('Sentiment Score')
    
    plt
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)




