
library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(knitr)
library(shiny)
library(shinyjs)
library(wordcloud2)
library(tidytext)
library(plotly)
library(gganimate)
library(shinythemes)

twitter <- read_csv("all_tweets.csv")

twitter_clean <- twitter %>%
  select(user, created_at, text, replies, retweets, favorites, party, state) %>%
  mutate(created_at = mdy_hm(created_at),
         party = case_when(party == "D" ~ "Democrat", 
                           party == "R" ~ "Republican",
                           party == "I" ~ "Independent",
                           user == "realDonaldTrump" ~ "Trump")) %>%
  filter(user != "BarackObama", created_at > "2017-01-20" & created_at < "2017-10-20") %>%
  rename("User" = "user", "Time" = "created_at", "Text" = "text", "Replies" = "replies", 
         "Retweets" = "retweets", "Favorites" = "favorites", "Party" = "party", "State" = "state")

senators <- twitter_clean %>%
  filter(User != "realDonaldTrump")

trump <- twitter_clean %>%
  filter(User == "realDonaldTrump")

summary <- twitter_clean %>%
  group_by(Party) %>%
  summarize("Total Users" = n_distinct(User),
            "Total Tweets" = n(),
            "Average Replies" = round(mean(Replies), digits = 0),
            "Average Retweets" = round(mean(Retweets), digits = 0),
            "Average Favorites" = round(mean(Favorites), digits = 0))

ui <- navbarPage("Presidential and Senate Twitter Activity: Trump's First 9 Months in Office",
                 
                 tabPanel("Summary Statistics", fluidPage(theme = shinytheme("cerulean"),
                   
                   # Application title
                   titlePanel("Summary Statistics"),
                   
                   sidebarPanel(
                     
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 300,
                                 value = 150)
                   ),
                   
                   mainPanel(
                     plotOutput(outputId = "densityplot", height = 400),
                     plotOutput(outputId = "densityplot2", height = 400),
                     tableOutput("summary")
                   )
                
                 )),
                 
                 tabPanel("Word Use Frequencies", fluidPage(theme = shinytheme("cerulean"),
                   
                   # Application title
                   titlePanel("Word Use Frequencies"),
                   
                   sidebarPanel(
                     selectInput("group", "Group:", c("Senate Democrats", "Senate Republicans", 
                                                      "All Senate", "Trump", "All"), "All"),
                     textInput("word", "Search Any Word!", "word")
                   ),
                   
                   mainPanel(
                     h2("Word Cloud: Word Use Frequency"),
                     wordcloud2Output(outputId = "cloud"),
                     
                     h2("Word Use Frequency Table"),
                     tableOutput("freqtable"),
                     
                     h2("Relative Use of Word"),
                     plotOutput("avg_use")
                   )
                   
                 )),
                 
                 tabPanel("Random Tweet Generator", fluidPage(theme = shinytheme("cerulean"),
                   
                   # Application title
                   titlePanel("Random Tweet Generator"),
                   
                   sidebarPanel(
                     actionButton("explore", "Explore!")
                   ),
                   
                   mainPanel(
                     HTML('<center><img src="tenor.gif" height = 400 width = 550 ></center>'),
                     tableOutput("tweet_summary")
                     )
                 )),
                 
                 tabPanel("Polling Demogrs", fluidPage(theme = shinytheme("cerulean"),
                   
                   # Application title
                   titlePanel("Polling Population Characteristics and Predictive Errorsci")
                 ))
                )

# Server
server <- function(input, output) {
  
  #SUMMARY
  
  output$densityplot <- renderPlot({
  ggplot(senators, aes(x = Time, fill = Party, color = Party)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) + #Center title/subtitle/caption
      labs(title = "Senator Tweeting By Date",
           subtitle = "Separated by Party",
           x = "Date Tweeted", y = "Frequency") + #Create title, subtitle, caption, x/y-axis labels
      guides(fill = guide_legend(title = "Party")) + #Set legend title
      geom_histogram(bins = input$bins)
  })
  
  output$densityplot2 <- renderPlot({
    ggplot(trump, aes(x = Time)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) + #Center title/subtitle/caption
      labs(title = "President Tweeting By Date:",
           subtitle = "Donald Trump",
           x = "Date Tweeted", y = "Frequency") + #Create title, subtitle, caption, x/y-axis labels
      geom_histogram(bins = input$bins, fill = "#ff8080")
  })
  
  output$summary <- renderTable(digits = 0, striped = TRUE, hover = TRUE, bordered = TRUE, {
    summary
  })
  
  
  #WORDS
  
  output$cloud <- renderWordcloud2({
    
    if (input$group == "Senate Democrats") {
      
      cloud_count <- twitter_clean %>%
        filter(Party == "Republican") %>%
        unnest_tokens(word, Text) %>%
        anti_join(stop_words) %>%
        count(word) %>%
        arrange(desc(n)) %>%
        filter(word != "https", word != "t.co", word != "amp", word != "rt") %>%
        head(n = 180)
      
      wordcloud2(cloud_count)
      
    } else if (input$group == "Senate Republicans") {
      
      cloud_count <- twitter_clean %>%
        filter(Party == "Republican") %>%
        unnest_tokens(word, Text) %>%
        anti_join(stop_words) %>%
        count(word) %>%
        arrange(desc(n)) %>%
        filter(word != "https", word != "t.co", word != "amp", word != "rt") %>%
        head(n = 180)
      
      wordcloud2(cloud_count)
      
    } else if (input$group == "All Senate") {
      
      cloud_count <- twitter_clean %>%
        filter(User != "realDonaldTrump") %>%
        unnest_tokens(word, Text) %>%
        anti_join(stop_words) %>%
        count(word) %>%
        arrange(desc(n)) %>%
        filter(word != "https", word != "t.co", word != "amp", word != "rt") %>%
        head(n = 180)
      
      wordcloud2(cloud_count)
      
    } else if (input$group == "Trump") {
      
      cloud_count <- twitter_clean %>%
        filter(User == "realDonaldTrump") %>%
        unnest_tokens(word, Text) %>%
        anti_join(stop_words) %>%
        count(word) %>%
        arrange(desc(n)) %>%
        filter(word != "https", word != "t.co", word != "amp", word != "rt") %>%
        head(n = 180)
      
      wordcloud2(cloud_count)
      
    } else {
      cloud_count <- twitter_clean %>%
        unnest_tokens(word, Text) %>%
        anti_join(stop_words) %>%
        count(word) %>%
        arrange(desc(n)) %>%
        filter(word != "https", word != "t.co", word != "amp", word != "rt") %>%
        head(n = 180)
      
      wordcloud2(cloud_count)
    }
  })
    
    output$freqtable <- renderTable(striped = TRUE, hover = TRUE, bordered = TRUE,
                                    spacing = "l", {
    
    count_table <- twitter_clean %>%
      unnest_tokens(word, Text) %>%
      group_by(Party) %>%
      filter(word == input$word) %>%
      count(word) %>%
      select(Party, n, word) %>%
      rename("Group" = "Party", "Uses" = "n", "Word" = "word")
    
    count_table
    
  })
    
    output$avg_use <- renderPlot({
      average_use <- twitter_clean %>%
        group_by(Party) %>%
        unnest_tokens(word, Text) %>%
        count(word) %>%
        group_by(Party) %>%
        mutate(total_words = sum(n),
               avg_count = n / total_words) %>%
        filter(word == input$word) %>%
        ggplot(aes(x = Party, y = avg_count)) +
        geom_col(fill = "skyblue", color = "navy blue")
      
      average_use
    })
    
   #RANDOMIZE
    
    output$tweet_summary <- renderTable({
      
      input$explore
      
      twitter_clean %>%
        select(-Party, -State) %>%
        sample_n(1) %>%
        select(-Replies, -Retweets, -Favorites)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

