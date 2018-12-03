#Download necessary libraries
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
library(shinythemes)

twitter <- read_csv("all_tweets.csv") #Download original dataset

#Download all .rds files produced in .rmd file
twitter_clean <- read_rds("twitter_clean1")
wordcounts <- read_rds("wordcounts1")
senator_party <- read_rds("senator_party1")
wordcounts_senator <- read_rds("wordcounts_senator1")
cloud_count <- read_rds("cloud_count1")
count_table <- read_rds("count_table1")
average_use <- read_rds("average_use1")
relative_comp <- read_rds("relative_comp1")
twitter_bing <- read_rds("twitter_bing1")
twitter_nrc <- read_rds("twitter_nrc1")
twitter_afinn <- read_rds("twitter_afinn1")
senator_afinn <- read_rds("senator_afinn1")

#Split the large dataset into one for Senators and another for Trump
senators <- twitter_clean %>%
  filter(User != "realDonaldTrump")
trump <- twitter_clean %>%
  filter(User == "realDonaldTrump")

#Build Summary Table that will be presented
summary <- twitter_clean %>%
  group_by(Party) %>%
  summarize("Total Users" = n_distinct(User),
            "Total Tweets" = n(),
            "Average Replies" = round(mean(Replies), digits = 0),
            "Average Retweets" = round(mean(Retweets), digits = 0),
            "Average Favorites" = round(mean(Favorites), digits = 0))

ui <- navbarPage("Presidential and Senate Twitter Activity: Trump's First 9 Months in Office",
                 
                 tabPanel("Description", fluidPage(theme = shinytheme("cerulean"),
                                                          
                 h1("Description"),
                 #The following description can also be found in the README in the Github Repo
                 br(),
                 
                 "We are in the Trump Era, which may also appropriately be called the ‘Twitter Era’.
                 President Trump is not the only politician who is taking advantage of the customizable, direct, public, and real-time platform.
                 Every Senator has an active Twitter account with hundreds-of-thousands or millions of followers. 
                 Trump alone, as of December 2018, has more than 50-million followers on the platform.",
                 br(),
                 br(),
                 "The language used by political elites is important in two main ways:",
                 br(),
                 "1)	The way political elites talk about their beliefs and actions in public may reflect those beliefs and actions.",
                 br(),
                 "2)	The way political elites talk about their beliefs and actions in public cues and may influence what citizens 
                 believe and how citizens act and vote.",
                 br(),
                 br(),
                 "The study investigates the language used by Senators and President Trump on Twitter during the Trump Presidency. 
                 The dataset that is the subject of this study was collected originally by FiveThirtyEight. 
                 The dataset was used originally for the study 'The Worst Tweeter In Politics Isn’t Trump' 
                 by Oliver Roeder, Dhrumil Mehta, and Gus Wezerek.",
                 br(),
                 br(),
                 a("Link to original article", href= "https://fivethirtyeight.com/features/the-worst-tweeter-in-politics-isnt-trump/"),
                 br(),
                 a("Link to Github associated with article", href= "https://github.com/fivethirtyeight/data/tree/master/twitter-ratio"),
                 br(),
                 br(),
                 "From these original datasets, one can extract every tweet from President Trump and all Senators for the first nine months of 
                 Trump’s Presidential term. I am interested only in tweets starting from Trump's inauguration.",
                 br(),
                 br(),
                 "Study of the data is split into four parts. First, I offer simple summary statistics and time-indexed histograms to give 
                 a sense of the nature of Senator and Trump Tweeting. Second, I investigate the differences in the language used by Senators 
                 and Trump by analyzing words used. Third, I have provided a sampling feature, where one can click through randomized tweets 
                 from the dataset for personal interest. Lastly, I use several different methods of sentiment analysis to study and compare 
                 the sentiments of different Senators and the President."
                 )),                 
                 
                 
                 tabPanel("Summary Statistics", fluidPage(theme = shinytheme("cerulean"),
                   
                   # Application title
                   titlePanel("Summary Statistics"),
                   
                   sidebarPanel(
                     #Select Bin number for Histograms
                     sliderInput("bins",
                                 "Number of Bins:",
                                 min = 1,
                                 max = 300,
                                 value = 150),
                     p("Select the desired number of bins to be used for the histograms on the right.")
                   ),
                   
                   mainPanel(
                     #Table title
                     strong("Summary Statistics of Presidential and Senate Tweeting:Trump's First 9-Month's"),
                     #Table
                     tableOutput("summary"),
                     #Table Caption
                     "The summary statistics calculated for Senate were split by Party. The table also
                     displays summary statistics for President Trump's Twitter account. The table shows
                     the amount Tweeted from each group and average reception received.",
                     #Histogram of Senator Tweets indexed by time
                     plotlyOutput(outputId = "densityplot", height = 400),
                     #Caption
                     "The histogram above displays the number of tweets published per time interval.
                     Notice there are periods of increased and decreased Tweeting. One can posit
                     what may have caused this at one time or another but would need to look further
                     at the content of the Tweets to truly understand the trends. The fill of the
                     histogram is separated by the portion that came from each Senator.",
                     
                     #Histogram of Trump Tweets indexed by time
                     plotlyOutput(outputId = "densityplot2", height = 400),
                     #Caption
                     "The histogram above displays the number of tweets published per time interval.
                      Notice there are periods of increased and decreased Tweeting. One can posit
                     what may have caused this at one time or another but would need to look further
                     at the content of the Tweets to truly understand the trends."
                   )
                
                 )),
                 
                 tabPanel("Word Use Frequencies", fluidPage(theme = shinytheme("cerulean"),
                   
                   # Application title
                   titlePanel("Word Use Frequencies"),
                   
                   sidebarPanel(
                     textInput("word", "Search Any Word!", "word")
                   ),
                   
                   mainPanel(
                     h2("Word Use Frequency Table"),
                     tableOutput("freqtable"),
                     
                     h2("Relative Use of Word"),
                     plotlyOutput("avg_use"),
                     
                     h2("Relative Word Comparison"),
                     plotlyOutput("rel_use"),
                     
                     h2("Word Cloud: Word Use Frequency: Democrats"),
                     HTML('<center><img src="trump.jpg" height = 400 width = 700 ></center>'),
                     
                     h2("Word Cloud: Word Use Frequency: Republicans"),
                     HTML('<center><img src="dem.jpg" height = 400 width = 700 ></center>'),
                     
                     h2("Word Cloud: Word Use Frequency: Trump"),
                     HTML('<center><img src="trump.jpg" height = 400 width = 700 ></center>')
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
                 
                 tabPanel("Sentiment Analysis", fluidPage(theme = shinytheme("cerulean"),
                   # Application title
                   titlePanel("Sentiment Analysis"),
                   
                   sidebarPanel(
                     selectInput("sentiment", "Select Sentiment Study:", 
                                 c("Positivity and Negativity (Bing)",
                                   "Positivity and Negativity (NRC)",
                                   "Positivity Measures (Afinn)",
                                   "Democrats/Independents Positivity",
                                   "Republicans Positivity"), "Positivity and Negativity (Bing)")
                   ),
                   
                   mainPanel(
                     plotlyOutput("sentiments")
                   )
                 ))
                )

# Server
server <- function(input, output) {
  
  #SUMMARY
  
  output$densityplot <- renderPlotly({
  ggplot(senators, aes(x = Time, fill = Party, color = Party)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) + #Center title/subtitle/caption
      labs(title = "Senator Tweeting By Date (2017)",
           subtitle = "Separated by Party",
           x = "Date Tweeted", y = "Frequency") + #Create title, subtitle, caption, x/y-axis labels
      guides(fill = guide_legend(title = "Party")) + #Set legend title
      geom_histogram(bins = input$bins)
  })
  
  output$densityplot2 <- renderPlotly({
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
  
  #output$cloud <- renderWordcloud2({
      
     #cloud_count2 <- cloud_count %>% filter(Party == input$group) %>%
     # count(word) %>%
     # arrange(desc(n)) %>%
     # head(n = 180)
    
      #wordcloud2(cloud_count2)
     
  #})
    
    output$freqtable <- renderTable(striped = TRUE, hover = TRUE, bordered = TRUE,
                                    spacing = "l", {
    
    count_table %>% filter(Word == input$word)
    
  })
    
    output$avg_use <- renderPlotly({
      average_use %>%
        filter(word == input$word) %>%
        ggplot(aes(x = Party, y = avg_count)) +
        geom_col(fill = "skyblue", color = "navy blue")
    })
    
    output$rel_use <- renderPlotly({
      ggplot(relative_comp, aes(x = `Average Democrat Use`, y = `Average Republican Use`, label = Word)) +
        geom_point(color = "skyblue")
    })
    
    
   #RANDOMIZE
    
    output$tweet_summary <- renderTable(striped = TRUE, hover = TRUE, bordered = TRUE,
                                        spacing = "l",{
      
      input$explore
      
      twitter_clean %>%
        select(-Party, -State) %>%
        sample_n(3) %>%
        select(-Replies, -Retweets, -Favorites) %>%
        mutate(Time = as.character(Time))
    })
    
    #SENTIMENT ANALYSIS
    
    output$sentiments <- renderPlotly({
      
      if (input$sentiment == "Positivity and Negativity (Bing)") {
        ggplot(twitter_bing, aes(x = Party, y = sentiment_strength, fill = sentiment)) +
          geom_bar(stat = "identity", position = "dodge")        
      }
      else if (input$sentiment == "Positivity and Negativity (NRC)"){
        ggplot(twitter_nrc, aes(x = Party, y = sentiment_strength, fill = sentiment)) +
          geom_bar(stat = "identity", position = "dodge")        
      }
      else if (input$sentiment == "Positivity Measures (Afinn)"){
        ggplot(twitter_afinn, aes(x = Party, y = sentiment_strength, fill = positivity_measure)) +
          geom_bar(stat = "identity", position = "dodge")     
      }
      else if (input$sentiment == "Democrats/Independents Positivity"){
        senator_dem_afinn <- senator_afinn %>% filter(Party == "Democrat/Independent")
        
        ggplot(senator_dem_afinn, aes(x = User, y = average_positivity, label = User)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) 
      }
      else if (input$sentiment == "Republicans Positivity"){
        senator_rep_afinn <- senator_afinn %>% filter(Party == "Republican")
        
        ggplot(senator_rep_afinn, aes(x = User, y = average_positivity, label = User)) +
          geom_bar(stat = "identity") +
          theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0))
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

