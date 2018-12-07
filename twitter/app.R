# Download necessary libraries

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

# Commented out below is the code used to build the "all_tweets.csv" dataset, combining 
# all of fivethirtyeight's political Twitter data from the following study:
#  https://fivethirtyeight.com/features/the-worst-tweeter-in-politics-isnt-trump/ 
# in the following github repository:
# https://github.com/fivethirtyeight/data/tree/master/twitter-ratio 
# "all_tweets.csv" is include in this repo instead of real-time building to prepare for the case
# where fivethirtyeight changes or removes the dataset from their repo.
# 
#  trump <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/twitter-ratio/realDonaldTrump.csv") # Trump Twitter Data
#  
#  obama <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master") # Obama Twitter Data
#  
#  sens <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/twitter-ratio/senators.csv") # Senator Twitter Data
#  
#  obama <- obama %>%
#    mutate(bioguide_id = NA, party = NA, state = NA) # Add rows to enable binding
#  
#  trump <- trump %>%
#    mutate(bioguide_id = NA, party = NA, state = NA) # Add rows to enable binding
# 
#  all_tweets <- rbind(trump, obama, sens) # Bind the three datasets together
#  
# Write new .csv file with all of the available data into repo
# write.csv(all_tweets, file = "all_tweets.csv")


# Load in dataset, built by description above. Dataset includes:
# 
# The 3,200 most recent tweets from President Obama, President Trump, and each Senator's Twitter account
# Time posted (created_at); Text of tweet (text); Url (url); 
# number of retweets (retweets), favorites (favorites), replies (replies); Twitter Username (user);
# Member IDs from the "Biographical Directory of the United States Congress" (bioguide_id); 
# Party ID (party); State Representing (state)

twitter <- read_csv("all_tweets.csv") # Download original dataset

# I am interested only in tweets starting from Trump's inauguration. I am also only interested in 
# Senators' and Trump's tweets from this period. In addition to a different sociopolitical
# dynamic before and after Trump became president, the way in which fivethirtyeight collected the data 
# (each user's last 3200 tweets) means data before this time period is not necessarily available
# for every senator starting at the chronological beginning of the dataset. Conveniently,
# the dataset includes every tweet from an exact 9-month period from Trump's inauguration. 
# In my cleaning of the large dataset below, I filter to only tweets in the 9 months following 
# Trump's inauguration, and I filter out President Obama's tweets from this period because they are 
# scarce and likely to be politically uninformative with President Obama out of office.

# Download all .rds files produced in .rmd file
# For information on creation of each of these .rds files, see .rmd file and associated comments.
# .rds files were built in markdown instead of .R file to help with deploying speed of app

twitter_clean <- read_rds("data_tables/twitter_clean1")
wordcounts <- read_rds("data_tables/wordcounts1")
senator_party <- read_rds("data_tables/senator_party1")
wordcounts_senator <- read_rds("data_tables/wordcounts_senator1")
cloud_count <- read_rds("data_tables/cloud_count1")
count_table <- read_rds("data_tables/count_table1")
average_use <- read_rds("data_tables/average_use1")
relative_comp <- read_rds("data_tables/relative_comp1")
twitter_bing <- read_rds("data_tables/twitter_bing1")
twitter_nrc <- read_rds("data_tables/twitter_nrc1")
twitter_afinn <- read_rds("data_tables/twitter_afinn1")
senator_afinn <- read_rds("data_tables/senator_afinn1")

# Split the large dataset into one for Senators and another for Trump.
# This will be used when making the posting frequency timelines/histogram
# Putting this code here because I found putting it in the server caused some lag

senators <- twitter_clean %>%
  filter(User != "realDonaldTrump")

trump <- twitter_clean %>%
  filter(User == "realDonaldTrump")

# Build Summary Table that will be presented
# This Summary Table was included to give an overview of the relative frequencies each group
# of interest tweets and the reception these tweets receive. I wanted to provide a picture
# of what the data consists of up front to help contextualize the results for viewers.

summary <- twitter_clean %>%
  group_by(Party) %>%
  summarize("Total Users" = n_distinct(User),
            "Total Tweets" = n(),
            "Average Replies" = round(mean(Replies), digits = 0),
            "Average Retweets" = round(mean(Retweets), digits = 0),
            "Average Favorites" = round(mean(Favorites), digits = 0))


# Build ui with navbarPage, which allows the use of tabs.
# Tabs were used to separate information into clearer categories 

ui <- navbarPage("Presidential and Senate Twitter Activity: Trump's First 9 Months in Office",
                 
                 tabPanel("Description", fluidPage(theme = shinytheme("cerulean"),
                                                          
                 h1("Description"),
                 
                 # The following description can also be found in the README in the Github Repo
                 
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
                 the sentiments of different Senators and the President.",
                
                br(),
                br(),
                br(),
                
                p(strong("Brief Results:")),
                
                "Given President Trump and all Senators are actively on Twitter, I sought to 
                 understand and contrast political elites’ tweet language and sentiment.",
                
                br(),
                br(),
                
                "The language used by Senate Democrats and Republicans is highly polarized. 
                 Democrats more often used words like “trumpcare”, “medicaid”, and “aca”, 
                 while Republicans more often used words like “obamacare”, “hearing”, and “foxnews”. 
                 Trump was found to be highly positive and highly negative in his speech, 
                 likely meaning he uses more emotionally inflammatory language overall. 
                 Democratic Senators were both more negative and less positive than their 
                 Republican collegues.",
                
                br(),
                br(),
                br(),
                br()
                
                 )),                 
                
                 tabPanel("Summary Statistics", fluidPage(theme = shinytheme("cerulean"),
                   
                   titlePanel("Summary Statistics"),
                   
                   sidebarPanel(
                     
                     # Select Bin number for Histograms. The histograms show the frequency of tweets
                     # indexed by time. This feature was added for the user to compare tweet frequency 
                     # for whatever time interval is desireable. The maximum is 300 bins because it
                     # is a clean number that is a little over one bin for each day of the period.
                     # (2017-01-20 to 2017-10-20)
                     
                     sliderInput("bins",
                                 "Number of Bins:",
                                 min = 1,
                                 max = 300,
                                 value = 150),
                     
                     p("Select the desired number of bins to be used for the histograms on the right.")
                     
                   ),
                   
                   mainPanel(
                     
                     strong("Summary Statistics of Presidential and Senate Tweeting:Trump's First 9-Month's"),
                     
                     tableOutput("summary"),
                     
                     "The summary statistics calculated for Senate were split by Party. The table also
                     displays summary statistics for President Trump's Twitter account. The table shows
                     the amount Tweeted from each group and average reception received in the form of favorites,
                     retweets, and replies. Time Period: 2017-01-20 to 2017-10-20.",
                     
                     plotlyOutput(outputId = "densityplot", height = 400),
                     
                     "The histogram above displays the number of tweets published per time interval.
                     Notice there are periods of increased and decreased Tweeting. One can posit
                     what may have caused this at one time or another but would need to look further
                     at the content of the Tweets to truly understand the trends. Usually, there are
                     many reasons, coming together, as to why there was a high tweet frequency 
                     (for example, Russia meddling news during the Summer healthcare debate).
                     The fill of the histogram is separated by the portion that came from each Senator.
                     Time Period: 2017-01-20 to 2017-10-20.",
                     
                     plotlyOutput(outputId = "densityplot2", height = 400),
                     
                     "The histogram above displays the number of tweets published per time interval.
                      Notice there are periods of increased and decreased Tweeting. One can posit
                      what may have caused this at one time or another but would need to look further
                      at the content of the Tweets to truly understand the trends. Often, high tweet frequencies
                      at a given period of time could be due to multiple events converging at once.
                      Time Period: 2017-01-20 to 2017-10-20."
                   
                     )
                
                 )),
                 
                 tabPanel("Word Use Frequencies", fluidPage(theme = shinytheme("cerulean"),
                   
                   titlePanel("Word Use Frequencies"),
                   
                   sidebarPanel(
                     
                     textInput("word", "Search Any Word!", "word"),
                     
                     "Input a word to see how often different groups use the word. The search is case sensitive."
                   
                     ),
                   
                   mainPanel(
                     
                     h2("Word Use Frequency Table"),
                     
                     tableOutput("freqtable"),
                     
                     "The table shows the raw number of total times each group has used a given word.
                      Time Period: 2017-01-20 to 2017-10-20.",
                     
                     h2("Relative Use of Word"),
                     
                     plotlyOutput("avg_use"),
                     
                     "The plot above compares the relative use of a given word by each user group 
                      during the first nine months of Donald Trump's Presidency in 2017. The Average
                      Word Use is defined as the raw number of times a word was used, divided by
                      the total number of words tweeted by a user group. Thus, the counts here are
                      adjusted for groups of varying total tweet frequency. Democrat, Independent, and 
                      Republican are all taken from the Twitter accounts of all Senators during the 
                      first nine months of Donald Trump's Presidency. Trump tweets were taken from
                      the same period. Time Period: 2017-01-20 to 2017-10-20.",
                     
                     h2("Relative Word Comparison"),
                     
                     plotlyOutput("rel_use"),
                     
                     "The plot above compares the relative use of a given word by Senate Democrats 
                      and Republicans in the first nine months of Donald Trump's Presidency in 2017.
                      Average Word Use is defined as the raw number of times a word was used, divided by
                      the total number of words tweeted by a user group. Thus, the counts here are
                      adjusted for groups of varying total tweet frequency. Words that vary widely from 
                      the line of best fit are disproportionately used by one of the parties. For the
                      purposes of this visualization, uninformative stop words like 'the' and 'and' as
                      well as uninformative strings common to Twitter data like 't.co' and 'https' were
                      were removed to improve scaling. Additionally, any tweet that is used a combined
                      .0001 times or fewer were not included. The large number of of points was not
                      supported by Shiny, so words with a lower frequency of use were removed here.
                      A line of best fit with 95% confidence intervals is shown.",
                     
                     br(),
                     br(),
                     
                     "Notice: Overall, word use is similar between the parties; however, many words diverge
                      from this trend and are informative about polarized political elite partisan rhetoric.
                      Time Period: 2017-01-20 to 2017-10-20.",
                     
                     # Below are JPEG images of the wordclouds built using the Wordcloud2 package.
                     # Providing the interactive wordclouds would have been more desireable, but
                     # Shiny would not load the page after being uploaded to the web 
                     # if the interactive wordcloud was included. Instead, I took screenshots of each
                     # word cloud while deploying the Shiny app locally and added the images to the
                     # app to be deployed on the web. Using the JPEG images gave a solution that Shiny 
                     # could handle deploying on the web.
                     
                     h2("Word Cloud: Word Use Frequency: Democrats"),
                     
                     HTML('<center><img src="dem.jpg" height = 400 width = 700 ></center>'),
                     
                     "Words used by the group are scaled by the relative frequency their use for visualization. Words that vary widely from 
                      the line of best fit are disproportionately used by one of the parties. For the
                      purposes of this visualization, uninformative stop words like 'the' and 'and' as
                      well as uninformative strings common to Twitter data like 't.co' and 'https' were
                      were removed to improve scaling. Time Period: 2017-01-20 to 2017-10-20.",
                     
                     h2("Word Cloud: Word Use Frequency: Republicans"),
                     
                     HTML('<center><img src="repub.jpg" height = 400 width = 700 ></center>'),
                    
                     "Words used by the group are scaled by the relative frequency their use for visualization. Words that vary widely from 
                      the line of best fit are disproportionately used by one of the parties. For the
                      purposes of this visualization, uninformative stop words like 'the' and 'and' as
                      well as uninformative strings common to Twitter data like 't.co' and 'https' were
                      were removed to improve scaling. Time Period: 2017-01-20 to 2017-10-20.",
                     
                     h2("Word Cloud: Word Use Frequency: Trump"),
                     
                     HTML('<center><img src="trump.jpg" height = 400 width = 700 ></center>'),
                     
                     "Words used by the group are scaled by the relative frequency their use for visualization. Words that vary widely from 
                      the line of best fit are disproportionately used by one of the parties. For the
                      purposes of this visualization, uninformative stop words like 'the' and 'and' as
                      well as uninformative strings common to Twitter data like 't.co' and 'https' were
                      were removed to improve scaling. Time Period: 2017-01-20 to 2017-10-20."
                     
                   )
                   
                 )),
                
                # This random tweet generator feature was included for two reasons:
                # 1) After conducting and presenting the analysis of word frequencies and 
                # and sentiment used in the tweets, I felt as though any viewer may lose track
                # of the true significance of the results they are seeing. I believe grounding
                # the results in some real samples of tweets about foreign affairs, health care,
                # local politics, etc. will help people understand the scope of the results
                # 2) Personal interest of the viewer to search through the dataset.
                 
                 tabPanel("Random Tweet Generator", fluidPage(theme = shinytheme("cerulean"),

                   titlePanel("Random Tweet Generator"),
                   
                   sidebarPanel(
                     
                     actionButton("explore", "Explore!"),
                     
                     br(),
                     
                     "Click to explore the data. Clicking will bring up three tweets from the dataset, selected at random."
                     
                   ),
                   
                   mainPanel(
                     
                     # .gif image included for aesthetics
                     
                     HTML('<center><img src="tenor.gif" height = 400 width = 550 ></center>'),
                     
                     tableOutput("tweet_summary")
                     
                     )
                 )),
                 
                 tabPanel("Sentiment Analysis", fluidPage(theme = shinytheme("cerulean"),
                                                          
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
                     
                     plotlyOutput("sentiments"),
                     
                     "The plots above compare the sentiment of the language tweeted by each user group 
                      during the first nine months of Donald Trump's Presidency in 2017. Democrat, Independent, and
                      Republican are all taken from the Twitter accounts of all Senators during the 
                      first nine months of Donald Trump's Presidency. Trump tweets were taken from
                      the same period. Time Period: 2017-01-20 to 2017-10-20.",
                     
                     br(), 
                     br(), 
                     
                     "Three lexicon methods of sentiment analysis (Bing, NRC, Afinn) were included to provide the most accurate picture possible.
                     The Bing and NRC bar graphs use the Bing and NRC lexicons to compare the 'positivity'
                     and 'negativity' of each user group. Both Bing and NRC provide labels for words of
                     'positive' and 'negative' sentiment. The number of words tweeted with the given
                     sentiment were divided by the total number of words used to determine the relative
                     'positivity' and 'negativity' of the user group on Twitter.",
                     
                     br(),
                     br(), 
                     
                     "The Afinn bar graph offers a different type of positivity measure. The Afinn lexicon
                     scores words on a scale -5 to 5 of increasing positivity. The average positivity
                     was calculated by finding the mean of all scores within a group. The average positivity
                     was used in the comparison of individual Senators because it provides a more full
                     picture than using just positivity from NRC or Bing.",
                     
                     br(), 
                     br(),

                     "Notably, Democratic Senators were both more negative and less positive than
                      their Republican collegues. Trump was used both highly negative and highly positive
                      language compared with the other groups, likely meaning he uses emotionally
                      inflammatory language overall. These conclusions were consistent across 
                      all measures of positivity and negativity."
                     
                   )
                 ))
                )

# Server

server <- function(input, output) {
  
  # DATA SUMMARY
  
  # The Senators plot was included to show periods of increased and decreased Tweeting.
  
  output$densityplot <- renderPlotly({
    
  ggplot(senators, aes(x = Time, fill = Party)) +
      scale_fill_manual(values = c("#000099", "#9900cc", "#cc0000")) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) +
      labs(title = "Senator Tweeting By Date (2017)",
           x = "Date Tweeted", y = "Frequency") +
      guides(fill = guide_legend(title = "Party")) +
      geom_histogram(bins = input$bins)
    
  })
  
  # The Trump plot was included to show periods of increased and decreased Tweeting. One can posit
  # what may have caused this at one time or another but would need to look further
  # at the content of the Tweets to truly understand the trends.
  
  output$densityplot2 <- renderPlotly({
    
    ggplot(trump, aes(x = Time)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) +
      labs(title = "President Trump Tweeting By Date (2017)",
           x = "Date Tweeted", y = "Frequency") +
      geom_histogram(bins = input$bins, fill = "#cc0000")
    
  })
  
  # This Summary Table was included to give an overview of the relative frequencies each group
  # of interest tweets and the reception these tweets receive. I wanted to provide a picture
  # of what the data consists of up front to help contextualize the results for viewers.
  
  output$summary <- renderTable(digits = 0, striped = TRUE, hover = TRUE, bordered = TRUE, {
    
    summary
    
  })
  
  
  # WORD/LANGUAGE ANALYSIS
  
    # This table simply shows the raw number of times each group has used a given word.
    # Raw frequency was used here because it is the most intuitive measure for viewers.
  
    output$freqtable <- renderTable(striped = TRUE, hover = TRUE, bordered = TRUE,
                                    spacing = "l", {
    
    count_table %>% filter(Word == input$word)
    
  })
    
    # The plot above compares the relative use of a given word by each user group 
    # during the first nine months of Donald Trump's Presidency in 2017. The Average
    # Word Use is defined as the raw number of times a word was used, divided by
    # the total number of words tweeted by a user group. Thus, the counts here are
    # adjusted for groups of varying total tweet frequency. Democrat, Independent, and 
    # Republican are all taken from the Twitter accounts of all Senators during the 
    # first nine months of Donald Trump's Presidency. Trump tweets were taken from
    # the same period. Time Period: 2017-01-20 to 2017-10-20.
    
    output$avg_use <- renderPlotly({
      
      average_use %>%
        filter(word == input$word) %>%
        ggplot(aes(x = Party, y = avg_count)) +
        geom_col(fill = "sky blue", color = "navy blue") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0.5)) + 
        labs(title = "Average Use of Word",
             subtitle = "During the First 9 Months of Trump's Presidency",
             x = "Group", y = "Average Word Use") + 
        guides(fill = guide_legend(title = "Party"))
      
    })
    
    # The plot above compares the relative use of a given word by Senate Democrats 
    # and Republicans in the first nine months of Donald Trump's Presidency in 2017.
    # Average Word Use is defined as the raw number of times a word was used, divided by
    # the total number of words tweeted by a user group. Thus, the counts here are
    # adjusted for groups of varying total tweet frequency. Words that vary widely from 
    # the line of best fit are disproportionately used by one of the parties. For the
    # purposes of this visualization, uninformative stop words like 'the' and 'and' as
    # well as uninformative strings common to Twitter data like 't.co' and 'https' were
    # were removed to improve scaling. Additionally, any tweet that is used a combined
    # .0001 times or fewer were not included. The large number of of points was not
    # supported by Shiny, so words with a lower frequency of use were removed here.
    # A line of best fit with 95% confidence intervals is shown. This plot is interactive
    # to enable the user to explore words of interest further.
    
    output$rel_use <- renderPlotly({
      ggplot(relative_comp, aes(x = `Average Democrat Use`, y = `Average Republican Use`, label = Word)) +
        
        # Alpha set to 0.7 to enable viewing of plot density
        
        geom_point(color = "blue", alpha = .7) +
        
        # Linear model with standard error included for users to see which words are within a reasonably
        # similar use between the two parties and which were more polarized.
        
        geom_smooth(method = "lm", color = "dark blue") +
        labs(title = "Average Word Uses by Republicans vs. Democrats",
             x = "Average Democrat Word Use", y = "Average Republican Word Use")
      
    })
    
    
   # RANDOMIZED DATA
    
    # This random tweet generator feature was included for two reasons:
    # 1) After conducting and presenting the analysis of word frequencies and 
    # and sentiment used in the tweets, I felt as though any viewer may lose track
    # of the true significance of the results they are seeing. I believe grounding
    # the results in some real samples of tweets about foreign affairs, health care,
    # local politics, etc. will help people understand the scope of the results
    # 2) Personal interest of the viewer to search through the dataset.
    
    output$tweet_summary <- renderTable(striped = TRUE, hover = TRUE, bordered = TRUE,
                                        spacing = "l",{
      
      # Input here to refresh randomly generated words                                  
      
      input$explore
      
      # Only user, time, and text were included here. This is because it these were the only
      # variables necessary to acheive the two goals previously described.
      # Users can place who is tweeting when and the content of that tweet.
                                          
      twitter_clean %>%
        select(-Party, -State) %>%
        
        #Sample three at a time because only one at a time requires too high of a clicking-to-reading ratio
        
        sample_n(3) %>%
        select(-Replies, -Retweets, -Favorites) %>%
        
        #Time parsed to character for displaying
        
        mutate(Time = as.character(Time))
      
    })
    
    
    # SENTIMENT ANALYSIS
    
    # Several different methods for sentiment analysis were included in the following plots. This was
    # true to prove the robustness of results that will hopefully be interesting to viewers.
    
    output$sentiments <- renderPlotly({
      
      if (input$sentiment == "Positivity and Negativity (Bing)") {
        
        ggplot(twitter_bing, aes(x = Party, y = sentiment_strength, fill = sentiment)) +
          scale_fill_manual(values= c("#e60000", "#009933")) +
          geom_bar(stat = "identity", position = "dodge", color = "dark gray") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0.5)) + # Center title/subtitle/caption
          labs(title = "Positivity and Negativity of Tweets: Bing",
               x = "Group", y = "Sentiment Strength") + # Create title, subtitle, caption, x/y-axis labels
          guides(fill = guide_legend(title = "Sentiment")) # Set legend title
        
      }
      
      else if (input$sentiment == "Positivity and Negativity (NRC)"){
        
        ggplot(twitter_nrc, aes(x = Party, y = sentiment_strength, fill = sentiment)) +
          scale_fill_manual(values= c("#e60000", "#009933")) +
          geom_bar(stat = "identity", position = "dodge", color = "dark gray") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0.5)) + # Center title/subtitle/caption
          labs(title = "Positivity and Negativity of Tweets: NRC",
               x = "Group", y = "Sentiment Strength") + # Create title, subtitle, caption, x/y-axis labels
          guides(fill = guide_legend(title = "Sentiment")) # Set legend title    
        
      }
      
      else if (input$sentiment == "Positivity Measures (Afinn)"){
        
        ggplot(twitter_afinn, aes(x = Party, y = average_positivity)) +
          geom_bar(stat = "identity", color = "dark gray", fill = "#009933") +
          theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0.5)) + # Center title/subtitle/caption
          labs(title = "Positivity of Tweets: Afinn",
               subtitle = "Using the Afinn Lexicon",
               x = "Group", y = "Average Positivity") # Create title, subtitle, caption, x/y-axis labels
      
      }
      
      else if (input$sentiment == "Democrats/Independents Positivity"){
        
        senator_dem_afinn <- senator_afinn %>% filter(Party == "Democrat/Independent")
        
        ggplot(senator_dem_afinn, aes(x = User, y = average_positivity, label = User)) +
          geom_bar(stat = "identity", fill = "dark blue") +
          
          # I considered labelling the x-axis with Senators' names instead of their Twitter handles,
          # but I ultimately decided against this because these tweets are not necessarily coming from
          # the mouths (or, rather, thumbs) of the Senatora themselves. They may be written by press teams.
          # The handle still includes at least the Senators' last names, so this was sufficient information.
          
          theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) + # Center title/subtitle/caption
          labs(title = "Positivity of Democrat and Independent Tweets",
               subtitle = "Using the Afinn Lexicon",
               x = "Senator", y = "Positivity")
        
      }
      
      else if (input$sentiment == "Republicans Positivity"){
        
        senator_rep_afinn <- senator_afinn %>% filter(Party == "Republican")
        
        # I considered labelling the x-axis with Senators' names instead of their Twitter handles,
        # but I ultimately decided against this because these tweets are not necessarily coming from
        # the mouths (or, rather, thumbs) of the Senatora themselves. They may be written by press teams.
        # The handle still includes at least the Senators' last names, so this was sufficient information.
        
        ggplot(senator_rep_afinn, aes(x = User, y = average_positivity, label = User)) +
          geom_bar(stat = "identity", fill = "dark red") +
          theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) + # Center title/subtitle/caption
          labs(title = "Positivity of Republican Tweets",
               subtitle = "Using the Afinn Lexicon",
               x = "Senator", y = "Positivity") 
        
      }
    })
}

#  Run the application 
shinyApp(ui = ui, server = server)

