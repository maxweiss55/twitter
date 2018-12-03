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

#Commented out below is the code used to build the "all_tweets.csv" dataset, combining 
#all of fivethirtyeight's political Twitter data from the following study 
# https://fivethirtyeight.com/features/the-worst-tweeter-in-politics-isnt-trump/ 
#"all_tweets.csv" is include in the repo instead of real-time building to prepare for the case
#where fivethirtyeight changes or removes the dataset.
#
# trump <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/twitter-ratio/realDonaldTrump.csv") #Trump Twitter Data
# 
# obama <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master") #Obama Twitter Data
# 
# sens <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/twitter-ratio/senators.csv") #Senator Twitter Data
# 
# obama <- obama %>%
#   mutate(bioguide_id = NA, party = NA, state = NA) #Add rows to enable binding
# 
# trump <- trump %>%
#   mutate(bioguide_id = NA, party = NA, state = NA) #Add rows to enable binding
#
# all_tweets <- rbind(trump, obama, sens) #Bind the three datasets together
# 
# write.csv(all_tweets, file = "all_tweets.csv") #Write new .csv file with all of the available data
#Load in dataset, built by description above. Dataset includes:
#3,200 most recent tweets from President Obama, President Trump, and each Senator's Twitter account
#Time posted (created_at); Text of tweet (text); Url (url); 
#number of retweets (retweets), favorites (favorites), replies (replies); Twitter Username (user);
#Member IDs from the "Biographical Directory of the United States Congress" (bioguide_id); 
#Party ID (party); State Representing (state)

twitter <- read_csv("all_tweets.csv") #Download original dataset

#I am interested only in tweets starting from Trump's inauguration. I am also only interested in 
#Senators' and Trump's tweets from this period. In addition to a different sociopolitical
#dynamic before and after Trump became president, the way in which fivethirtyeight collected the data 
#(each user's last 3200 tweets) means data before this time period is sparse in the dataset.
#Conveniently, the dataset includes nearly an exact 9-month period from Trump's inauguration. 
#In my cleaning of the large dataset below, I filter to only tweets in the 9 months following 
#Trump's inauguration, and I filter out Obama's tweets from this period because they are scarce and
#likely to be politically uninformative with Obama out of office.

#Download all .rds files produced in .rmd file
#For information on creation of each of these .rds files, see .rmd notes

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
#This Summary Table was included to give an overview of the relative frequencies each group
#of interest tweets and the reception these tweets receive.

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
                 
                 #Tabs used for clear organization
                
                 tabPanel("Summary Statistics", fluidPage(theme = shinytheme("cerulean"),
                   
                   titlePanel("Summary Statistics"),
                   
                   sidebarPanel(
                     
                     #Select Bin number for Histograms. The histograms show the frequency of tweets
                     #indexed by time. This feature was added for the user to compare tweet frequency 
                     #for whatever time interval is desireable.
                     
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
                     the amount Tweeted from each group and average reception received.",
                     
                     plotlyOutput(outputId = "densityplot", height = 400),
                     
                     "The histogram above displays the number of tweets published per time interval.
                     Notice there are periods of increased and decreased Tweeting. One can posit
                     what may have caused this at one time or another but would need to look further
                     at the content of the Tweets to truly understand the trends. The fill of the
                     histogram is separated by the portion that came from each Senator.",
                     
                     plotlyOutput(outputId = "densityplot2", height = 400),
                     
                     "The histogram above displays the number of tweets published per time interval.
                      Notice there are periods of increased and decreased Tweeting. One can posit
                      what may have caused this at one time or another but would need to look further
                      at the content of the Tweets to truly understand the trends."
                   
                     )
                
                 )),
                 
                 tabPanel("Word Use Frequencies", fluidPage(theme = shinytheme("cerulean"),
                   
                   titlePanel("Word Use Frequencies"),
                   
                   sidebarPanel(
                     
                     textInput("word", "Search Any Word!", "word"),
                     
                     "Input a word to see how different groups use the word. The search is case sensitive."
                   
                     ),
                   
                   mainPanel(
                     
                     h2("Word Use Frequency Table"),
                     
                     tableOutput("freqtable"),
                     "Table shows the raw number of times each group has used a given word.",
                     
                     h2("Relative Use of Word"),
                     
                     plotlyOutput("avg_use"),
                     "The plot above compares the relative use of a given word by each user group 
                      during the first nine months of Donald Trump's Presidency in 2017. The Average
                      Word Use is defined as the raw number of times a word was used, divided by
                      the total number of words tweeted by a user group. Thus, the counts here are
                      adjusted for groups of varying total tweet frequency. Democrat, Independent, and 
                      Republican are all taken from the Twitter accounts of all Senators during the 
                      first nine months of Donald Trump's Presidency. Trump tweets were taken from
                      the same period.",
                     
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
                      A line of best fit with 95% confidence intervals is shown. One can notice that,
                      overall, word use is similar between the parties; however, many words diverge
                      from this trend and are informative about political elite party rhetoric.",
                     
                     #Below are JPEG images of the wordclouds built using the Wordcloud2 package
                     #Providing the interactive wordclouds would have been more desireable, but
                     #Shiny would not load the page if the interactive wordcloud was included.
                     #Using the JPEG images gave a solution that Shiny could handle.
                     
                     h2("Word Cloud: Word Use Frequency: Democrats"),
                     
                     HTML('<center><img src="dem.jpg" height = 400 width = 700 ></center>'),
                     
                     "Words used by the group are scaled by the relative frequency their use for visualization.",
                     
                     h2("Word Cloud: Word Use Frequency: Republicans"),
                     
                     HTML('<center><img src="repub.jpg" height = 400 width = 700 ></center>'),
                    
                    "Words used by the group are scaled by the relative frequency their use for visualization.",
                     
                     h2("Word Cloud: Word Use Frequency: Trump"),
                     
                     HTML('<center><img src="trump.jpg" height = 400 width = 700 ></center>'),
                     
                     "Words used by the group are scaled by the relative frequency their use for visualization."
                   )
                   
                 )),
                 
                 tabPanel("Random Tweet Generator", fluidPage(theme = shinytheme("cerulean"),

                   titlePanel("Random Tweet Generator"),
                   
                   sidebarPanel(
                     actionButton("explore", "Explore!"),
                     br(),
                     "Click to explore the data. Clicking will bring up three tweets from the dataset, selected at random."
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
                     plotlyOutput("sentiments"),
                     
                     "The plots above compare the sentiment of the language tweeted by each user group 
                      during the first nine months of Donald Trump's Presidency in 2017. Democrat, Independent, and
                      Republican are all taken from the Twitter accounts of all Senators during the 
                      first nine months of Donald Trump's Presidency. Trump tweets were taken from
                      the same period.",
                     
                     br(), 
                     br(), 
                     
                     "The Bing and NRC bar graphs use the Bing and NRC lexicons to compare the 'positivity'
                     and 'negativity' of each user group. Both Bing and NRC provide labels for words of
                     'positive' and 'negative' sentiment. The number of words tweeted with the given
                     sentiment were divided by the total number of words used to determine the relative
                     'positivity' and 'negativity' of the user group on Twitter.",
                     
                     br(),
                     br(), 
                     
                     "The Afinn bar graph uses two different measures of positivity. The Afinn lexicon
                     scores words on a scale -5 to 5 of increasing positivity. The net positivity was found
                     by dividing the sum of all positivity scores by the total number of words used by the group.
                     This gives the groups overall positivity, adjusted for tweet frequency. The average positivity
                     was calculated by finding the mean of all scores within a group. The average positivity
                     was used in the comparison of individual Senators.",
                     
                     br(), 
                     br(),
                     
                     "Notably, Democratic Senators were both more negative and less positive than
                      their Republican collegues. Trump was both highly negative and highly positive
                      compared with the other groups, likely meaning he uses emotionally
                      inflammatory language overall. These conclusions were consistent across 
                      all measures of positivity and negativity."
                   )
                 ))
                )

# Server
server <- function(input, output) {
  
  #SUMMARY
  
  #The Senators plot was included to show periods of increased and decreased Tweeting. One can posit
  #what may have caused this at one time or another but would need to look further
  #at the content of the Tweets to truly understand the trends.
  
  output$densityplot <- renderPlotly({
  ggplot(senators, aes(x = Time, fill = Party, color = Party)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) +
      labs(title = "Senator Tweeting By Date (2017)",
           subtitle = "Separated by Party",
           x = "Date Tweeted", y = "Frequency") +
      guides(fill = guide_legend(title = "Party")) +
      geom_histogram(bins = input$bins)
  })
  
  #The Trump plot was included to show periods of increased and decreased Tweeting. One can posit
  #what may have caused this at one time or another but would need to look further
  #at the content of the Tweets to truly understand the trends.
  
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
  
  #These summary statistics were included for users to understand the relative frequency and reception
  #of tweets from different groups before diving into the rest of the data.
  
  output$summary <- renderTable(digits = 0, striped = TRUE, hover = TRUE, bordered = TRUE, {
    summary
  })
  
  
  #WORDS
  
    #This table simply shows the raw number of times each group has used a given word.
    #Raw frequency was used here because it is the most intuitive measure for viewers.
  
    output$freqtable <- renderTable(striped = TRUE, hover = TRUE, bordered = TRUE,
                                    spacing = "l", {
    
    count_table %>% filter(Word == input$word)
    
  })
    
    # The plot compares the relative use of a given word by each user group 
    # during the first nine months of Donald Trump's Presidency in 2017. The Average
    # Word Use is defined as the raw number of times a word was used, divided by
    # the total number of words tweeted by a user group. Thus, the counts here are
    # adjusted for groups of varying total tweet frequency. Democrat, Independent, and 
    # Republican are all taken from the Twitter accounts of all Senators during the 
    # first nine months of Donald Trump's Presidency. Trump tweets were taken from
    # the same period.
    
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
    
    # The plot compares the relative use of a given word by Senate Democrats 
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
        geom_point(color = "blue", alpha = .7) +
        geom_smooth(method = "lm", color = "dark blue") +
        labs(title = "Average Word Uses by Republicans vs. Democrats",
             subtitle = "During the First 9 Months of Trump's Presidency",
             x = "Average Democrat Word Use", y = "Average Republican Word Use")
    })
    
    
   #RANDOMIZE
    
    #This tool was included to enable users to explore the data and potentially find interesting
    #tweets on their own. The feature also allows for a better understanding of what the data means.
    
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
    
    #Several different methods for sentiment analysis were included in the following plots. This was
    #true to prove the robustness of results that will hopefully be interesting to users.
    
    output$sentiments <- renderPlotly({
      
      if (input$sentiment == "Positivity and Negativity (Bing)") {
        ggplot(twitter_bing, aes(x = Party, y = sentiment_strength, fill = sentiment)) +
          scale_fill_manual(values= c("#e60000", "#009933")) +
          geom_bar(stat = "identity", position = "dodge", color = "dark gray") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0.5)) + #Center title/subtitle/caption
          labs(title = "Positivity and Negativity of Tweets: Bing",
               subtitle = "Using the Bing Lexicon",
               x = "Group", y = "Sentiment Strength") + #Create title, subtitle, caption, x/y-axis labels
          guides(fill = guide_legend(title = "Sentiment")) #Set legend title
      }
      else if (input$sentiment == "Positivity and Negativity (NRC)"){
        ggplot(twitter_nrc, aes(x = Party, y = sentiment_strength, fill = sentiment)) +
          scale_fill_manual(values= c("#e60000", "#009933")) +
          geom_bar(stat = "identity", position = "dodge", color = "dark gray") +
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0.5)) + #Center title/subtitle/caption
          labs(title = "Positivity and Negativity of Tweets: NRC",
               subtitle = "Using the NRC Lexicon",
               x = "Group", y = "Sentiment Strength") + #Create title, subtitle, caption, x/y-axis labels
          guides(fill = guide_legend(title = "Sentiment")) #Set legend title    
      }
      else if (input$sentiment == "Positivity Measures (Afinn)"){
        
        twitter_afinn1 <- twitter_afinn %>%
          mutate(positivity_measure = case_when(positivity_measure == "average_positivity" ~ "Average Positivity",
                                                positivity_measure == "net_positivity" ~ "Net Positivity"))
        
        ggplot(twitter_afinn1, aes(x = Party, y = sentiment_strength, fill = positivity_measure)) +
          scale_fill_manual(values= c("#00e64d", "#009933")) +
          geom_bar(stat = "identity", position = "dodge", color = "dark gray") +
          theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5),
              plot.caption = element_text(hjust = 0.5)) + #Center title/subtitle/caption
          labs(title = "Positivity of Tweets: Afinn",
               subtitle = "Using the Afinn Lexicon",
               x = "Group", y = "Positivity") + #Create title, subtitle, caption, x/y-axis labels
          guides(fill = guide_legend(title = "Positivity Measure")) #Set legend title    
      }
      else if (input$sentiment == "Democrats/Independents Positivity"){
        senator_dem_afinn <- senator_afinn %>% filter(Party == "Democrat/Independent")
        
        ggplot(senator_dem_afinn, aes(x = User, y = average_positivity, label = User)) +
          geom_bar(stat = "identity", fill = "dark blue") +
          theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) + #Center title/subtitle/caption
          labs(title = "Positivity of Democrat and Independent Tweets",
               subtitle = "Using the Afinn Lexicon",
               x = "Senator", y = "Positivity")
      }
      else if (input$sentiment == "Republicans Positivity"){
        senator_rep_afinn <- senator_afinn %>% filter(Party == "Republican")
        
        ggplot(senator_rep_afinn, aes(x = User, y = average_positivity, label = User)) +
          geom_bar(stat = "identity", fill = "dark red") +
          theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0)) + #Center title/subtitle/caption
          labs(title = "Positivity of Republican Tweets",
               subtitle = "Using the Afinn Lexicon",
               x = "Senator", y = "Positivity") 
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

