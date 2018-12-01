
library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(knitr)
library(shiny)
library(shinyjs)

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
                 
                 tabPanel("Summary Statistics", fluidPage(
                   
                   # Application title
                   titlePanel("Summary Statistics"),
                   
                   sidebarPanel(
                     
                     sliderInput("bins",
                                 "Number of bins:",
                                 min = 1,
                                 max = 300,
                                 value = 30)
                   ),
                   
                   mainPanel(
                     plotOutput(outputId = "densityplot", height = 400),
                     plotOutput(outputId = "densityplot2", height = 400),
                     tableOutput("summary")
                   )
                
                 )),
                 
                 tabPanel("Word Use Frequencies", fluidPage(
                   
                   # Application title
                   titlePanel("Word Use Frequencies")
                   
                 )),
                 
                 tabPanel("Random Tweet Generator", fluidPage(
                   
                   # Application title
                   titlePanel("Random Tweet Generator")
                   
                 )),
                 
                 tabPanel("Polling Demogrs", fluidPage(
                   
                   # Application title
                   titlePanel("Polling Population Characteristics and Predictive Errorsci")
                 ))
                )

# Server
server <- function(input, output) {
  
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)

