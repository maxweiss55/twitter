library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(knitr)
twitter_big <- read_csv("all_tweets.csv")

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  titlePanel("Presidential and Senate Twitter Data"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 300,
                  value = 30)
    ),
    
    # Outputs
    mainPanel(
      plotOutput(outputId = "densityplot", height = 400),
      plotOutput(outputId = "densityplot2", height = 400),
      tableOutput("summary")
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  obama_trump <- twitter_big %>%
    select(created_at, user, retweets) %>%
    mutate(created_at = mdy_hm(created_at)) %>%
    filter(!is.na(created_at), !is.na(user), user %in% c("realDonaldTrump", "BarackObama"))
  
  senators <- twitter_big %>%
    select(-text, -url) %>%
    mutate(created_at = mdy_hm(created_at)) %>%
    filter(!is.na(created_at), !is.na(user), user != "realDonaldTrump", user != "BarackObama") 
  
  senators_table <- senators %>%
    mutate(Party = case_when(party == "D" ~ "Democrat", party == "R" ~ "Republican", party == "I" ~ "Independent")) %>%
    group_by(Party) %>%
    summarize("Total Senators" = n_distinct(user),
              "Total Tweets" = n(),
              "Average Replies" = round(mean(replies)),
              "Average Retweets" = round(mean(retweets)),
              "Average Favorites" = round(mean(favorites)))
  
  output$densityplot <- renderPlot({
    ggplot(senators, aes(x = created_at, fill = party)) +
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
    ggplot(obama_trump, aes(x = created_at, fill = user)) +
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5)) + #Center title/subtitle/caption
      labs(title = "President Tweeting By Date",
           subtitle = "Barack Obama and Donald Trump",
           x = "Date Tweeted", y = "Frequency") + #Create title, subtitle, caption, x/y-axis labels
      guides(fill = guide_legend(title = "President Twitter")) + #Set legend title
      geom_histogram(bins = input$bins)
  })
  
  output$summary <- renderTable({
    senators_table
  })

}

# Create the Shiny app object
shinyApp(ui = ui, server = server)
