
library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(knitr)
library(shiny)
library(shinyjs)


ui <- navbarPage("Presidential and Senate Twitter Activity: Trump's First 9 Months in Office",
                 
                 tabPanel("Polling Demographics", fluidPage(
                   
                   # Application title
                   titlePanel("Polling Population Characteristics and Predictive Error")
                
                 )),
                 
                 tabPanel("Polling Demographic", fluidPage(
                   
                   # Application title
                   titlePanel("Polling Population Characteristics and Predictive John")
                   
                 )),
                 
                 tabPanel("Polling Demographiz", fluidPage(
                   
                   # Application title
                   titlePanel("Polling Population Characteristics and Predictive Errorz")
                   
                 )),
                 
                 tabPanel("Polling Demogrs", fluidPage(
                   
                   # Application title
                   titlePanel("Polling Population Characteristics and Predictive Errorsci")
                 ))
                )

# Server
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)

