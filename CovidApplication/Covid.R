library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(timetk)
library(modeltime)
library(tidymodels)

#source("Modules/EDACountry.R")
source("Modules/forecastNav.R")
source("Modules/forecast.R")
source("Modules/death.R")
source("Modules/sentiment.R")
source("Modules/forecastWorker.R")
source("Modules/histogram.R")

ui <- fluidPage(
  titlePanel("Amanda is TOP student"),
  
  navbarPage(
    theme = shinytheme("sandstone"),
    "Covid",
    tabPanel("Forecasting Positive Cases",
             sidebarPanel(
               fileInput("file", "File input:"),
               textInput("txt", "Text input:", "general"),
               sliderInput("slider", "Slider input:", 1, 100, 30),
               tags$h5("Default actionButton:"),
               actionButton("action", "Search"),
               
               tags$h5("actionButton with CSS class:"),
               actionButton("action2", "Action button", class = "btn-primary")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab 1",
                          h4("Table"),
                          tableOutput("table"),
                          h4("Verbatim text output"),
                          verbatimTextOutput("txtout"),
                          h1("Header 1"),
                          h2("Header 2"),
                          h3("Header 3"),
                          h4("Header 4"),
                          h5("Header 5")
                 ),
                 tabPanel("Tab 2", "This panel is intentionally left blank"),
                 tabPanel("Tab 3", "This panel is intentionally left blank")
               )
             )
    ),
    tabPanel("Exploring Deaths", "Wait for Mdm"),
    tabPanel("Understanding vaccination sentiments", "Wait for Daniel"),
    tabPanel("test", 
             sidebarPanel(
               forecastNavUI("Nav")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Tab A", EDACountryUI("c","US")),
                 tabPanel("Tab 2", "This panel is intentionally left blank"),
                 tabPanel("Tab 3", "This panel is intentionally left blank")
               )
             )
      )
  )
  
)

server <- function(input, output, session){
  forecastNavServer("Nav")
  #EDACountryServer("c","US")
}

shinyApp(ui,server)






