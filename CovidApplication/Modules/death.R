################
# Death Module #
################

# Module UI
deathUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      
      selectInput(inputId = "A_y_variable", "Y-variable:", choices = c("Cumulative Deaths", "Fatality Rate")),
      
      selectInput(inputId = "A_x_variable",
                  "X-variable:", 
                  choices = c("Total number of positive cases" = "total_cases",
                              "Total number of tests conducted" = "total_tests",
                              "Positive rate" = "positive_rate",
                              "Number of hospital beds" = "hospital_beds_per_thousand",
                              "Number of physicians" = "num_physicians_per_thousand",
                              "Number of handwashing facilities" = "handwashing_facilities",
                              "GDP per capita" = "gdp_per_capita",
                              "Current health expenditure (% of GDP)" = "current_health_exp_%gdp",
                              "Government health expenditure (% of total government expenditure)" = "govt_health_exp_%totalgovtexp",
                              "Total population" = "population",
                              "Population density" = "population_density",
                              "Human development index (HDI)" = "human_development_index",
                              "Annual international arrivals" = "annual_intl_arrivals_thousands")
                  ),
      
      sliderInput("slider", "Slider input:", 1, 100, 50),
      
      
      
      
      submitButton("submit", "Search")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Bivariate Analysis",
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
        tabPanel("Multivariate Analysis", 
                 
                 h5("This panel is intentionally left blank"),
                 
                 h4("Table"),
                 tableOutput("table"),
                 
                 h4("Verbatim text output"),
                 verbatimTextOutput("txtout"),
                 
                 h1("Header 1"),
                 h2("Header 2"),
                 h3("Header 3"),
                 h4("Header 4"),
                 h5("Header 5")
                 
                 
                 )
      )
    )
 
  )
}

# Module Server
deathServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}