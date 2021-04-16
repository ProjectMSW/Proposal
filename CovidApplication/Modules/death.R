################
# Death Module #
################

# Module UI
deathUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    tabsetPanel(
      
      ##########################
      # Start of Bivariate Tab #
      ##########################
      tabPanel("Bivariate Analysis",
               hr(),
               sidebarPanel(
                 
                 checkboxInput(inputId = "A_y_variable", 
                             "Y-variable:", 
                             value = FALSE),
                             #choices = c("Cumulative Deaths" = "total_deaths",
                            #             "Fatality Rate" = "case_fatality_rate"),
                            # selected = "Cumulative Deaths"
                            # ),
                 
                 
                 tags$h5("Plot options:"),
                 conditionalPanel(
                   condition = "A_y_variable == TRUE"),
                   selectInput(inputId = "testCum",
                               "Option Cumulative",
                               list("lm", "glm", "gam", "loess", "rlm"))
                   
                   
                 ),
                 
                 conditionalPanel(
                   condition = paste0("input['", ns("A_y_variable"), "'] == 'Fatality Rate' "),
                   selectInput(inputId = "testFatal",
                               "Option Fatal",
                               list("lm", "glm", "gam", "loess", "rlm"))
                   
                   
                 ),
                 
                 hr(),
                 submitButton("Go!"),
                 
                 selectInput("plotType", "Plot Type",
                             c(Scatter = "scatter", Histogram = "hist")
                             ),
               # Only show this panel if the plot type is a histogram
                 conditionalPanel(condition = "input.plotType == 'hist'",
                                  selectInput("breaks", "Breaks",
                                              c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom")
                                              ),
                 # Only show this panel if Custom is selected
                 conditionalPanel(condition = "input.breaks == 'custom'",
                                  sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
                                  )
                 
               ), # End of sidebarPanel
               
               mainPanel(
                 
                 h4("Scatterplot"),
                 
                 
 
               ) # End of mainPanel
      ),
      ########################
      # End of Bivariate Tab #
      ########################
      
      #############################
      # Start of Multivariate Tab #
      #############################
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
      ###########################
      # End of Multivariate Tab #
      ###########################
    ) # End of 
  )
}

# Module Server
deathServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    ###################################
    # Start of Bivariate Plot Options #
    ###################################
    
    
    
    output$input_type_text <- renderText({
      input$A_y_variable
    })
    
    #################################
    # End of Bivariate Plot Options #
    #################################
    
    
    
    
    data <- reactive(mtcars[[input$var]])
    
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
    
  })
}