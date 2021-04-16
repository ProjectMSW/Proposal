
deathBivariateUI <- function(id) {
  ns <- NS(id)
  tagList(
      
    hr(),
    
    checkboxInput(inputId = "y_variable", 
                  "Y-variable:", 
                  value = FALSE),
      #choices = c("Cumulative Deaths" = "total_deaths",
      #             "Fatality Rate" = "case_fatality_rate"),
      # selected = "Cumulative Deaths"
      # ),
      
      
    tags$h5("Plot options:"),
      
    conditionalPanel(condition = "y_variable == TRUE"),
      
    selectInput(inputId = "testCum",
                "Option Cumulative",
                list("lm", "glm", "gam", "loess", "rlm")
                ),
      
      
    checkboxInput(inputId = "y_variable", 
                  "Y-variable:", 
                  value = FALSE
                  ),
      #choices = c("Cumulative Deaths" = "total_deaths",
      #             "Fatality Rate" = "case_fatality_rate"),
      # selected = "Cumulative Deaths"
      # ),
      
      
    tags$h5("Plot options:"),
      
    conditionalPanel(condition = "y_variable == TRUE"),
      
    selectInput(inputId = "testCum",
                "Option Cumulative",
                list("lm", "glm", "gam", "loess", "rlm")
                ),
      
    conditionalPanel(condition = paste0("input['", ns("y_variable"), "'] == 'Fatality Rate' "),
                     selectInput(inputId = "testFatal",
                                 "Option Fatal",
                                 list("lm", "glm", "gam", "loess", "rlm"))),
      
    hr(),
      
    submitButton("Go!"),
      
    selectInput("plotType", "Plot Type",
                c(Scatter = "scatter", Histogram = "hist")
                ),
  
    # Only show this panel if the plot type is a histogram
  
    conditionalPanel(condition = "input.plotType == 'hist'",
                     selectInput("breaks", "Breaks",
                                 c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom"))
                     ),
                   # Only show this panel if Custom is selected
                   
    conditionalPanel(condition = "input.breaks == 'custom'",
                     sliderInput("breakCount", "Break Count", min = 1, max = 50, value = 10)
                     )
                   
  )
}