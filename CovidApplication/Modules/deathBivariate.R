###############
# Side Bar UI #
###############

deathBivariateSideBarUI <- function(id) {
  ns <- NS(id)
  tagList(
      
    hr(),
    
    checkboxInput(ns("y_variable"), 
                  "Y-variable:", 
                  value = FALSE),
      #choices = c("Cumulative Deaths" = "total_deaths",
      #             "Fatality Rate" = "case_fatality_rate"),
      # selected = "Cumulative Deaths"
      # ),
      
      
    tags$h5("Plot options:"),
      
    conditionalPanel(condition = "y_variable == TRUE"),
      
    selectInput(NS(id, "testCum"),
                "Option Cumulative",
                list("lm", "glm", "gam", "loess", "rlm")
                ),
      
    conditionalPanel(condition = paste0("input['", ns("y_variable"), "'] == 'Fatality Rate' "),
                     selectInput(NS(id, "testFatal"),
                                 "Option Fatal",
                                 list("lm", "glm", "gam", "loess", "rlm"))),
      
    hr(),
      
    submitButton("Go!"),
      
    selectInput(NS(id, "plotType"),
                "Plot Type",
                c(Scatter = "scatter", Histogram = "hist")
                ),
  
    # Only show this panel if the plot type is a histogram
  
    conditionalPanel(condition = "input.plotType == 'hist'",
                     ns = ns,
                     selectInput(ns("breaks"),
                                 "Breaks",
                                 c("Sturges", "Scott", "Freedman-Diaconis", "[Custom]" = "custom"))
                     ),
                   
    # Only show this panel if Custom is selected
                   
    conditionalPanel(condition = "input.breaks == 'custom'",
                     ns = ns,
                     sliderInput(ns("breakCount"),
                                 "Break Count", 
                                 min = 1, 
                                 max = 50, 
                                 value = 10)
                     )
                   
  )
}
###################
# Side Bar Server #
###################

deathBivariateSideBarServer <- function(id) {
  
  callModule(condpanel, "foo")
  
  output$selectInput <- renderUI({
    
    selectInput(
      inputId = "selectInput",
      label = "Conditional Input B :",
      choices = c("A","B","C")
    )
    
  })
  
}



#################
# Main Panel UI #
#################

deathBivariateMainPanel <- function(id) {
  ns <- NS(id)
  tagList(
    
    hr()
    
  )
}








deathBivariateServer <- function(id) {
  print("am i here")
  moduleServer(id, function(input, output, session) {
    
    print("i am inside module server")
    
    output$input_type_text <- renderText({
      input$y_variable
    })
    
    
    
  })
}


