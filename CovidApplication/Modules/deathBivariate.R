
deathBivariateUI <- function(id,a) {
  ns <- NS(id)
  countrydata <- getIndividualCountryData(confirmed_cases_country_level,a)
  tagList(
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
    
  )
  
}

deathBivariateServer <- function(id) {
  print("am i here")
  moduleServer(id, function(input, output, session) {
    
  })
}