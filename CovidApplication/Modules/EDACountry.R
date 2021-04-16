

EDACountryUI <- function(id,a) {
  ns <- NS(id)
  countrydata <- getIndividualCountryData(confirmed_cases_country_level,a)
  tagList(
    countrydata%>%
      plot_time_series(Date, Daily_new_cases, 
                       .facet_ncol =2, .facet_scales = "free",
                       .interactive = TRUE,
                       .plotly_slider = TRUE)
    
  )
  
}

forecastNavServer <- function(id) {
  print("am i here")
  moduleServer(id, function(input, output, session) {

  })
}