

forecastNavUI <- function(id) {
  ns <- NS(id)
  tagList(
   
      selectInput(NS(id, "Msea"), "South East Asia", choices = c("Brunei"="Brunei", "Burma"="Burma", "Cambodia"="Cambodia", 
                                                         "Timor-Leste"="Timor-Leste", "Indonesia"="Indonesia", "Laos"="Laos", "Malaysia"="Malaysia", 
                                                         "Philippines"="Philippines","Singapore"="Singapore", "Thailand"="Thailand", "Vietnam"="Vietnam")),
      
      selectInput(NS(id, "MUS"), "USA", choices = c("US"="US")),
      
      #fileInput("file", "File input:"),
      
      
      actionButton(ns("button"), label = "Search"),
      
  
 
  )
}


forecastNavServer <- function(id) {
  print("am i here1111")
  print(id)
  moduleServer(id, function(input, output, session) {
    #data <- reactive(input$Msea)
    observeEvent(input$button, {
      print("datai am here at last")
      #countrydata <- getIndividualCountryData(confirmed_cases_country_level,input$Msea)
      
      print(input$Msea)
     
      #return(input$Msea)
      
    })
    
   
  })
}


EDACountryUI <- function(id,a) {
  ns <- NS(id)
  print("inside")
 # countrydata <- getIndividualCountryData(confirmed_cases_country_level,a)
  tagList(
    #textOutput(ns("distPlot"))
    plotlyOutput(ns("distPlot"))
    
  )
}
  
  EDACountryServer <- function(id) {
    #countrydata <- getIndividualCountryData(confirmed_cases_country_level,input$Msea)
    print("am i here1111")
    print(id)
    #print(input$Msea)
    moduleServer(id, function(input, output, session) {
    output$distPlot <-renderPlotly(
     #renderText(input$Msea)
      plot_time_series(input$Msea,Date, Daily_new_cases,
                       .facet_ncol =3, .facet_scales = "free",
                       .interactive = TRUE,)
      
    )
    })
    
  }
  
  
  
