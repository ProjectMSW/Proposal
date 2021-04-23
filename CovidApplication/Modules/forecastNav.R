

forecastNavUI <- function(id) {
  ns <- NS(id)
  tagList(
      
      selectInput(NS(id, "Msea"), 'Country', choices = list(
        Asia = c("Default: Singapore" = "Singapore","Brunei"="Brunei", "Burma"="Burma", "Cambodia"="Cambodia", 
                 "Timor-Leste"="Timor-Leste", "Indonesia"="Indonesia", "Laos"="Laos", "Malaysia"="Malaysia", 
                 "Philippines"="Philippines","Singapore"="Singapore", "Thailand"="Thailand", "Vietnam"="Vietnam"),
        Europe = c("Austria"="Austria", "Belgium" = "Belgium", "Denmark" = "Denmark", "Finland" ="Finland","France" = "France", 
                   "Germany" = "Germany", "Greece" = "Greece", 
                   "Ireland" = "Ireland","Italy" ="Italy", "Luxembourg" ="Luxembourg", "Netherlands" = "Netherlands", 
                   "Portugal" = "Portugal","Spain" ="Spain", "Sweden" = "Sweden", "United Kingdom" = "United Kingdom"),
        Australia_Oceania = c("Australia" = "Australia", "New Zealand" = "New Zealand"),
        North_South_America = c("US" = "US", "Mexico" = "Mexico", "Haiti" = "Haiti", "Canada" = "Canada", "Panama"="Panama")
      ),selectize = FALSE),
      actionButton(ns("goButton"), label = "Search"),
  )
}



forecastNavServer <- function(id) {
 
  print("In forecastNavServer")
  
  moduleServer(id, function(input, output, session) {
    values <- reactiveValues(default = 0)
    
    observeEvent(input$goButton,{
      values$default <- input$goButton
    })
    
   aa<- eventReactive(input$goButton, {
      isolate(input$Msea)
   })
   
   reactive(if(values$default == 0){
     return("Singapore")
   }else{
     return(aa())
   })

  })
}

csvFileUI <- function(id, label = "CSV file") {
    ns <- NS(id)
    tagList(
      fileInput(ns("file"), label)
    )
  }
  

  
  csvFileServer <- function(id, stringsAsFactors,myvalues) {
    moduleServer(
      id,
      function(input, output, session) {
        userFile <- reactive({
          validate(need(input$file, message = FALSE))
          input$file
        })
        dataframe <- reactive({
          abc<-read_csv(userFile()$datapath)
        })
        observe({
          msg <- sprintf("File %s was uploaded", userFile()$name)
          cat(msg, "\n")
          myvalues$default <-1
        })
        return(dataframe)
      }
    )
  }