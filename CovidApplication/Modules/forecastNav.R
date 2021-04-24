

forecastNavUI <- function(id) {
  ns <- NS(id)
  tagList(
      h4("Parameters"),
      selectInput(NS(id, "Msea"), h5('Country'), choices = list(
        Asia = c("Brunei"="Brunei", "Burma"="Burma", "Cambodia"="Cambodia", 
                 "Timor-Leste"="Timor-Leste", "Indonesia"="Indonesia", "Laos"="Laos", "Malaysia"="Malaysia", 
                 "Philippines"="Philippines","Singapore"="Singapore", "Thailand"="Thailand", "Vietnam"="Vietnam"),
        Europe = c("Austria"="Austria", "Belgium" = "Belgium", "Denmark" = "Denmark", "Finland" ="Finland","France" = "France", 
                   "Germany" = "Germany", "Greece" = "Greece", 
                   "Ireland" = "Ireland","Italy" ="Italy", "Luxembourg" ="Luxembourg", "Netherlands" = "Netherlands", 
                   "Portugal" = "Portugal","Spain" ="Spain", "Sweden" = "Sweden", "United Kingdom" = "United Kingdom"),
        Australia_Oceania = c("Australia" = "Australia", "New Zealand" = "New Zealand"),
        North_South_America = c("US" = "US", "Mexico" = "Mexico", "Haiti" = "Haiti", "Canada" = "Canada", "Panama"="Panama")
      ),selectize = FALSE, selected = "Singapore"),
      actionButton(ns("goButton"), label = "Go"),
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


etsmodelPanelUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
      selectInput("dayselection", 'Forecast Horizon', choices =
                    c("60 days"="60 days",
                      "50 days"="50 days",
                      "40 days" = "40 days",
                      "30 days" = "30 days",
                      "20 days" = "20 days",
                      "10 days" = "10 days"),
                  width = '100%'),
      ),
      column(6,
      dateRangeInput("date_range", "Change dataset Date Range:",
                     start = "1-23-2020", # Start date of the selected df
                     end = getMyDate(confirmed_cases_raw), # End date of the selected df
                     format = "m-d-yyyy")
    )
    ),
    
    fluidRow(
      column(4,
            selectInput("errorinput", 'Error', choices =
                           c("Additive"="additive",
                             "Multiplicative"="multiplicative"),
                         width = '100%')
             
      ),
      column(4, 
             selectInput("trendinput", 'Trend', choices =
                           c("Additive"="additive",
                             "Multiplicative"="multiplicative",
                             "None" = "none"),
                         width = '100%')
      ),
      column(4,
             selectInput("seasoninput", 'Season', choices =
                           c("Additive"="additive",
                             "Multiplicative"="multiplicative",
                             "None" = "none"),
                         width = '100%'),
             actionButton("ETSGo", label = "Go")
      )
    ),
    fluidRow(
      column(12,
             plotlyOutput("etspredictive"))),
    
    fluidRow(
      column(12,
             reactableOutput("etspredictiveaccuracy")))
  )
}


etsmodelPanelServer <-function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    }
  )
}


prophetmodelPanelUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             selectInput("dayselection", 'Forecast Horizon', choices =
                           c("60 days"="60 days",
                             "50 days"="50 days",
                             "40 days" = "40 days",
                             "30 days" = "30 days",
                             "20 days" = "20 days",
                             "10 days" = "10 days"),
                         width = '100%'),
      ),
      column(6,
             dateRangeInput("date_range", "Change dataset Date Range:",
                            start = "1-23-2020", # Start date of the selected df
                            end = getMyDate(confirmed_cases_raw), # End date of the selected df
                            format = "m-d-yyyy")
      )
    ),
    
    fluidRow(
      column(4,
             selectInput("growthinput", 'Growth', choices =
                           c("Linear"="linear",
                             "Logistic"="logistic"),
                         width = '100%')
             
      ),
      column(4, 
             selectInput("changepointinput", 'changepoint_range', choices =
                           c("0.1"="0.1",
                             "0.2"="0.2",
                             "0.3"="0.3",
                             "0.4"="0.4",
                             "0.5"="0.5",
                             "0.6"="0.6",
                             "0.7"="0.7",
                             "0.8" = "0.8",
                             "0.9" ="0.9"),
                         width = '100%')
      ),
      column(4,
             selectInput("pseasoninput", 'Season', choices =
                           c("Additive"="additive",
                             "Multiplicative"="multiplicative",
                             "None" = "none"),
                         width = '100%'),
             actionButton("ProphetGo", label = "Go")
      )
    ),
    fluidRow(
      column(12,
             plotlyOutput("prophetpredictive"))),
    
    fluidRow(
      column(12,
             reactableOutput("prophetpredictiveaccuracy"))),
    br()
  )
}


prophetmodelPanelServer <-function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    }
  )
}
