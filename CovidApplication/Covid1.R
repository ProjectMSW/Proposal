library(shiny)
library(shinythemes)
library(plotly)
library(ggplot2)
library(timetk)
library(modeltime)
library(tidymodels)
library(tidyverse)
library(readr)
library(tsibble)
library(fpp3)
library(reactable)

library(UpSetR)


library(ggstatsplot)
library(ggExtra)
library(olsrr)
library(recipes)





source("Modules/forecastNav.R")
source("Modules/forecastWorker.R")
source("Modules/survey.R")



ui <- fluidPage(
  titlePanel("Covid Explorer"),
  

  
  navbarPage(
    theme = shinytheme("sandstone"),
    "Menu",
    tabPanel("Forecasting Positive Cases",
             sidebarPanel(
               csvFileUI("datafile", "Select File to Load (or default dataset will be used)"),
               conditionalPanel(
                 condition = "input.tabs == 'Explore Country Data'",
               forecastNavUI("datafile"),hr(),
               ),
               
               conditionalPanel(
                 condition = "input.tabs == 'Prediction'",
                 
                 checkboxGroupInput("variable1", "Model Selection",
                                    c("arima_boosted" ="arima_boosted", "ets" = "ets", "prophet"="prophet",
                                      "lm" = "lm", "mars" = "mars", "snaive" ="snaive", "ETS"="ETS")),
                 actionButton("ModelgoButton", label = "Go"),
                 hr(),
                 
                 
                 dateRangeInput("date_range", "Change Date Range for the dataset.:",
                                start = "1-23-2020", # Start date of the selected df
                                end = getMyDate(confirmed_cases_raw), # End date of the selected df
                                format = "m-d-yyyy")
                 
               )
             ),
             mainPanel(
               tabsetPanel(id="tabs",
                
                 tabPanel("Raw Data", 
                          textOutput("initialtext"),hr(),
                          dataTableOutput("table") ),
                 tabPanel("Explore Country Data",
                          textOutput("initialtext1"),
                          plotlyOutput("distPlot"),
                          plotlyOutput("anomalyPlot"),
                          plotlyOutput("acfPlot"),
                          plotlyOutput("stlPlot")
                         
                        ),
                 tabPanel("Prediction",
                      conditionalPanel(
                            condition = "input.tabs == 'Prediction'",
                            textOutput("initialdisplay")
                        ),
                          plotlyOutput("predictive"),
                          reactableOutput("accuracy")
                          )
                      )
             )
    ),
    tabPanel("Death",
             tabsetPanel(
               tabPanel("Bivariate Analysis", 
                        sidebarPanel(
                          #forecastNavUI("Nav")
                        ),
                        mainPanel(
                        )
               ),
               tabPanel("MultiVariate Analysis", 
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
                          
                        )
                        
               )
             )
    ),
    tabPanel("Understanding vaccination sentiments", 
             sidebarPanel(
               surveysideUI("datafile")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Survey Finding", plotOutput("likert")),
                 tabPanel("Association of Factors", "This is for factors"),
                 tabPanel("Data Exploration", "This is for data exploration")
               )
             )
          )
  )
)

server <- function(input, output, session){
  
  myvalues <- reactiveValues(default = 0)
  mypredictvalue <- reactiveValues(default =0)
  myinitialvalue <- reactiveValues(default =0)
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE,myvalues)
  output$initialtext <- renderText(
    if(myvalues$default == 0){ 
      paste("Please Upload the latest file from Johns Hopkins University. Exisitng Dataset as follows:", "", sep="\n\n\n")
    }else{
      ""
    }
  )
  
  output$initialtext1 <- renderText(
    if(myvalues$default == 0){
      "Using Default Dataset - Data collected from 22 Jan 2020 to 27 Mar 2021"
    }else{
      ""
    }
  )
  
  selectedCountry <- forecastNavServer("datafile")


  output$table <- renderDataTable({
    if(myvalues$default == 1){
    datafile()
    }else{
      confirmed_cases_raw
    }
  })
  
  
  output$distPlot <- renderPlotly({
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
    selected <- formatraw( datafile())
    }
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    plot_time_series(CountrySelected,Date, Daily_new_cases,
                     .facet_ncol =3, .facet_scales = "free",
                     .interactive = TRUE,.plotly_slider = TRUE,  .y_lab = "Number of Cases")
    
    })
  
  output$stlPlot <- renderPlotly({
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw( datafile())
    }
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    plot_stl_diagnostics(CountrySelected,
                         Date, Daily_new_cases,
                         .frequency = "auto", .trend = "auto",
                         .feature_set = c("observed", "season", "trend", "remainder"),
                         .interactive = TRUE)
    
    
  })
  
  output$anomalyPlot <- renderPlotly({
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
    }
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    plot_anomaly_diagnostics(CountrySelected,Date, Daily_new_cases, .facet_ncol = 2)
    
    
  })
  
  output$acfPlot <- renderPlotly({
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw( datafile())
    }
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    plot_acf_diagnostics(CountrySelected,Date, Daily_new_cases, 
                         .facet_ncol = 2, 
                         .facet_scale = "free",
                         .interactive = TRUE)
    
  })
  
  
  

  modelselection <- eventReactive(input$ModelgoButton, {
    isolate(input$variable1)
  })
  
  observeEvent(input$ModelgoButton,{
    myinitialvalue$default <- input$ModelgoButton
  })


  output$predictive <- renderPlotly({
    date_start <- input$date_range[1]
    date_start <- date_start+1
    x <- format(date_start, "%d-%m-%Y")
    x<-geYearMonth(x)
    y<-geYearMonth("27-03-2021")
   
    
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
      updateDateRangeInput(session, "date_range",end = getMyDate(datafile()))
      date_end <- input$date_range[2]
      date_end <-  date_end+1
      y <- format(date_end, "%d-%m-%Y")
      y<-geYearMonth(y)
    }
    selected <- getDataByTime(selected,x,y)
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- PredictionModel1(traindata)
    
    if(myinitialvalue$default >0){
      USmodels_tbl <- PredictionModel(traindata,modelselection())
    }
   
    
    UScalibration_tbl <- USmodels_tbl %>%
      modeltime_calibrate(new_data = testdata)
    
    UScalibration_tbl %>%
      modeltime_accuracy() 
    
    USrefit_tbl <- UScalibration_tbl %>%
      modeltime_refit(data = CountrySelected)
    
    USrefit_tbl %>%
      modeltime_forecast(h = "10 days", actual_data = CountrySelected) %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE,
        .title = selectedCountry()
      )
   
  })
  
 
  
  output$accuracy <- renderReactable({
    date_start <- input$date_range[1]
    date_start <- date_start+1
    x <- format(date_start, "%d-%m-%Y")
   
    x<-geYearMonth(x)
    y<-geYearMonth("27-03-2021")
  
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
      updateDateRangeInput(session, "date_range",end = getMyDate(datafile()))
      date_end <- input$date_range[2]
      date_end <-  date_end+1
      y <- format(date_end, "%d-%m-%Y")
      y<-geYearMonth(y)
    }
    selected <- getDataByTime(selected,x,y)
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- PredictionModel1(traindata)
    
    if(myinitialvalue$default >0){
      USmodels_tbl <- PredictionModel(traindata,modelselection())
    }
    
    UScalibration_tbl <- USmodels_tbl %>%
      modeltime_calibrate(new_data = testdata)
    
    UScalibration_tbl %>%
      modeltime_accuracy() 
    
    USrefit_tbl <- UScalibration_tbl %>%
      modeltime_refit(data = CountrySelected)
    
    USrefit_tbl %>%
      modeltime_forecast(h = "10 days", actual_data = CountrySelected) 
    
    
    USrefit_tbl %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(
        .interactive = TRUE,
        .title = selectedCountry()
      )
    
  })
  
  
  output$likert <- renderPlot({
    main_df<- read_csv("data/main_df.csv") 
    
    
    countrySelected = "Australia"   
    strength = 4  # For user to determine level of agreement - 4 Agree, 5 Strongly agree
    
    upset_df <- filter(main_df, country == countrySelected)
    # vac_1 - willing to get vaccine
    upset_df <- upset_df %>% mutate(vac_1_ag = case_when(
      vac_1 >= strength ~ as.integer(1),
      TRUE ~ as.integer(0)
    )
    )
    
    # vac2_1 - worried about getting COVID19
    upset_df <- upset_df %>% mutate(vac2_1_ag = case_when(
      vac2_1 >= strength ~ as.integer(1),
      TRUE ~ as.integer(0)
    ))
    
    # vac2_2 - worried about side effects of vaccine (inv relationship)
    upset_df <- upset_df %>% mutate(vac2_2_ag = case_when(
      vac2_2 <= (6-strength) ~ as.integer(1),   # when strength 4, equivalent is 2. When strength 5, equiv is 1
      TRUE ~ as.integer(0)
    ))
    
    # vac2_3 - confidence in government providing effective vaccine
    upset_df <- upset_df %>% mutate(vac2_3_ag = case_when(
      vac2_3 >= strength ~ as.integer(1),   
      TRUE ~ as.integer(0)
    ))
    
    # vac2_4 - confident vaccine protects recipient from COVID19 effects
    upset_df <- upset_df %>% mutate(vac2_4_ag = case_when(
      vac2_4 >= strength ~ as.integer(1),   
      TRUE ~ as.integer(0)
    ))
    
    # vac2_5 - confidence in vaccine preventing recipient from spreading COVID19
    upset_df <- upset_df %>% mutate(vac2_5_ag = case_when(
      vac2_5 >= strength ~ as.integer(1),   
      TRUE ~ as.integer(0)
    ))
    
    # vac2_6 - will regret if do not take vaccine
    upset_df <- upset_df %>% mutate(vac2_6_ag = case_when(
      vac2_6 >= strength ~ as.integer(1),   
      TRUE ~ as.integer(0)
    ))
    
    # vac_3 - keen to get vaccine 1 year later
    upset_df <- upset_df %>% mutate(vac_3_ag = case_when(
      vac_3 >= strength ~ as.integer(1),   
      TRUE ~ as.integer(0)
    ))
    
    # vac4 - how important vaccine is for health
    upset_df <- upset_df %>% mutate(vac4_ag = case_when(
      vac4 >= (strength-1) ~ as.integer(1),
      TRUE ~ as.integer(0)
    ))
    
    # vac5 - will get vaccine if available
    upset_df <- upset_df %>% mutate(vac5_ag = case_when(
      vac5 == 1 ~ as.integer(1),   
      TRUE ~ as.integer(0)
    ))
    
    # vac6 - will get vaccine if available
    upset_df <- upset_df %>% mutate(vac6_ag = case_when(
      vac6 == 1 ~ as.integer(1),  
      TRUE ~ as.integer(0)
    ))
    
    # vac7 - trust COVID19 vaccines
    upset_df <- upset_df %>% mutate(vac7_ag = case_when(
      vac7 >= (strength-1) ~ as.integer(1),
      TRUE ~ as.integer(0)
    ))
    # create combination matrix
    
    combiMatrix <- select(upset_df,vac_1_ag,vac2_1_ag,vac2_2_ag,
                          vac2_3_ag,vac2_4_ag,vac2_5_ag,
                          vac2_6_ag,vac_3_ag,vac4_ag,
                          vac5_ag,vac6_ag,vac7_ag,)
    
    combiMatrix <- filter(combiMatrix, vac_1_ag == 1) 
    # UpSetR cannot have 0 length argument (i.e. 0 0 0 0 0) so investigate only vac_1 = 1 cases
    upset(combiMatrix, sets = c("vac_1_ag","vac2_1_ag","vac2_2_ag",
                                    "vac2_3_ag","vac2_4_ag","vac2_5_ag",          
                                    "vac2_6_ag","vac_3_ag","vac4_ag",
                                    "vac5_ag","vac6_ag","vac7_ag"),
              mb.ratio = c(0.55,0.45), order.by = "freq")
  
  })
  
  
  


}

shinyApp(ui,server)






