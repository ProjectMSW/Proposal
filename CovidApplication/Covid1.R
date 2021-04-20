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

library(dplyr)
library(HH)
library(UpSetR)
library(naniar)
library(dlookr)
library(ggridges)
library(forcats)






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
               csvFileUI("datafile", "Select File to Load or default dataset will be used"),
               forecastNavUI("datafile"),hr(),
               
               conditionalPanel(
                 condition = "input.tabs == 'Prediction'",
                 
                 checkboxGroupInput("variable1", "Model Selection",
                                    c("arima_boosted" ="arima_boosted", "ets" = "ets", "prophet"="prophet",
                                      "lm" = "lm", "mars" = "mars", "snaive" ="snaive", "ETS"="ETS")),
                 actionButton("ModelgoButton", label = "Go"),
               )
               
               
               
             ),
             mainPanel(
               tabsetPanel(id="tabs",
                 #tabPanel("Explore Data",plotlyOutput("distPlot")),
                 tabPanel("Raw Data", 
                          textOutput("initialtext"),hr(),
                          dataTableOutput("table") ),
                 tabPanel("Explore Country Data",
                          textOutput("initialtext1"),
                          plotlyOutput("distPlot"),
                          #fluidRow(
                          #  column(5, offset = 1,
                                plotlyOutput("anomalyPlot"),
                          #  ),
                         #   column(5,
                                plotlyOutput("acfPlot"),
                        #   )
                        #  ),
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
               surveysideUI("daniel")
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Survey Finding", plotlyOutput("likertchart") ),
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
  
  
  
  output$initialdisplay <- renderText({
    if(mypredictvalue$default == 0){
    "Please Select a model"
    }else{
    ""
  }
   # mypredictvalue$default <- 1
    
    })
  
 
  
 
  
  
  

  modelselection <- eventReactive(input$ModelgoButton, {
    isolate(input$variable1)
  })

  
  output$predictive <- renderPlotly({
   
    
    #modelselection <- input$variable1
    if(length(modelselection()) == 0){
      print("haha. this is working in predict")
      modelselection <- "arima_no_boost"
    }
    
    if(is.null(modelselection())){
      print("haha. this is working in null")
      modelselection <- "arima_no_boost"
    }
    
    
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
    }
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- PredictionModel(traindata,modelselection())
    
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
    modelselection <- input$variable1
    if(length(modelselection) == 0){
      print("haha. this is working")
      modelselection <- "arima_no_boost"
    }
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
    }
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- PredictionModel(traindata,modelselection)
    
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
  
  
  
  output$likertchart <- renderPlotly({
    
    data <- loaddataset()
    cleandata <- cleandataset(data)
    
    
    # Make the table long - Gather question data from columns to rows
    main_gathered <- gather(cleandata, measure, response, c(6:17))
    
    # Obtain dataset with only responses for selected question
    
    qn_selected <- "vac_1"    # variable of question selected
    
    vac_1 <- filter(main_gathered, measure == qn_selected)
    
    # Obtain contingency table for vac_1
    vac_1_df <- table(vac_1$country,vac_1$response) %>% as.data.frame.matrix()
    # Change column names, now labelled as 1-5
    
    colnames(vac_1_df) <- c("Strongly Disagree",
                            "Disagree",
                            "Neutral",
                            "Agree",
                            "Strongly Agree")
    
    rownames(vac_1_df) <- c("Australia","Canada","Denmark",
                            "Finland","France","Germany",
                            "Israel","Italy","Japan",
                            "Netherlands","Norway","Singapore",
                            "South Korea","Spain","Sweden",
                            "United Kingdom","United States")
    
    # Remove other columns containing other responses (for other questions)
    vac_1_df <- vac_1_df[,c(1:5)]
    
    # Add a column with rownames
    vac_1_df <- tibble::rownames_to_column(vac_1_df, var="Country")
    
    if (qn_selected == "vac_1") {
      div_chart_title <- "Proportion who are willing to take vaccine"
    } else if (qn_selected == "vac2_1") {
      div_chart_title <- "Proportion worried about getting COVID-19"
    } else if (qn_selected == "vac2_2") {
      div_chart_title <- "Proportion worried about side effects of COVID-19 vaccines"
    } else if (qn_selected == "vac2_3") {
      div_chart_title <- "Proportion confident government will provide effective COVID-19 vaccines"
    } else if (qn_selected == "vac2_4") {
      div_chart_title <- "Proportion confident vaccine will completely protect recipients from health effects of COVID-19"
    } else if (qn_selected == "vac2_5") {
      div_chart_title <- "Proportion confident vaccine will completely prevent transmission of COVID-19 from recipient to others"
    } else if (qn_selected == "vac2_6") {
      div_chart_title <- "Proportion who feel they will regret if they do not take the vaccine"
    } else if (qn_selected == "vac_3") {
      div_chart_title <- "Proportion who will take the vaccine if available in 1 year"
    }
    
    
   likert(Country ~ .,data = vac_1_df, ylab = NULL,
                RefernceZero = 3, as.percent=TRUE,
                positive.order=TRUE,
                main = list(div_chart_title, 
                            x =unit(.55,"npc")),
                sub = list("Response", x=unit(.57,"npc")),
                xlim = c(-100,-80,-60,-40,-20,0,20,40,60,80,100),
                strip = FALSE,
                par.strip.text=list(cex=.7)
    )
  
    
  })
  
  
}





shinyApp(ui,server)






