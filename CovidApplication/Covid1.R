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
          
             
             
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                   
                   ##############################
                   # Side bar for Bivariate tab #
                   ##############################
                   condition = "input.tabs_name == 'tab_b'",
                   
                   h4("Plot Parameters"),
                   
                   #####################
                   # Select y variable #
                   #####################
                   
                   selectInput("yVariable_b", 
                               h5("Y-variable"),
                               #data = death_df,
                               c("Cumulative death" = "total_deaths", "Fatality rate" = "rate"),
                               selected = "total_deaths"
                   ), # End selectInput
                   
                   #################################################
                   # conditionalPanel for y variable: total_deaths #
                   #################################################
                   
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_b' && input.yVariable_b == 'total_deaths'",
                     ns = NS(NULL),
                     
                     selectInput("xVariable_bc",
                                 h5("X-variable:"), 
                                 #data = death_df,
                                 list(
                                   'COVID related' = list(
                                     "Cumulative positive cases" = "total_cases",
                                     "Cumulative tests conducted" = "total_tests",
                                     "Positive rate" = "positive_rate"),
                                   'Healthcare' = list(
                                     "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                     "Physicians (per thousand)" = "num_physicians_per_thousand",
                                     "Handwashing facilities" = "handwashing_facilities"),
                                   'Country indicators' = list(
                                     "Current health expenditure (% of GDP)" = "current_health_exp_%gdp",
                                     "% population living in extreme poverty" = "extreme_poverty",
                                     "GDP per capita" = "gdp_per_capita",
                                     "Government health expenditure (% of total government expenditure)" = "govt_health_exp_%totalgovtexp",
                                     "Human development index (HDI)" = "human_development_index"),
                                   'Population' = list(
                                     "Total population" = "population",
                                     "Population aged 0 to 14" = "0_to_14(%)",
                                     "Population aged 15 to 64" = "15_to_64(%)",
                                     "Population aged 65 and above" = "65_and_above(%)",
                                     "Population density" = "population_density"),
                                   'Others' = list(
                                     "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                 ) # End list
                     ), # End selectInput
                     
                     checkboxInput("use.log.bc",
                                   h5("Use log values"),
                                   value = TRUE
                     ),
                     
                     hr(),
                     
                     h4("Plot options"),
                     checkboxInput("showStatTest",
                                   "Show statistical test",
                                   value = TRUE),
                     
                     conditionalPanel(
                       condition = "input.showStatTest == 1",
                       ns = NS(NULL),
                       
                       selectInput("type",
                                   h5("Statistical test:"),
                                   c("Bayesian", "Non-parametrtic", "Parametric", "Robust")
                       )
                       
                     ), # End conditionalPanel for show statistical test
                     
                     selectInput("marginal.type",
                                 h5("Marginal distribution:"),
                                 c("Boxplot" = "boxplot", "Densigram" = "densigram", 
                                   "Density" = "density", "Histogram" = "histogram",
                                   "Violin plot"= "violin")
                     ),
                     
                     selectInput("gg.regression.method",
                                 h5("Regression line:"),
                                 c("Generalised Additive Model (GAM)" = "gam",
                                   "General Linear Model (GLM)" = "glm",
                                   "Linear Model" = "lm", 
                                   "Loess" = "loess")
                     ),
                     
                     selectInput("conf.level",
                                 h5("Confidence level for regression line:"),
                                 c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"),
                                 selected = "0.95"
                     )
                     
                   ), # End conditionalPanel for y variable: cumDeath
                   
                   
                   #########################################
                   # conditionalPanel for y variable: rate #
                   #########################################
                   
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_b' && input.yVariable_b == 'rate'",
                     ns = NS(NULL),
                     
                     selectInput("xVariable_br",
                                 h5("X-variable:"), 
                                 list(
                                   'COVID related' = list(
                                     "Log of cumulative positive cases" = "total_cases_log",
                                     "Cumulative tests conducted" = "total_tests"),
                                   'Healthcare' = list(
                                     "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                     "Physicians (per thousand)" = "num_physicians_per_thousand",
                                     "Handwashing facilities" = "handwashing_facilities"),
                                   'Population' = list(
                                     "Total population" = "population")
                                 ) # End list
                     ), # End selectInput
                     
                     hr(),
                     
                     h4("Plot options"),
                     
                     checkboxGroupInput("conf.range",
                                        h5("Display limits for confidence level:"),
                                        choices = list(
                                          "90%" = 90,
                                          "95%" = 95,
                                          "99%" = 99,
                                          "99.9%" = 99.9
                                        )
                     ) # End checkboxGroupInput
                     
                   ) # End conditionalPanel for y variable: rate
                   
                   
                 ), # End conditionalPanel for bivariate tab
                 
                 #################################
                 # Side bar for Multivariate tab #
                 #################################
                 conditionalPanel(
                   condition = "input.tabs_name == 'tab_m'",
                   
                   h4("Regression Model Parameters"),
                   
                   #####################
                   # Select y variable #
                   #####################
                   selectInput("yVariable_m", 
                               h5("Y-variable"),
                               c("Cumulative death" = "total_deaths", "Case fatality rate" = "caseFatalityRate")
                   ), # End selectInput
                   
                   #################################################
                   # conditionalPanel for y variable: total_deaths #
                   #################################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.yVariable_m == 'total_deaths'",
                     ns = NS(NULL),
                     
                     selectizeInput("xVariable_mc",
                                    h5("X-variable:"), 
                                    list(
                                      'COVID related' = list(
                                        "Cumulative positive cases" = "total_cases",
                                        "Cumulative tests conducted" = "total_tests",
                                        "Positive rate" = "positive_rate"),
                                      'Healthcare' = list(
                                        "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                        "Physicians (per thousand)" = "num_physicians_per_thousand",
                                        "Handwashing facilities" = "handwashing_facilities"),
                                      'Country indicators' = list(
                                        "Current health expenditure (% of GDP)" = "current_health_exp_%gdp",
                                        "% population living in extreme poverty" = "extreme_poverty",
                                        "GDP per capita" = "gdp_per_capita",
                                        "Government health expenditure (% of total government expenditure)" = "govt_health_exp_%totalgovtexp",
                                        "Human development index (HDI)" = "human_development_index"),
                                      'Population' = list(
                                        "Total population" = "population",
                                        "Population aged 0 to 14" = "0_to_14(%)",
                                        "Population aged 15 to 64" = "15_to_64(%)",
                                        "Population aged 65 and above" = "65_and_above(%)",
                                        "Population density" = "population_density"),
                                      'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                    ), # End list
                                    multiple = TRUE,
                                    options = list(
                                      plugins = list("remove_button")
                                    )
                     ) # End selectizeInput
                     
                   ), # End conditionalPanel for y variable: total_deaths
                   
                   #########################################
                   # conditionalPanel for y variable: rate #
                   #########################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.yVariable_m == 'caseFatalityRate'",
                     ns = NS(NULL),
                     
                     selectizeInput("xVariable_mr",
                                    h5("X-variable:"), 
                                    list(
                                      'COVID related' = list(
                                        "Cumulative positive cases" = "total_cases",
                                        "Cumulative deaths" = "total_deaths",
                                        "Cumulative tests conducted" = "total_tests",
                                        "Positive rate" = "positive_rate"),
                                      'Healthcare' = list(
                                        "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                        "Physicians (per thousand)" = "num_physicians_per_thousand",
                                        "Handwashing facilities" = "handwashing_facilities"),
                                      'Country indicators' = list(
                                        "Current health expenditure (% of GDP)" = "current_health_exp_%gdp",
                                        "% population living in extreme poverty" = "extreme_poverty",
                                        "GDP per capita" = "gdp_per_capita",
                                        "Government health expenditure (% of total government expenditure)" = "govt_health_exp_%totalgovtexp",
                                        "Human development index (HDI)" = "human_development_index"),
                                      'Population' = list(
                                        "Total population" = "population",
                                        "Population aged 0 to 14" = "0_to_14(%)",
                                        "Population aged 15 to 64" = "15_to_64(%)",
                                        "Population aged 65 and above" = "65_and_above(%)",
                                        "Population density" = "population_density"),
                                      'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                    ), # End list
                                    multiple = TRUE,
                                    options = list(
                                      plugins = list("remove_button")
                                    )
                     ) # End selectInput
                   ), # End conditionalPanel for y variable: rate
                   
                   hr(),
                   ###########################
                   # Select regression model #
                   ###########################
                   
                   selectInput("regressionModel", 
                               h5("Method to build regression model:"),
                               c("Least squares method" = "lsm", "Variable selection method" = "vsm")
                   ), # End selectInput
                   
                   #########################################################
                   # conditionalPanel for regression method: least squares #
                   #########################################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.regressionModel == 'lsm'",
                     ns = NS(NULL),
                     
                     checkboxInput("iterm", # TO UPDATE TO ITERM
                                   "Model includes interaction terms",
                                   value = FALSE)
                     
                   ), # End conditionalPanel for regression method: least squares
                   
                   ##############################################################
                   # conditionalPanel for regression method: variable selection #
                   ##############################################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.regressionModel == 'vsm'",
                     ns = NS(NULL),
                     
                     selectInput("vsMethod",
                                 h5("Variable selection methods:"),
                                 c("All possible regression" = "all_possible",
                                   "Best subset regression" = "best_subset",
                                   "Stepwise forward regression (p-value)" = "forward_p",
                                   "Stepwise backward regression (p-value)" = "backward_p",
                                   "Stepwise regression (p-value)" = "both_p",
                                   "Stepwise forward regression (AIC)" = "forward_aic",
                                   "Stepwise backward regression (AIC)" = "backward_aic",
                                   "Stepwise regression (AIC)" = "both_aic")
                     ), # End selectInput
                     
                     ################################################
                     # conditionalPanel for vsMethods with p-values #
                     ################################################
                     conditionalPanel(
                       condition = "input.vsMethod == 'forward_p' 
                                    | input.vsMethod == 'backward_p' 
                                    | input.vsMethod == 'both_p'",
                       ns = NS(NULL),
                       
                       numericInput("prem", # TO UPDATE TO PREM
                                    h5("Threshold for p-value:"),
                                    value = 0.05,
                                    min = 0,
                                    max = 1,
                                    step = 0.005
                       ) # End numericInput
                     ), # End conditionalPanel for vsm with p-values
                     
                     h5("Output options:"),
                     checkboxInput("showStep", # TO UPDATE TO PROGRESS
                                   "Show detailed steps",
                                   value = FALSE),
                     
                     checkboxInput("plotResults",
                                   "Plot model summary results",
                                   value = FALSE),
                     
                     checkboxInput("plotDiagnostics",
                                   "Plot model diagnostics",
                                   value = FALSE)
                     
                   ) # End conditionalPanel for regression method: variable selection
                   
                 ) # End conditionalPanel for multivariate tab
                 
               ), # End sidebarPanel
               
               mainPanel(
                 tabsetPanel(
                   type = "tabs", id = "tabs_name",
                   
                   #################
                   # Bivariate Tab #
                   #################
                   tabPanel( 
                     "Bivariate Analysis", value = "tab_b",
                     
                     conditionalPanel(
                       condition = "input.tabs_name == 'tab_b' && input.yVariable_b == 'total_deaths'",
                       ns = NS(NULL),
                       
                       hr(),
                       
                       selectInput("selectedContinent",
                                   "Display label for:",
                                   c("All" = "All",
                                     "Africa" = "Africa",
                                     "Asia" = "Asia",
                                     "Europe" = "Europe",
                                     "North America" = "North America",
                                     "Oceania" = "Oceania",
                                     "South America" = "South America"),
                                   selected = "All"
                       ), # End selectInput
                       
                       hr(),
                       
                       plotOutput("scatterplot")
                       
                     ), # End conditionalPanel
                     
                     conditionalPanel(
                       condition = "input.tabs_name == 'tab_b' && input.yVariable_b == 'rate'",
                       ns = NS(NULL),
                       
                       h3("Funnel plot with marginal distribution"),
                       
                       plotlyOutput("funnelplot")
                       
                     ) # End conditionalPanel
                     
                     
                   ), # Close tabPanel Bivariate
                   
                   
                   ####################
                   # Multivariate Tab #
                   ####################
                   tabPanel(
                     "Multivariate Analysis", value = "tab_m",
                     
                     h4("Regression Model"),
                     
                     conditionalPanel(
                       condition = "input.tabs_name == 'tab_m' && input.regressionModel == 'lsm'",
                       ns = NS(NULL),
                       
                       h5("Model Output:"),
                       verbatimTextOutput("lsrResultsText")
                       
                     ), # End conditionalPanel for least squares
                     
                     conditionalPanel(
                       condition = "input.tabs_name == 'tab_m' && input.regressionModel == 'vsm'",
                       ns = NS(NULL),
                       
                       h5("Model Output:"),
                       verbatimTextOutput("VSMResultsText"),
                       
                       conditionalPanel(
                         condition = "input.plotResults == 1",
                         ns = NS(NULL),
                         
                         plotlyOutput("vsmResultsPlot")
                         
                       ), # End conditionalPanel for plotResults
                       
                       conditionalPanel(
                         condition = "input.plotDiagnostics == 1",
                         ns = NS(NULL),
                         
                         plotOutput("BaseDiagnosticsPlot")
                         
                       ) # End conditionalPanel for plotDiagnostics
                       
                       
                       
                     ) # End conditionalPanel for variable selection
                   ) # Close tabpanel Multivariate
                 ) # Close tabset tabs_names
               ) # End mainPanel
             )  # End sidebarLayout
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
             
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
  
  
  
  
  
  
  output$scatterplot <- renderPlot({
    
    
    
    if (input$selectedContinent == "All") {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05)), # regression line
                     label.var = location,
                     # NO CRITERIA TO DISPLAY BY WHICH CONTINENT
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     title = "Scatterplot with marginal distribution")
    } else if (input$selectedContinent == "Africa") {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05)), # regression line
                     label.var = location,
                     label.expression = continent == "Africa", # criteria to display labels
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     title = "Scatterplot with marginal distribution")
    } else if (input$selectedContinent == "Asia") {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05)), # regression line
                     label.var = location,
                     label.expression = continent == "Asia", # criteria to display labels
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     title = "Scatterplot with marginal distribution")
    } else if (input$selectedContinent == "Europe") {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05)), # regression line
                     label.var = location,
                     label.expression = continent == "Europe", # criteria to display labels
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     title = "Scatterplot with marginal distribution")
    } else if (input$selectedContinent == "North America") {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05)), # regression line
                     label.var = location,
                     label.expression = continent == "North America", # criteria to display labels
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     title = "Scatterplot with marginal distribution")
    } else if (input$selectedContinent == "Oceania") {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05)), # regression line
                     label.var = location,
                     label.expression = continent == "Oceania", # criteria to display labels
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     title = "Scatterplot with marginal distribution")
    } else if (input$selectedContinent == "South America") {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05)), # regression line
                     label.var = location,
                     label.expression = continent == "South America", # criteria to display labels
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     title = "Scatterplot with marginal distribution")
    }
    
  }) # End renderPlot for scatterplot
  
  output$funnelplot <- renderPlotly({
    
    rate <- death_df$case_fatality_rate # TO UPDATE
    number <- death_df$total_cases_log # TO UPDATE
    
    rate.se <- sqrt((rate*(1-rate)) / (number))
    df <- data.frame(rate, number, rate.se)
    
    rate.fem <- weighted.mean(rate, 1/rate.se^2)
    
    # Calculate lower and upper limits for 95% and 99.9% CI
    number.seq <- seq(1, max(number), 1)
    number.ll90 <- rate.fem - 1.645 * sqrt((rate.fem*(1-rate.fem)) / (number.seq)) 
    number.ul90 <- rate.fem + 1.645 * sqrt((rate.fem*(1-rate.fem)) / (number.seq)) 
    number.ll95 <- rate.fem - 1.96 * sqrt((rate.fem*(1-rate.fem)) / (number.seq)) 
    number.ul95 <- rate.fem + 1.96 * sqrt((rate.fem*(1-rate.fem)) / (number.seq))
    number.ll99 <- rate.fem - 2.576 * sqrt((rate.fem*(1-rate.fem)) / (number.seq))
    number.ul99 <- rate.fem + 2.576 * sqrt((rate.fem*(1-rate.fem)) / (number.seq))
    number.ll999 <- rate.fem - 3.29 * sqrt((rate.fem*(1-rate.fem)) / (number.seq)) 
    number.ul999 <- rate.fem + 3.29 * sqrt((rate.fem*(1-rate.fem)) / (number.seq)) 
    dfCI <- data.frame(number.ll90, number.ul90,
                       number.ll95, number.ul95,
                       number.ll99, number.ul99,
                       number.ll999, number.ul999,
                       number.seq, rate.fem)
    
    # Draw funnel plot
    
    fp_ggplot <- ggplot(death_df, 
                        aes(x = total_cases_log, y = case_fatality_rate)) + # TO UPDATE ACCORDINGLY
      geom_point(aes(colour=continent,
                     size=population,
                     label=location),
                 alpha=0.4) +
      geom_hline(data = dfCI, aes(yintercept = rate.fem), 
                 size = 0.4, colour = "grey40") +
      xlab("Total Number of COVID-19 Cases - log") + # TO UPDATE
      ylab("Case Fatality Rate") + # TO UPDATE
      theme_light() +
      theme(legend.title = element_blank(),
            legend.text = element_text(size=7)) +
      scale_size_continuous(guide = FALSE) +
      
      geom_line(data = dfCI, aes(x = number.seq, y = number.ll90), 
                size = 0.4, colour = "grey40", linetype = "dotted") +
      geom_line(data = dfCI, aes(x = number.seq, y = number.ul90), 
                size = 0.4, colour = "grey40", linetype = "dotted") +
      annotate("text", x = 0.3, y = -0.15, label = "90%", size = 2, colour = "grey40") +
      
      geom_line(data = dfCI, aes(x = number.seq, y = number.ll95), 
                size = 0.4, colour = "grey40", linetype = "dashed") +
      geom_line(data = dfCI, aes(x = number.seq, y = number.ul95), 
                size = 0.4, colour = "grey40", linetype = "dashed") +
      annotate("text", x = 0.3, y = -0.19, label = "95%", size = 2, colour = "grey40") +
      
      geom_line(data = dfCI, aes(x = number.seq, y = number.ll99), 
                size = 0.4, colour = "grey40", linetype = "twodash") +
      geom_line(data = dfCI, aes(x = number.seq, y = number.ul99), 
                size = 0.4, colour = "grey40", linetype = "twodash") +
      annotate("text", x = 0.3, y = -0.25, label = "99%", size = 2, colour = "grey40") +
      
      geom_line(data = dfCI, aes(x = number.seq, y = number.ll999), 
                size = 0.4, colour = "grey40") +
      geom_line(data = dfCI, aes(x = number.seq, y = number.ul999), 
                size = 0.4, colour = "grey40") +
      annotate("text", x = 0.3, y = -0.33, label = "99.9%", size = 2, colour = "grey40")
    
    fp_densigram <- ggplot(death_df, aes(x = case_fatality_rate)) + # TO UPDATE
      geom_histogram(bins = 40, fill="orange", colour="grey40", size=0.2) + 
      geom_density(colour="grey40", size=0.2) +
      coord_flip(clip = "off") +
      ggtitle("Distribution") +
      theme_void() +
      theme(axis.line = element_line(colour = "white"),
            panel.grid = element_line(colour = "white"))
    
    subplot(fp_ggplot, fp_densigram,
            nrows = 1,
            widths = c(4/5,1/5),
            shareY = TRUE,
            which_layout = 1)
  }) # End renderPlotly for funnelplot
  
  output$lsrResultsText <- renderPrint({
    model_lsr <- ols_regress(total_deaths ~ total_cases + positive_rate + hospital_beds_per_thousand,
                             data = death_df,# TO CHANGE Y AND X VARIABLES
                             iterm = FALSE) # TO CHANGE ACCORDINGLY
    
    model_lsr
  }) # End renderPrint for lsr results
  
  
  output$VSMResultsText <- renderPrint({
    
    model <- lm(total_deaths ~ total_cases + positive_rate + hospital_beds_per_thousand + gdp_per_capita, 
                data = death_df) # TO CHANGE Y AND X VARIABLES
    
    model_vsm <- ols_step_both_p(model, # TO CHANGE VSM SELECTED
                                 prem = input$prem,
                                 progress = input$showStep)
    
    model_vsm
    
  }) # End renderPrint for vsmResultsText
  
  # NOT SURE IF SHOULD PUT INSIDE
  output$vsmResultsPlotly <- renderPlotly({
    
    model <- lm(total_deaths ~ total_cases + positive_rate + hospital_beds_per_thousand + gdp_per_capita, 
                data = death_df) # TO CHANGE Y AND X VARIABLES
    
    model_vsm <- ols_step_both_p(model, # TO CHANGE VSM SELECTED
                                 prem = input$prem,
                                 progress = input$showStep)
    
    plot(model_vsm)
    
    
  }) # End renderPlot for vsmResultsPlot
  
  output$BaseDiagnosticsPlot <- renderPlot({
    
    model <- lm(total_deaths ~ total_cases + positive_rate + hospital_beds_per_thousand + gdp_per_capita, 
                data = death_df) # TO CHANGE Y AND X VARIABLES
    
    ols_plot_diagnostics(model)
    ols_coll_diag(model)
  }) # End renderPlot for BaseDiagnosticsPlot  
  
  
  
  
  
  
  
  
}

shinyApp(ui,server)






