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
library(earth)
library(reactable)

library(UpSetR)
library(dlookr)
library(naniar)


library(ggstatsplot)
library(ggExtra)
library(olsrr)
library(gridExtra)

library(shinyjs)

import::from(naniar,replace_with_na)


source("Modules/forecastNav.R")
source("Modules/forecastWorker.R")
source("Modules/survey.R")



ui <- fluidPage(
  titlePanel(""),
  
  shinyjs::useShinyjs(),
  
  navbarPage(
    theme = shinytheme("sandstone"),
    "COVID EXPLORER",
    tabPanel("Forecasting Positive Cases",
             sidebarPanel(
               h4("Dataset"),
               csvFileUI("datafile", h5("Select File to Load (or default dataset will be used)")),hr(),
               conditionalPanel(
                 condition = "input.tabs == 'Explore Country Data' || input.tabs == 'Prediction' ",
                 forecastNavUI("datafile"),hr(),
               ),
               
               conditionalPanel(
                 condition = "input.tabs == 'Prediction'",
                 
                 checkboxGroupInput("variable1", h5("Model Selection - (Default Parameters)"),
                                    c("Arima"="arima","Arima_Boosted" ="arima_boosted", "Exponential Smoothing (ETS)" = "ets", "Prophet"="prophet",
                                      "Linear Regression" = "lm", "MARS" = "mars", "SNAIVE" ="snaive"),selected =c("arima")),
                
                 actionButton("ModelgoButton", label = "Go"),hr(),
                 a(id = "toggleAdvanced", h5("Advance - Model Parameters Selection"), href = "#"),
                 shinyjs::hidden(
                   div(id = "advanced",
                       checkboxInput("variable4", "Exponential Smoothing (ETS)",value=FALSE),
                       checkboxInput("variable5", "Prophet",value=FALSE)
                   )
                 ),
               )
             ),
             mainPanel(
               tabsetPanel(id="tabs",
                          
                           tabPanel("Explore Country Data",
                                    br(),
                                    span( textOutput("initialtext1"), style="font-size:18px"),
                                    hr(),
                                    
                                    h4("Visualising the time series data"),br(),
                                    shinycssloaders::withSpinner(plotlyOutput("distPlot")),
                                    hr(),
                                    h4("View anomalies in the data"),br(),
                                    shinycssloaders::withSpinner(plotlyOutput("anomalyPlot")),
                                    hr(),
                                    h4("Detecting Autocorrelation (ACF) and
                                    Partial Autocorrelation (PACF) of Lagged Predictors"),br(),
                                    shinycssloaders::withSpinner(plotlyOutput("acfPlot")),
                                    hr(),
                                    h4("Visualizing time series STL Decomposition"),br(),
                                    shinycssloaders::withSpinner(plotlyOutput("stlPlot")),
                                    br(),
                                    br()
                                    
                           ),
                           tabPanel("Prediction",
                              hr(),
                              tabsetPanel(id="tabs1",
                                tabPanel("Model Comparison",
                                    conditionalPanel(
                                      condition = "input.tabs == 'Prediction'",
                                      textOutput("initialdisplay")
                                    ),
                                    flowLayout(
                                    selectInput("dayselection", h5('Forecast Horizon'), choices =
                                                  c("60 days"="60 days",
                                                    "50 days"="50 days",
                                                    "40 days" = "40 days",
                                                    "30 days" = "30 days",
                                                    "20 days" = "20 days",
                                                    "10 days" = "10 days"),
                                                width = '100%'),
                                    
                                    dateRangeInput("date_range", h5("Change dataset Date Range:"),
                                                   start = "1-23-2020", # Start date of the selected df
                                                   end = getMyDate(confirmed_cases_raw), # End date of the selected df
                                                   format = "m-d-yyyy")
                                    ),
                                    h4("Time Series Forecast"),
                                    shinycssloaders::withSpinner(plotlyOutput("predictive")),
                                    hr(),
                                    h4("Model Assessment Metrics"),
                                    shinycssloaders::withSpinner(reactableOutput("accuracy")),
                                    br(),
                                    br()
                                )
                                )      
                                     
                           ),
                           tabPanel("Data Table", 
                                    span( textOutput("initialtext"), style="font-size:18px"),hr(),
                                    dataTableOutput("table") )
                           
               )
             )
    ),
    tabPanel("Exploring Death",
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
                               c("Total death" = "total_deaths", 
                                 "Log(total death)" = "total_deaths_log",
                                 "Fatality rate" = "rate"),
                               selected = "total_deaths_log"
                   ), # End selectInput
                   
                   #################################################
                   # conditionalPanel for y variable: total_deaths #
                   #################################################
                   
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_b' && (input.yVariable_b == 'total_deaths'|| input.yVariable_b == 'total_deaths_log')",
                     ns = NS(NULL),
                     
                     selectInput("xVariable_bc",
                                 h5("X-variable:"),
                                 list(
                                   'COVID related' = list(
                                     "Total positive cases" = "total_cases",
                                     "Total tests conducted" = "total_tests",
                                     "Log(total positive cases)" = "total_cases_log",
                                     "Log(total tests conducted)" = "total_tests_log",
                                     "Positive rate" = "positive_rate"),
                                   'Healthcare' = list(
                                     "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                     "Physicians (per thousand)" = "num_physicians_per_thousand",
                                     "Handwashing facilities" = "handwashing_facilities"),
                                   'Country indicators' = list(
                                     "Current health expenditure (% of GDP)" = "current_health_exp",
                                     "% population living in extreme poverty" = "extreme_poverty",
                                     "GDP per capita" = "gdp_per_capita",
                                     "Government health expenditure (% of total govt. exp.)" = "govt_health_exp",
                                     "Human development index (HDI)" = "human_development_index",
                                     "Log(GDP per capita)" = "gdp_per_capita_log"),
                                   'Population' = list(
                                     "Total population" = "population",
                                     "Population - young (aged 0 to 14)" = "pop_young",
                                     "Population - working (aged 15 to 64)" = "pop_working",
                                     "Population - old (aged 65 and above)" = "pop_old",
                                     "Population density" = "population_density",
                                     "Log(Total population)" = "population_log",
                                     "Log(Population density)" = "population_density_log"),
                                   'Others' = list(
                                     "Annual international arrivals" = "annual_intl_arrivals_thousands",
                                     "Log(Annual international arrivals)" = "annual_intl_arrivals_thou_log")
                                 ),
                                 selected = "total_cases_log" # End list
                     ), # End selectInput
                     
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
                       ),
                       
                       selectInput("conf.level",
                                   h5("Confidence level for statistical test:"),
                                   c("90%" = "0.90", "95%" = "0.95", "99%" = "0.99"),
                                   selected = "0.95"
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
                                   "Loess" = "loess",
                                   "None" = "NULL"),
                                 selected = "lm"
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
                                     "Total positive cases" = "total_cases",
                                     "Total tests conducted" = "total_tests"),
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
                                        ),
                                        selected = c(95, 99.9)
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
                               c("Total death" = "total_deaths", 
                                 "Case fatality rate" = "case_fatality_rate",
                                 "Death rate (tests conducted)" = "deaths_per_test",
                                 "Death rate (population)" = "deaths_per_population"
                               )
                   ), # End selectInput
                   
                   #################################################
                   # conditionalPanel for y variable: total_deaths #
                   #################################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.yVariable_m == 'total_deaths'",
                     ns = NS(NULL),
                     
                     selectizeInput("xVariable_m_td",
                                    h5("X-variable:"), 
                                    list(
                                      'COVID related' = list(
                                        "Total positive cases" = "total_cases",
                                        "Total tests conducted" = "total_tests",
                                        "Positive rate" = "positive_rate"),
                                      'Healthcare' = list(
                                        "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                        "Physicians (per thousand)" = "num_physicians_per_thousand",
                                        "Handwashing facilities" = "handwashing_facilities"),
                                      'Country indicators' = list(
                                        "Current health expenditure (% of GDP)" = "current_health_exp",
                                        "% population living in extreme poverty" = "extreme_poverty",
                                        "GDP per capita" = "gdp_per_capita",
                                        "Government health expenditure (% of total government expenditure)" = "govt_health_exp",
                                        "Human development index (HDI)" = "human_development_index"),
                                      'Population' = list(
                                        "Total population" = "population",
                                        "Population - young (aged 0 to 14)" = "pop_young",
                                        "Population - working (aged 15 to 64)" = "pop_working",
                                        "Population - old (aged 65 and above)" = "pop_old",
                                        "Population density" = "population_density"),
                                      'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                    ), # End list
                                    multiple = TRUE,
                                    selected = c("total_cases",
                                                 "hospital_beds_per_thousand",
                                                 "gdp_per_capita",
                                                 "pop_old"),
                                    options = list(
                                      plugins = list("remove_button")
                                    )
                     ) # End selectizeInput
                     
                   ), # End conditionalPanel for y variable: total_deaths
                   
                   #######################################################
                   # conditionalPanel for y variable: case fatality rate #
                   #######################################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.yVariable_m == 'case_fatality_rate'",
                     ns = NS(NULL),
                     
                     selectizeInput("xVariable_m_cfr",
                                    h5("X-variable:"), 
                                    list(
                                      'COVID related' = list(
                                        "Total tests conducted" = "total_tests",
                                        "Positive rate" = "positive_rate"),
                                      'Healthcare' = list(
                                        "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                        "Physicians (per thousand)" = "num_physicians_per_thousand",
                                        "Handwashing facilities" = "handwashing_facilities"),
                                      'Country indicators' = list(
                                        "Current health expenditure (% of GDP)" = "current_health_exp",
                                        "% population living in extreme poverty" = "extreme_poverty",
                                        "GDP per capita" = "gdp_per_capita",
                                        "Government health expenditure (% of total government expenditure)" = "govt_health_exp",
                                        "Human development index (HDI)" = "human_development_index"),
                                      'Population' = list(
                                        "Total population" = "population",
                                        "Population - young (aged 0 to 14)" = "pop_young",
                                        "Population - working (aged 15 to 64)" = "pop_working",
                                        "Population - old (aged 65 and above)" = "pop_old",
                                        "Population density" = "population_density"),
                                      'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                    ), # End list
                                    multiple = TRUE,
                                    selected = c("positive_rate",
                                                 "hospital_beds_per_thousand",
                                                 "gdp_per_capita",
                                                 "pop_old"),
                                    options = list(
                                      plugins = list("remove_button")
                                    )
                     ) # End selectInput
                   ), # End conditionalPanel for y variable: case fatality rate
                   
                   #################################################################
                   # conditionalPanel for y variable: death rate (tests conducted) #
                   #################################################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.yVariable_m == 'deaths_per_test'",
                     ns = NS(NULL),
                     
                     selectizeInput("xVariable_m_dpt",
                                    h5("X-variable:"), 
                                    list(
                                      'COVID related' = list(
                                        "Total positive cases" = "total_cases",
                                        "Positive rate" = "positive_rate"),
                                      'Healthcare' = list(
                                        "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                        "Physicians (per thousand)" = "num_physicians_per_thousand",
                                        "Handwashing facilities" = "handwashing_facilities"),
                                      'Country indicators' = list(
                                        "Current health expenditure (% of GDP)" = "current_health_exp",
                                        "% population living in extreme poverty" = "extreme_poverty",
                                        "GDP per capita" = "gdp_per_capita",
                                        "Government health expenditure (% of total government expenditure)" = "govt_health_exp",
                                        "Human development index (HDI)" = "human_development_index"),
                                      'Population' = list(
                                        "Total population" = "population",
                                        "Population - young (aged 0 to 14)" = "pop_young",
                                        "Population - working (aged 15 to 64)" = "pop_working",
                                        "Population - old (aged 65 and above)" = "pop_old",
                                        "Population density" = "population_density"),
                                      'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                    ), # End list
                                    multiple = TRUE,
                                    selected = c("total_cases",
                                                 "hospital_beds_per_thousand",
                                                 "gdp_per_capita",
                                                 "pop_old"),
                                    options = list(
                                      plugins = list("remove_button")
                                    )
                     ) # End selectInput
                   ), # End conditionalPanel for y variable: death rate (tests conducted)
                   
                   ############################################################
                   # conditionalPanel for y variable: death rate (population) #
                   ############################################################
                   conditionalPanel(
                     condition = "input.tabs_name == 'tab_m' && input.yVariable_m == 'deaths_per_population'",
                     ns = NS(NULL),
                     
                     selectizeInput("xVariable_m_dpp",
                                    h5("X-variable:"), 
                                    list(
                                      'COVID related' = list(
                                        "Total positive cases" = "total_cases",
                                        "Total tests conducted" = "total_tests",
                                        "Positive rate" = "positive_rate"),
                                      'Healthcare' = list(
                                        "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                        "Physicians (per thousand)" = "num_physicians_per_thousand",
                                        "Handwashing facilities" = "handwashing_facilities"),
                                      'Country indicators' = list(
                                        "Current health expenditure (% of GDP)" = "current_health_exp",
                                        "% population living in extreme poverty" = "extreme_poverty",
                                        "GDP per capita" = "gdp_per_capita",
                                        "Government health expenditure (% of total government expenditure)" = "govt_health_exp",
                                        "Human development index (HDI)" = "human_development_index"),
                                      'Population' = list(
                                        "Population - young (aged 0 to 14)" = "pop_young",
                                        "Population - working (aged 15 to 64)" = "pop_working",
                                        "Population - old (aged 65 and above)" = "pop_old",
                                        "Population density" = "population_density"),
                                      'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                    ), # End list
                                    multiple = TRUE,
                                    selected = c("total_cases",
                                                 "hospital_beds_per_thousand",
                                                 "gdp_per_capita",
                                                 "pop_old"),
                                    options = list(
                                      plugins = list("remove_button")
                                    )
                     ) # End selectInput
                   ), # End conditionalPanel for y variable: death rate (population)
                   tags$em(h6("(Select more variables if error is encountered)")),
                   
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
                                    || input.vsMethod == 'backward_p' 
                                    || input.vsMethod == 'both_p'",
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
                     
                     ##########################################################################
                     # conditionalPanel for vsMethods other than all possible and best subset #
                     ##########################################################################
                     conditionalPanel(
                       condition = "input.vsMethod == 'forward_p' 
                                    || input.vsMethod == 'backward_p' 
                                    || input.vsMethod == 'both_p'
                                    || input.vsMethod == 'forward_aic'
                                    || input.vsMethod == 'backward_aic' 
                                    || input.vsMethod == 'both_aic'",
                       ns = NS(NULL),
                       
                       checkboxInput("showStep", # TO UPDATE TO PROGRESS
                                     "Show selection process",
                                     value = FALSE)
                     ), # End conditionalPanel for vsm with p-values & aic
                     
                     ######################################
                     # conditionalPanel for vsMethods aic #
                     ######################################
                     conditionalPanel(
                       condition = "input.vsMethod == 'forward_aic'
                                    || input.vsMethod == 'backward_aic' 
                                    || input.vsMethod == 'both_aic'",
                       ns = NS(NULL),
                       
                       checkboxInput("plotResults",
                                     "Plot model summary results",
                                     value = FALSE)
                     ), # End conditionalPanel for vsm with aic
                     
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
                       condition = "input.tabs_name == 'tab_b' && 
                                    (input.yVariable_b == 'total_deaths' || input.yVariable_b == 'total_deaths_log')",
                       ns = NS(NULL),
                       
                       hr(),
                       
                       selectInput("selectedContinent",
                                   "Display label for:",
                                   c("None" = "None",
                                     "All" = "All",
                                     "Africa" = "Africa",
                                     "Asia" = "Asia",
                                     "Europe" = "Europe",
                                     "North America" = "North America",
                                     "Oceania" = "Oceania",
                                     "South America" = "South America"
                                   ),
                                   selected = "None"
                       ), # End selectInput
                       
                       hr(),
                       
                       h3("Scatterplot with marginal distribution"),
                       
                       shinycssloaders::withSpinner(plotOutput("scatterplot"))
                       
                     ), # End conditionalPanel
                     
                     conditionalPanel(
                       condition = "input.tabs_name == 'tab_b' && input.yVariable_b == 'rate'",
                       ns = NS(NULL),
                       
                       h3("Funnel plot with rate distribution"),
                       
                       shinycssloaders::withSpinner(plotlyOutput("funnelplot"))
                       
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
                       
                       h5("Model output:"),
                       verbatimTextOutput("lsrResultsText")
                       
                     ), # End conditionalPanel for least squares
                     
                     conditionalPanel(
                       condition = "input.tabs_name == 'tab_m' && input.regressionModel == 'vsm'",
                       ns = NS(NULL),
                       
                       h5("Model Output:"),
                       verbatimTextOutput("VSMResultsText"),
                       
                       conditionalPanel(
                         condition = "input.plotResults == 1 && 
                                        (input.vsMethod == 'forward_aic' 
                                            || input.vsMethod == 'backward_aic'
                                            || input.vsMethod == 'both_aic')",
                         ns = NS(NULL),
                         
                         shinycssloaders::withSpinner(plotOutput("VSMResultsPlot", height = 250, width = 500))
                         
                       ), # End conditionalPanel for plotResults
                       
                       conditionalPanel(
                         condition = "input.plotDiagnostics == 1",
                         ns = NS(NULL),
                         
                         h4("Model diagnostics:"),
                         h6("Model fit assessment and assumptions validation"),
                         shinycssloaders::withSpinner(plotOutput("BaseDiagnosticsPlot1", height = 200)),
                         shinycssloaders::withSpinner(plotOutput("BaseDiagnosticsPlot2", height = 200)),
                         hr(),
                         h6("Measures of influence"),
                         shinycssloaders::withSpinner(plotOutput("BaseDiagnosticsPlot3", height = 200)),
                         hr(),
                         h6("Collinearity"),
                         verbatimTextOutput("BaseCollResultsText")
                         
                       ) # End conditionalPanel for plotDiagnostics
                       
                     ) # End conditionalPanel for variable selection
                   ) # Close tabpanel Multivariate
                 ) # Close tabset tabs_names
               ) # End mainPanel
             )  # End sidebarLayout
    ),
    tabPanel("Understanding vaccination sentiments", 
             sidebarPanel(
               conditionalPanel(
                 condition = "input.danieltab == 'Survey Finding' ",
                 h4("View country responses to survey questions"),
                 selectInput("qn", 'Select Question', choices =
                               c("Proportion who are willing to take vaccine"="vac_1",
                                 "Proportion worried about getting COVID-19"="vac2_1",
                                 "Proportion worried about side effects of COVID-19 vaccines"="vac2_2",
                                 "Proportion confident government will provide effective COVID-19 vaccines"="vac2_3",
                                 "Proportion confident vaccine will completely protect recipients from health effects of COVID-19"="vac2_4",
                                 "Proportion confident vaccine will completely prevent transmission of COVID-19 from recipient to others"="vac2_5",
                                 "Proportion who feel they will regret if they do not take the vaccine"="vac2_6",
                                 "Proportion who will take the vaccine if available in 1 year"="vac_3"),
                             width = '100%')
                
                 
               ),
               
               
               
               
               
               conditionalPanel(
                 condition = "input.danieltab == 'Association of Factors' ",
                 h4("View correlation between factors by Country"),
               selectInput("countryofinterest", h5('Select Country'), choices = 
                             c("Australia" ="Australia","Canada" ="Canada","Denmark"="Denmark",
                               "Finland"="Finland","France"="France","Germany"="Germany",
                               "Israel"="Israel","Italy"="Italy","Japan"="Japan",
                               "Netherlands"="Netherlands","Norway"="Norway","Singapore"="Singapore",
                               "South Korea"="South Korea","Spain"="Spain","Sweden"="Sweden",
                               "United Kingdom"="United Kingdom","United States"="United States"),
                           width = '100%'),
               h4("Plot Options"),
               selectInput("strengthResponse", h5('Select level of agreement to be reflected in Upset plot'), choices = 
                             c("Strongly Agreed"="5", 
                               "Agreed"="4"),
                           width = '100%'),
                             
                           selectizeInput("factorofinterest",
                                          h5("Select factors of interest to view association (in blue) in Upset plot"), 
                                          list(
                                            "Willing to take vaccine" = "vac_1_ag",
                                            "Fear of getting COVID19" = "vac2_1_ag",
                                            "Concern on vaccine side effects" = "vac2_2_ag",
                                            "Confidence in vaccine efficacy" = "vac2_3_ag",
                                            "Vaccine will protect recipient from COVID19 health effects" = "vac2_4_ag",
                                            "Vaccine will prevent transmission of COVID from recipient" = "vac2_5_ag",          
                                            "Will regret if don't take vaccine" = "vac2_6_ag",
                                            "Will take vaccine 1 year later" = "vac_3_ag",
                                            "Vaccine is important for health" = "vac4_ag",
                                            "Will get vaccine if available" = "vac5_ag",
                                            "Family and friends will want respondent to be vaccinated" = "vac6_ag",
                                            "Trust COVID19 vaccines" = "vac7_ag"
                                          ), # End list
                                          multiple = TRUE, selected=c("vac_1_ag","vac2_1_ag")),  
               
               actionButton("DfactorButton", label = "Click to refresh plot")
                             
               ), 
               conditionalPanel(
                 condition = "input.danieltab == 'Data Exploration' ",
                 h4("Parameters"),
                 selectInput("countryofinterest1", h5('Select Country'), choices = 
                               c("Australia" ="Australia","Canada" ="Canada","Denmark"="Denmark",
                                 "Finland"="Finland","France"="France","Germany"="Germany",
                                 "Israel"="Israel","Italy"="Italy","Japan"="Japan",
                                 "Netherlands"="Netherlands","Norway"="Norway","Singapore"="Singapore",
                                 "South Korea"="South Korea","Spain"="Spain","Sweden"="Sweden",
                                 "United Kingdom"="United-Kingdom","United States"="United-States"),
                             width = '100%'),
                 h4("Plot Options"),
                 selectInput("targetVariable", 
                             h5('Select Target Variable (variable 1 (y-axis))'), 
                             choices = 
                               c("Gender"="gender", 
                                 "Household size"="household_size",
                                 "Household children"="household_children",
                                 "Willing to take vaccine"="vac_1",
                                 "Fear of getting COVID19"="vac2_1",
                                 "Concern on vaccine side effects"="vac2_2",
                                 "Confidence in vaccine efficacy"="vac2_3",
                                 "Vaccine will protect recipient from COVID19 health effects"="vac2_4",
                                 "Vaccine will prevent transmission of COVID from recipient"="vac2_5",
                                 "Will regret if don't take vaccine"="vac2_6",
                                 "Will take vaccine 1 year later"="vac_3",
                                 "Vaccine is important for health"="vac4",
                                 "Will get vaccine if available"="vac5",
                                 "Family and friends will want respondent to be vaccinated"="vac6",
                                 "Trust COVID19 vaccines"="vac7"),
                             width = '100%'),
                 
                 selectInput("predictiveVariable", 
                             h5('Select Predictive Variable (variable 2 (x-axis))'), 
                             choices = 
                               c("Age"="age", 
                                 "Gender"="gender", 
                                 "Household size"="household_size",
                                 "Household children"="household_children",
                                 "Willing to take vaccine"="vac_1",
                                 "Fear of getting COVID19"="vac2_1",
                                 "Concern on vaccine side effects"="vac2_2",
                                 "Confidence in vaccine efficacy"="vac2_3",
                                 "Vaccine will protect recipient from COVID19 health effects"="vac2_4",
                                 "Vaccine will prevent transmission of COVID from recipient"="vac2_5",
                                 "Will regret if don't take vaccine"="vac2_6",
                                 "Will take vaccine 1 year later"="vac_3",
                                 "Vaccine is important for health"="vac4",
                                 "Will get vaccine if available"="vac5",
                                 "Family and friends will want respondent to be vaccinated"="vac6",
                                 "Trust COVID19 vaccines"="vac7"),
                             width = '100%'),
                 
               )
         
             ),
             mainPanel(
               tabsetPanel(id="danieltab",
                 tabPanel("Survey Finding", 
                          shinycssloaders::withSpinner(plotOutput("likertplot")),hr(),hr(),
                          
                          fluidRow(
                            column(6,
                                   selectInput("responselvl", h5('Response Level'), choices =
                                                 c("Strongly Agreed"="5",
                                                   "Agreed"="4",
                                                   "Neutral"="3",
                                                   "Disagreed"="2",
                                                   "Strongly Disagreed" = "1"),
                                               width = '100%')
                                   ),
                          
                        
                            column(6,
                                   selectInput("confidlvl", h5('Confidence interval for error bar'), choices =
                                                 c("0.90"="0.90",
                                                   "0.95"="0.95",
                                                   "0.98"="0.98",
                                                   "0.99"="0.99"),
                                               width = '100%')
                                   )
                          ),
                          shinycssloaders::withSpinner(plotOutput("errorbars"))),
                 tabPanel("Association of Factors",  
                          
                          fluidRow(
                            column(12, h3("Association between questions and socio-determinants"))
                          ),
                          fluidRow(
                            column(12, h5("- Bar chart on the left shows the frequency of the factors of interest")),
                            column(12, h5("- Column chart above shows the number of occurrences of the combination of factors
                                          directly below each bar"))
                          ),
                          fluidRow(
                            column(12,  shinycssloaders::withSpinner(plotOutput("factorInterest")))
                          ),
                          hr(),
                          fluidRow(
                            column(12,h3("Legend"))
                          ),
                          
                          fluidRow(
                            column(12,   
                                   "vac_1_ag = Willing to take vaccine"),
                            column(12,   
                                   "vac2_1_ag = Fear of getting COVID19"),
                            column(12,   
                                   "vac2_2_ag = Concern on vaccine side effects")
                            
                          ),
                          
                          fluidRow(
                            column(12,   
                                   "vac2_3_ag = Confidence in vaccine efficacy"),
                            column(12,   
                                   "vac2_4_ag = Vaccine will protect recipient from COVID19 health effects"),
                            column(12,   
                                   "vac2_5_ag = Vaccine will prevent transmission of COVID from recipient")
                            
                          ),   
                          
                          fluidRow(
                            column(12,   
                                   "vac2_6_ag = Will regret if don't take vaccine"),
                            column(12,   
                                   "vac_3_ag = Will take vaccine 1 year later"),
                            column(12,   
                                   "vac4_ag = Vaccine is important for health")
                            
                          ),
                          
                          fluidRow(
                            column(12,   
                                   "vac5_ag = Will get vaccine if available"),
                            column(12,   
                                   "vac6_ag = Family and friends will want respondent to be vaccinated"),
                            column(12,   
                                   "vac7_ag = Trust COVID19 vaccines")
                            
                          ),
                          br(),
                          br()
                          
                                   
                      ),
                         
                 tabPanel("Data Exploration",  
                          fluidRow(
                            column(12,h3("Distribution between 2 variables"))
                          ),
                          shinycssloaders::withSpinner(plotOutput("dataexplorationtab")),
                          hr(),
                          fluidRow(
                            column(12,h3("Legend"))
                          ),
                          
                          fluidRow(
                            column(12,   
                                   "vac_1_ag = Willing to take vaccine"),
                            column(12,   
                                   "vac2_1_ag = Fear of getting COVID19"),
                            column(12,   
                                   "vac2_2_ag = Concern on vaccine side effects")
                            
                          ),
                          
                          fluidRow(
                            column(12,   
                                   "vac2_3_ag = Confidence in vaccine efficacy"),
                            column(12,   
                                   "vac2_4_ag = Vaccine will protect recipient from COVID19 health effects"),
                            column(12,   
                                   "vac2_5_ag = Vaccine will prevent transmission of COVID from recipient")
                            
                          ),   
                          
                          fluidRow(
                            column(12,   
                                   "vac2_6_ag = Will regret if don't take vaccine"),
                            column(12,   
                                   "vac_3_ag = Will take vaccine 1 year later"),
                            column(12,   
                                   "vac4_ag = Vaccine is important for health")
                            
                          ),
                          
                          fluidRow(
                            column(12,   
                                   "vac5_ag = Will get vaccine if available"),
                            column(12,   
                                   "vac6_ag = Family and friends will want respondent to be vaccinated"),
                            column(12,   
                                   "vac7_ag = Trust COVID19 vaccines")
                            
                          )
                          )
               )
             )
    )
  )
)
server <- function(input, output, session){
  danielvalue <- reactiveValues(default =-1)
  daniel1value <- reactiveValues(default =-1)
  myvalues <- reactiveValues(default = 0)
  mypredictvalue <- reactiveValues(default =0)
  myinitialvalue <- reactiveValues(default =0)
  myetsvalue <- reactiveValues(default = 0)
  myprophetvalue <- reactiveValues(default = 0)
  datafile <- csvFileServer("datafile", stringsAsFactors = FALSE,myvalues)
  output$initialtext <- renderText(
    if(myvalues$default == 0){ 
      paste("Please Upload the latest file from Johns Hopkins University. Exisitng Dataset as follows (caa 27 Mar 2021):", "", sep="\n\n\n")
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
                     .facet_scales = "free",
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
                         .interactive = TRUE, .y_lab = "Number of Cases", .x_lab = "Date")
    
    
  })
  
  output$anomalyPlot <- renderPlotly({
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
    }
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    plot_anomaly_diagnostics(CountrySelected,Date, Daily_new_cases, .facet_ncol = 2, .y_lab = "Number of Cases")
    
    
  })
  
  

  
etsmodelPanelServer("datafile")
  
prophetmodelPanelServer("datafile")

  
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
  

  
  output$etspredictive <- renderPlotly({
    
    observeEvent(input$ETSGo,{
      myetsvalue$default <- input$ETSGo
    })
    
    
     etserrorselection <- eventReactive(input$ETSGo, {
       isolate(input$errorinput)
     })
     etstrendselection <- eventReactive(input$ETSGo, {
       isolate(input$trendinput)
     })
     etsseasonselection <- eventReactive(input$ETSGo, {
       isolate(input$seasoninput)
     })
    
    date_start <- input$date_range1[1]
    date_start <- date_start+1
    x <- format(date_start, "%d-%m-%Y")
    x<-geYearMonth(x)
    y<-geYearMonth("27-03-2021")
    
    
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
      updateDateRangeInput(session, "date_range1",end = getMyDate(datafile()))
      date_end <- input$date_range[2]
      date_end <-  date_end+1
      y <- format(date_end, "%d-%m-%Y")
      y<-geYearMonth(y)
    }
    selected <- getDataByTime(selected,x,y)
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- ETSModel(traindata,"additive","additive","additive")
    
    if(myetsvalue$default == 1){
    USmodels_tbl <- ETSModel(traindata,etserrorselection(),etstrendselection(),etsseasonselection())
    }

    UScalibration_tbl <- USmodels_tbl %>%
      modeltime_calibrate(new_data = testdata)
    
    UScalibration_tbl %>%
      modeltime_accuracy() 
    
    USrefit_tbl <- UScalibration_tbl %>%
      modeltime_refit(data = CountrySelected)
    
    mydayselection <- input$dayselection1
    
    USrefit_tbl %>%
      modeltime_forecast(h = mydayselection, actual_data = CountrySelected) %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE,
        .title = selectedCountry(),
        .y_lab = "Number of Cases"
      )
   
  })
  
  
  
  output$etspredictiveaccuracy <- renderReactable({
    etserrorselection <- eventReactive(input$ETSGo, {
      isolate(input$errorinput)
    })
    etstrendselection <- eventReactive(input$ETSGo, {
      isolate(input$trendinput)
    })
    etsseasonselection <- eventReactive(input$ETSGo, {
      isolate(input$seasoninput)
    })
    
    date_start <- input$date_range1[1]
    date_start <- date_start+1
    x <- format(date_start, "%d-%m-%Y")
    
    x<-geYearMonth(x)
    y<-geYearMonth("27-03-2021")
    
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
      updateDateRangeInput(session, "date_range1",end = getMyDate(datafile()))
      date_end <- input$date_range[2]
      date_end <-  date_end+1
      y <- format(date_end, "%d-%m-%Y")
      y<-geYearMonth(y)
    }
    selected <- getDataByTime(selected,x,y)
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- ETSModel(traindata,"additive","additive","additive")
    
    if(myetsvalue$default == 1){
    USmodels_tbl <- ETSModel(traindata,etserrorselection(),etstrendselection(),etsseasonselection())
    }
    
    UScalibration_tbl <- USmodels_tbl %>%
      modeltime_calibrate(new_data = testdata)
    
    UScalibration_tbl %>%
      modeltime_accuracy() 


    USrefit_tbl <- UScalibration_tbl %>%
      modeltime_refit(data = CountrySelected)
    
    mydayselection <- input$dayselection1
    
    USrefit_tbl %>%
      modeltime_forecast(h = mydayselection, actual_data = CountrySelected)
    
    USrefit_tbl %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(
        .interactive = TRUE,
        .title = selectedCountry()
      )
  })
  
  
  output$prophetpredictive <- renderPlotly({
    
    observeEvent(input$ProphetGo,{
      myprophetvalue$default <- input$ProphetGo
    })
    
    
    growthselection <- eventReactive(input$ProphetGo, {
      isolate(input$growthinput)
    })
    changepointselection <- eventReactive(input$ProphetGo, {
      isolate(input$changepointinput)
    })
    pseasonselection <- eventReactive(input$ProphetGo, {
      isolate(input$pseasoninput)
    })
    
    date_start <- input$date_range2[1]
    date_start <- date_start+1
    x <- format(date_start, "%d-%m-%Y")
    x<-geYearMonth(x)
    y<-geYearMonth("27-03-2021")
    
    
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
      updateDateRangeInput(session, "date_range2",end = getMyDate(datafile()))
      date_end <- input$date_range[2]
      date_end <-  date_end+1
      y <- format(date_end, "%d-%m-%Y")
      y<-geYearMonth(y)
    }
    selected <- getDataByTime(selected,x,y)
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- ProphetModel(traindata,"linear",0.25,"additive")
 
    if(myprophetvalue$default == 1){
      USmodels_tbl <- ProphetModel(traindata,growthselection(),   as.numeric(changepointselection()),pseasonselection())
    }
    
    UScalibration_tbl <- USmodels_tbl %>%
      modeltime_calibrate(new_data = testdata)
    
    UScalibration_tbl %>%
      modeltime_accuracy() 
    
    USrefit_tbl <- UScalibration_tbl %>%
      modeltime_refit(data = CountrySelected)
    
    mydayselection <- input$dayselection2
    
    USrefit_tbl %>%
      modeltime_forecast(h = mydayselection, actual_data = CountrySelected) %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE,
        .title = selectedCountry(),
        .y_lab = "Number of Cases"
      )
    
  })
  
  output$prophetpredictiveaccuracy <- renderReactable({
    
    observeEvent(input$ProphetGo,{
      myprophetvalue$default <- input$ProphetGo
    })
    
    
    growthselection <- eventReactive(input$ProphetGo, {
      isolate(input$growthinput)
    })
    changepointselection <- eventReactive(input$ProphetGo, {
      isolate(input$changepointinput)
    })
    pseasonselection <- eventReactive(input$ProphetGo, {
      isolate(input$pseasoninput)
    })
    
    date_start <- input$date_range2[1]
    date_start <- date_start+1
    x <- format(date_start, "%d-%m-%Y")
    x<-geYearMonth(x)
    y<-geYearMonth("27-03-2021")
    
    
    selected <- formatraw(confirmed_cases_raw)
    if(myvalues$default == 1){
      selected <- formatraw(datafile())
      updateDateRangeInput(session, "date_range2",end = getMyDate(datafile()))
      date_end <- input$date_range[2]
      date_end <-  date_end+1
      y <- format(date_end, "%d-%m-%Y")
      y<-geYearMonth(y)
    }
    selected <- getDataByTime(selected,x,y)
    CountrySelected <- getIndividualCountryData(selected,selectedCountry())
    traindata <- createTrainData(CountrySelected)
    testdata <- createTestData(CountrySelected)
    
    USmodels_tbl <- ProphetModel(traindata,"linear",0.25,"additive")
    
    if(myprophetvalue$default == 1){
      USmodels_tbl <- ProphetModel(traindata,growthselection(),   as.numeric(changepointselection()),pseasonselection())
    }
    
    UScalibration_tbl <- USmodels_tbl %>%
      modeltime_calibrate(new_data = testdata)
    
    UScalibration_tbl %>%
      modeltime_accuracy() 
    
    USrefit_tbl <- UScalibration_tbl %>%
      modeltime_refit(data = CountrySelected)
    
    mydayselection <- input$dayselection2
    
    USrefit_tbl %>%
      modeltime_forecast(h = mydayselection, actual_data = CountrySelected)
    
    USrefit_tbl %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(
        .interactive = TRUE,
        .title = selectedCountry()
      )
    
  })
  
  


  

  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE))  
 
 
  modelselection <- eventReactive(input$ModelgoButton, {
    validate(
      need(input$variable1, 'No model selected. Please select a model!')
     
    )
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
    
    mydayselection <- input$dayselection
    
    USrefit_tbl %>%
      modeltime_forecast(h = mydayselection, actual_data = CountrySelected) %>%
      plot_modeltime_forecast(
        .legend_max_width = 25, # For mobile screens
        .interactive      = TRUE,
        .title = selectedCountry(),
        .y_lab = "Number of Cases"
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
    
    mydayselection <- input$dayselection
    
    USrefit_tbl %>%
      modeltime_forecast(h = mydayselection, actual_data = CountrySelected)
    
    USrefit_tbl %>%
      modeltime_accuracy() %>%
      table_modeltime_accuracy(
        .interactive = TRUE,
        .title = selectedCountry()
      )
    })
  
 
 ###################  Daniel's Server Code #########################################
  
  output$dataexplorationtab <- renderPlot({
    display <-  c("age"="age", "gender"="gender", "household_size"="household_size","household_children"="household_children",
                  "vac_1"="vac_1","vac2_1"="vac2_1","vac2_2"="vac2_2","vac2_3"="vac2_3","vac2_4"="vac2_4","vac2_5"="vac2_5","vac2_6"="vac2_6",
                  "vac_3"="vac_3","vac4"="vac4","vac5"="vac5","vac6"="vac6","vac7"="vac7")
    
    
    dcountry1 <- input$countryofinterest1
    
    tvariable <-input$targetVariable
    pvariable <- input$predictiveVariable
    
   
    
    if(daniel1value$default == -1){
      mydisplay <- setdiff(display, tvariable)
      updateSelectInput(session, "predictiveVariable",choices =mydisplay )
      daniel1value$default <- 0
    }
   
   observe({
     ovar <-input$targetVariable
     if(ovar != tvariable){
       daniel1value$default <- -1
     }
     
   })
    
    abc<- mydataexplorationplot(dcountry1,tvariable,pvariable)
    plot(abc)
    
  })
  
  
  mydataexplorationplot <- function(dcountry1,tvariable,pvariable){
    main_df <- main_df %>% mutate(vac_1 = case_when(
      vac_1 == "1 - Strongly agree" ~ "1",
      vac_1 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac_1)))
    
    main_df <- main_df %>% mutate(vac2_1 = case_when(
      vac2_1 == "1 - Strongly agree" ~ "1",
      vac2_1 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_1)))
    
    main_df <- main_df %>% mutate(vac2_2 = case_when(
      vac2_2 == "1 - Strongly agree" ~ "1",
      vac2_2 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_2)))
    
    main_df <- main_df %>% mutate(vac2_3 = case_when(
      vac2_3 == "1 - Strongly agree" ~ "1",
      vac2_3 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_3)))
    
    main_df <- main_df %>% mutate(vac2_4 = case_when(
      vac2_4 == "1 - Strongly agree" ~ "1",
      vac2_4 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_4)))
    
    main_df <- main_df %>% mutate(vac2_5 = case_when(
      vac2_5 == "1 - Strongly agree" ~ "1",
      vac2_5 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_5)))
    
    main_df <- main_df %>% mutate(vac2_6 = case_when(
      vac2_6 == "1 - Strongly agree" ~ "1",
      vac2_6 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_6)))
    
    main_df <- main_df %>% mutate(vac_3 = case_when(
      vac_3 == "1 - Strongly agree" ~ "1",
      vac_3 == "5 – Strongly disagree" ~ "5",
      TRUE ~ as.character(vac_3)))
    
    main_df <- main_df %>% mutate(vac4 = case_when(
      vac4 == "Not at all important" ~ "1",
      vac4 == "A little important" ~ "2",
      vac4 == "Moderately important" ~ "3",
      vac4 == "Very important" ~ "4"))
    
    main_df <- main_df %>% mutate(vac5 = case_when(
      vac5 == "Yes" ~ "1",
      vac5 == "No" ~ "2",
      vac5 == "Not sure" ~ "99"))
    
    main_df <- main_df %>% mutate(vac6 = case_when(
      vac6 == "Yes" ~ "1",
      vac6 == "No" ~ "2",
      vac6 == "Not sure" ~ "99"))
    
    main_df <- main_df %>% mutate(vac7 = case_when(
      vac7 == "Not at all" ~ "1",
      vac7 == "A little" ~ "2",
      vac7 == "Moderately" ~ "3",
      vac7 == "Very much" ~ "4"))
    
    main_df <- main_df %>% replace_with_na(replace = list(vac5 = "99", vac6 = "99" ))
    
    countrySelected = dcountry1
    
    explore_df <- filter(main_df, country == countrySelected)
    
    variable_selected <- tvariable   # to be selected by user
    pred_var <- pvariable             # to be selected by user
    
    categ <- target_by(explore_df, variable_selected)
    cat_num <- relate(categ, pred_var)
    return(cat_num)
  }
  
  ###################################################################################
  

  
  observeEvent(input$variable4, {
    if(input$variable4 == FALSE){
      myetsvalue$default <- 0
      removeTab(inputId = "tabs1", target = "ETS Model")
    }
   else{
      insertTab(inputId = "tabs1",
                tabPanel("ETS Model", etsmodelPanelUI("datafile")),
                target = "Model Comparison"
      )
      
      updateTabsetPanel(session, "tabs1",
                        selected = "ETS Model"
      )
      
    }
  })
  
  
  observeEvent(input$variable5, {
    if(input$variable5 == FALSE){
      removeTab(inputId = "tabs1", target = "Prophet Model")
    }
    else{
    
      insertTab(inputId = "tabs1",
                tabPanel("Prophet Model", prophetmodelPanelUI("datafile")),
                target = "Model Comparison"
      )
      
      updateTabsetPanel(session, "tabs1",
                        selected = "Prophet Model"
      )
      
    }
  })
  
  
  output$errorbars <- renderPlot({
    dresponselvl <- input$responselvl
    dconfidlvl <- input$confidlvl
    
    
    #recode variables
    main_df <- main_df %>% mutate(vac_1 = case_when(
      vac_1 == "1 - Strongly agree" ~ "1",
      vac_1 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac_1)))
    
    main_df <- main_df %>% mutate(vac2_1 = case_when(
      vac2_1 == "1 - Strongly agree" ~ "1",
      vac2_1 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_1)))
    
    main_df <- main_df %>% mutate(vac2_2 = case_when(
      vac2_2 == "1 - Strongly agree" ~ "1",
      vac2_2 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_2)))
    
    main_df <- main_df %>% mutate(vac2_3 = case_when(
      vac2_3 == "1 - Strongly agree" ~ "1",
      vac2_3 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_3)))
    
    main_df <- main_df %>% mutate(vac2_4 = case_when(
      vac2_4 == "1 - Strongly agree" ~ "1",
      vac2_4 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_4)))
    
    main_df <- main_df %>% mutate(vac2_5 = case_when(
      vac2_5 == "1 - Strongly agree" ~ "1",
      vac2_5 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_5)))
    
    main_df <- main_df %>% mutate(vac2_6 = case_when(
      vac2_6 == "1 - Strongly agree" ~ "1",
      vac2_6 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_6)))
    
    main_df <- main_df %>% mutate(vac_3 = case_when(
      vac_3 == "1 - Strongly agree" ~ "1",
      vac_3 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac_3)))
    
    main_df <- main_df %>% mutate(vac4 = case_when(
      vac4 == "Not at all important" ~ "1",
      vac4 == "A little important" ~ "2",
      vac4 == "Moderately important" ~ "3",
      vac4 == "Very important" ~ "4"))
    
    main_df <- main_df %>% mutate(vac5 = case_when(
      vac5 == "Yes" ~ "1",
      vac5 == "No" ~ "2",
      vac5 == "Not sure" ~ "99"))
    
    main_df <- main_df %>% mutate(vac6 = case_when(
      vac6 == "Yes" ~ "1",
      vac6 == "No" ~ "2",
      vac6 == "Not sure" ~ "99"))
    
    main_df <- main_df %>% mutate(vac7 = case_when(
      vac7 == "Not at all" ~ "1",
      vac7 == "A little" ~ "2",
      vac7 == "Moderately" ~ "3",
      vac7 == "Very much" ~ "4"))
    
    main_df <- main_df %>% replace_with_na(replace = list(vac5 = "99", vac6 = "99" ))
    # declare responses with "Not Sure" as missing values  
    
    # Make the table long - Gather question data from columns to rows
    main_gathered <- gather(main_df, measure, response, c(6:17))
    
    # Obtain dataset with only responses for selected question
    
    qn_selected <- input$qn     # variable of question selected
    
    vac_1 <- filter(main_gathered, measure == qn_selected)
    
    responseLevel <- dresponselvl 
    
    vac_1 <- vac_1 %>% mutate(countSA = case_when(
      response >= responseLevel ~ 1,
      TRUE ~ as.double(0)
    ))
    
    vac_1 <- vac_1 %>% mutate(countResponse = 1)
    
    # Obtain contingency table for Prop of SA
    propSA_df <- table(vac_1$country,vac_1$countSA) %>% as.data.frame.matrix()
    countResponse_df <- table(vac_1$country,vac_1$countResponse) %>% as.data.frame.matrix()
    
    # Change column names, now labelled as 1-5
    colnames(propSA_df) <- c("NotSA","SA")
    
    # Compute proportion of SA
    propSA_df$propSA <- (propSA_df$SA / (propSA_df$SA + propSA_df$NotSA))
    
    # Compute SE
    propSA_df$SE <- sqrt((propSA_df$propSA*(1 - propSA_df$propSA))/ (propSA_df$SA + propSA_df$NotSA))
    
    # compute z-value
    
    confidence_level <- dconfidlvl  
    z_value <- 0
    if (confidence_level == "0.90") {
      print("not in here 0.9")
      z_value <- 1.645
    } else if (confidence_level == "0.95") {
      print("not in here 0.95")
      z_value <- 1.96
    } else if (confidence_level == "0.98") {
      print("not in here 0.98")
      z_value <- 2.33
    } else if (confidence_level == "0.99") {
      print("not in here 0.99")
      z_value <- 2.58
    }
    
    # Compute Upper and Lower Limits
    propSA_df$UppLim <- propSA_df$propSA + z_value*propSA_df$SE
    propSA_df$LowLim <- propSA_df$propSA - z_value*propSA_df$SE
    
    # Insert column into propSA_df with country labels
    propSA_df$country <- c("Australia","Canada","Denmark",
                           "Finland","France","Germany",
                           "Israel","Italy","Japan",
                           "Netherlands","Norway","Singapore",
                           "South Korea","Spain","Sweden",
                           "United Kingdom","United States")
    
    #vertical bar chart with error plots
    a <- ggplot(propSA_df) +
      geom_bar(aes(reorder(country,propSA),y = propSA), 
               stat = "identity", fill ="dodgerblue3", alpha = 0.8) +labs(title="Proportion of Respondents who provided the Response Level selected", x="Country", y="Proportion") + 
      theme(axis.title.y = element_text(size = rel(2), angle = 90)) +
      theme(axis.title.x = element_text(size = rel(2), angle = 0)) +
      theme(axis.text.x = element_text(color="black", size=16, angle=0)) +
      theme(axis.text.y = element_text(color="black", size=16, angle=0)) +
      theme(legend.title = element_text(colour="black", size=18, face="bold")) +
      geom_errorbar(aes(x = country, ymin = LowLim, ymax = UppLim), 
                    width = 0.5, colour = "firebrick2",
                    alpha = 0.9, size = 0.6) +
      coord_flip()
    theme(legend.position = "top")
    
    return (a)
    
  })
  
  
  output$factorInterest <- renderPlot({
    
    dcountry <- input$countryofinterest
    print(dcountry)
    dstrength <- input$strengthResponse
    print(dstrength)
    totalselection <- c("vac_1_ag","vac2_1_ag")
    
  
    
    
   totalselection <- eventReactive(input$DfactorButton, {
      input$factorofinterest
    })
   
    observeEvent(input$DfactorButton,{
      danielvalue$default <- input$DfactorButton
    })
    
  
   
    
    countrySelected = dcountry
    strength = as.integer(dstrength)  # For user to determine level of agreement - 4 Agree, 5 Strongly agree
    
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
    
    selectionlist <- vector()
    if(sum(upset_df$vac7_ag) == 0){
      selectionlist <- append(selectionlist,"vac7_ag")
    }
    if(sum(upset_df$vac6_ag) == 0){
      selectionlist <- append(selectionlist,"vac6_ag")
    }
    if(sum(upset_df$vac5_ag) == 0){
      selectionlist <- append(selectionlist,"vac5_ag")
    }
    if(sum(upset_df$vac4_ag) == 0){
      selectionlist <- append(selectionlist,"vac4_ag")
    }
    if(sum(upset_df$vac_3_ag) == 0){
      selectionlist <- append(selectionlist,"vac_3_ag")
    }
    if(sum(upset_df$vac2_6_ag) == 0){
      selectionlist <- append(selectionlist,"vac2_6_ag")
    }
    if(sum(upset_df$vac2_5_ag) == 0){
      selectionlist <- append(selectionlist,"vac2_5_ag")
    }
    if(sum(upset_df$vac2_4_ag) == 0){
      selectionlist <- append(selectionlist,"vac2_4_ag")
    }
    if(sum(upset_df$vac2_3_ag) == 0){
      selectionlist <- append(selectionlist,"vac2_3_ag")
    }
    if(sum(upset_df$vac2_2_ag) == 0){
      selectionlist <- append(selectionlist,"vac2_2_ag")
    }
    if(sum(upset_df$vac2_1_ag) == 0){
      selectionlist <- append(selectionlist,"vac_2_ag")
    }
    if(sum(upset_df$vac_1_ag) == 0){
      selectionlist <- append(selectionlist,"vac_1_ag")
    }
    
    
    initiallist <- c(
      "vac_1_ag" = "vac_1_ag",
      "vac2_1_ag" = "vac2_1_ag",
      "vac2_2_ag" = "vac2_2_ag",
      "vac2_3_ag" = "vac2_3_ag",
      "vac2_4_ag" = "vac2_4_ag",
      "vac2_5_ag" = "vac2_5_ag",          
      "vac2_6_ag" = "vac2_6_ag",
      "vac_3_ag" = "vac_3_ag",
      "vac4_ag" = "vac4_ag",
      "vac5_ag" = "vac5_ag",
      "vac6_ag" = "vac6_ag",
      "vac7_ag" = "vac7_ag"
    )
    
    
    selectionlist <- setdiff(initiallist, selectionlist)
    
    if(danielvalue$default == -1){
    updateSelectInput(session, "factorofinterest",choices = selectionlist, selected=c("vac_1_ag","vac2_1_ag")) 
      danielvalue$default <-0
    }
   
    
    # create combination matrix
    combiMatrix <- select(upset_df,vac_1_ag,vac2_1_ag,vac2_2_ag,
                          vac2_3_ag,vac2_4_ag,vac2_5_ag,
                          vac2_6_ag,vac_3_ag,vac4_ag,
                          vac5_ag,vac6_ag,vac7_ag)
    
    
    if(danielvalue$default > 0){
    combiMatrix <- filter(combiMatrix, vac_1_ag == 1) 
    # UpSetR cannot have 0 length argument (i.e. 0 0 0 0 0) so investigate only vac_1 = 1 cases
    upset(combiMatrix, sets = c("vac_1_ag","vac2_1_ag","vac2_2_ag",
                                "vac2_3_ag","vac2_4_ag","vac2_5_ag",          
                                "vac2_6_ag","vac_3_ag","vac4_ag",
                                "vac5_ag","vac6_ag","vac7_ag"),
          mb.ratio = c(0.55,0.45), order.by = "freq",
          queries = list(
            list(query = intersects, params = totalselection(), active = T)
          ))
    }else{
      combiMatrix <- filter(combiMatrix, vac_1_ag == 1) 
      # UpSetR cannot have 0 length argument (i.e. 0 0 0 0 0) so investigate only vac_1 = 1 cases
      upset(combiMatrix, sets = c("vac_1_ag","vac2_1_ag","vac2_2_ag",
                                  "vac2_3_ag","vac2_4_ag","vac2_5_ag",          
                                  "vac2_6_ag","vac_3_ag","vac4_ag",
                                  "vac5_ag","vac6_ag","vac7_ag"), 
            mb.ratio = c(0.55,0.45), order.by = "freq",
            queries = list(
              list(query = intersects, params = c("vac_1_ag","vac2_1_ag"), active = T)
            ))
      
    }
    
  })
  

  
  output$likertplot <- renderPlot({
    
    dqn <- input$qn
   
    
    #recode variables
    main_df <- main_df %>% mutate(vac_1 = case_when(
      vac_1 == "1 - Strongly agree" ~ "1",
      vac_1 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac_1)))
    
    main_df <- main_df %>% mutate(vac2_1 = case_when(
      vac2_1 == "1 - Strongly agree" ~ "1",
      vac2_1 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_1)))
    
    main_df <- main_df %>% mutate(vac2_2 = case_when(
      vac2_2 == "1 - Strongly agree" ~ "1",
      vac2_2 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_2)))
    
    main_df <- main_df %>% mutate(vac2_3 = case_when(
      vac2_3 == "1 - Strongly agree" ~ "1",
      vac2_3 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_3)))
    
    main_df <- main_df %>% mutate(vac2_4 = case_when(
      vac2_4 == "1 - Strongly agree" ~ "1",
      vac2_4 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_4)))
    
    main_df <- main_df %>% mutate(vac2_5 = case_when(
      vac2_5 == "1 - Strongly agree" ~ "1",
      vac2_5 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_5)))
    
    main_df <- main_df %>% mutate(vac2_6 = case_when(
      vac2_6 == "1 - Strongly agree" ~ "1",
      vac2_6 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac2_6)))
    
    main_df <- main_df %>% mutate(vac_3 = case_when(
      vac_3 == "1 - Strongly agree" ~ "1",
      vac_3 == "5 - Strongly disagree" ~ "5",
      TRUE ~ as.character(vac_3)))
    
    main_df <- main_df %>% mutate(vac4 = case_when(
      vac4 == "Not at all important" ~ "1",
      vac4 == "A little important" ~ "2",
      vac4 == "Moderately important" ~ "3",
      vac4 == "Very important" ~ "4"))
    
    main_df <- main_df %>% mutate(vac5 = case_when(
      vac5 == "Yes" ~ "1",
      vac5 == "No" ~ "2",
      vac5 == "Not sure" ~ "99"))
    
    main_df <- main_df %>% mutate(vac6 = case_when(
      vac6 == "Yes" ~ "1",
      vac6 == "No" ~ "2",
      vac6 == "Not sure" ~ "99"))
    
    main_df <- main_df %>% mutate(vac7 = case_when(
      vac7 == "Not at all" ~ "1",
      vac7 == "A little" ~ "2",
      vac7 == "Moderately" ~ "3",
      vac7 == "Very much" ~ "4"))
    
    main_df <- main_df %>% replace_with_na(replace = list(vac5 = "99", vac6 = "99" ))
    # declare responses with "Not Sure" as missing values  
    
    # Make the table long - Gather question data from columns to rows
    main_gathered <- gather(main_df, measure, response, c(6:17))
    
    # Obtain dataset with only responses for selected question
    
    qn_selected <- dqn    # variable of question selected
    
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
    
    # Matching appropriate graph titles to qn_selected
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
    
    
    HH::likert(Country ~ .,data = vac_1_df, ylab = NULL,
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
  
  
  
  
  ############################### AMANDA  #######################
  
  ###############
  # SCATTERPLOT #
  ###############
  output$scatterplot <- renderPlot({
    
    xselect <- input$xVariable_bc
    yselect <- input$yVariable_b
    continentselect <- input$selectedContinent
    
    xselect <- enquo(xselect)
    yselect <- enquo(yselect)
    continentselect <- enquo(continentselect)
    
    if (input$selectedContinent == "All") {
      ggscatterstats(data = death_df,
                     x = !!xselect, # independent variable
                     y = !!yselect, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05), # regression line
                                             scale_size_continuous(guide = FALSE)), 
                     label.var = location,
                     # NO CRITERIA TO DISPLAY BY WHICH CONTINENT
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     ggtheme = ggplot2::theme(legend.position = "bottom"))
    } else if (input$selectedContinent == "None") {
      ggscatterstats(data = death_df,
                     x = !!xselect, # independent variable
                     y = !!yselect, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05), # regression line
                                             scale_size_continuous(guide = FALSE)), 
                     # NO DISPLAY OF LABEL
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     ggtheme = ggplot2::theme(legend.position = "bottom"))
    } else {
      ggscatterstats(data = death_df,
                     x = total_cases, # independent variable
                     y = total_deaths_log, # dependent variable
                     type = input$type, # statistical test
                     conf.level = as.numeric(input$conf.level), # confidence level
                     results.subtitle = input$showStatTest, # set to FALSE to NOT display the statistical tests
                     ggplot.component = list(geom_smooth(method = input$gg.regression.method, size =0.05), # regression line
                                             scale_size_continuous(guide = FALSE)), 
                     label.var = location,
                     label.expression = continent == !!continentselect, # criteria to display labels
                     point.args = list(aes(colour=continent,
                                           size=population),
                                       alpha = 0.4),
                     smooth.line.args = list(color = NA, 
                                             se = FALSE),
                     marginal.type = input$marginal.type, # marginal distribution
                     ggtheme = ggplot2::theme(legend.position = "bottom"))
    } # End if else
    
  }) # End renderPlot for scatterplot
  
  ###############
  # FUNNEL PLOT #
  ###############
  output$funnelplot <- renderPlotly({
    
    if (input$xVariable_br == "total_cases") {
      rate <- death_df$case_fatality_rate
      number <- death_df$total_cases_log
    } else if (input$xVariable_br == "total_tests") {
      rate <- death_df$deaths_per_test
      number <- death_df$total_tests_log
    } else if (input$xVariable_br == "population") {
      rate <- death_df$deaths_per_population
      number <- death_df$population_log
    }
    
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
    if (input$xVariable_br == "total_cases") {
      fp_ggplot <- ggplot(death_df, 
                          aes(x = total_cases_log, y = case_fatality_rate)) +
        geom_point(aes(colour=continent,
                       size=population,
                       label=location),
                   alpha=0.4) +
        geom_hline(data = dfCI, aes(yintercept = rate.fem), 
                   size = 0.4, colour = "grey40") +
        xlab("Total number of COVID-19 cases - log") +
        ylab("Case fatality rate") +
        ylim(-0.35,0.35) +
        theme_light() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=7)) +
        scale_size_continuous(guide = FALSE)
      
      fp_densigram <- ggplot(death_df, aes(x = case_fatality_rate)) +
        geom_histogram(bins = 40, fill="orange", colour="grey40", size=0.2) + 
        geom_density(colour="grey40", size=0.2) +
        coord_flip(clip = "off") +
        ggtitle("Distribution") +
        theme_void() +
        theme(axis.line = element_line(colour = "white"),
              panel.grid = element_line(colour = "white"))
      
    } else if (input$xVariable_br == "total_tests") {
      fp_ggplot <- ggplot(death_df, 
                          aes(x = total_tests_log, y = deaths_per_test)) +
        geom_point(aes(colour=continent,
                       size=population,
                       label=location),
                   alpha=0.4) +
        geom_hline(data = dfCI, aes(yintercept = rate.fem), 
                   size = 0.4, colour = "grey40") +
        xlab("Total number of COVID-19 tests - log") +
        ylab("Number of deaths per test") +
        ylim(-0.025,0.05) +
        theme_light() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=7)) +
        scale_size_continuous(guide = FALSE)
      
      fp_densigram <- ggplot(death_df, aes(x = deaths_per_test)) +
        geom_histogram(bins = 60, fill="orange", colour="grey40", size=0.2) + 
        geom_density(colour="grey40", size=0.2) +
        coord_flip(clip = "off") +
        ggtitle("Distribution") +
        theme_void() +
        theme(axis.line = element_line(colour = "white"),
              panel.grid = element_line(colour = "white"))
      
    } else if (input$xVariable_br == "population") {
      fp_ggplot <- ggplot(death_df, 
                          aes(x = population_log, y = deaths_per_population)) +
        geom_point(aes(colour=continent,
                       size=population,
                       label=location),
                   alpha=0.4) +
        geom_hline(data = dfCI, aes(yintercept = rate.fem), 
                   size = 0.4, colour = "grey40") +
        xlab("Total population - log") +
        ylab("Death rate (population)") +
        ylim(-0.025, 0.075) +
        theme_light() +
        theme(legend.title = element_blank(),
              legend.text = element_text(size=7)) +
        scale_size_continuous(guide = FALSE)
      
      fp_densigram <- ggplot(death_df, aes(x = deaths_per_population)) +
        geom_histogram(binwidth = 0.001, fill="orange", colour="grey40", size=0.2) + 
        geom_density(colour="grey40", size=0.2) +
        coord_flip(clip = "off") +
        ggtitle("Distribution") +
        theme_void() +
        theme(axis.line = element_line(colour = "white"),
              panel.grid = element_line(colour = "white"))
    }
    
    # Display confidence range
    if (90 %in% input$conf.range) {
      fp_ggplot <- fp_ggplot +
        geom_line(data = dfCI, aes(x = number.seq, y = number.ll90), 
                  size = 0.4, colour = "grey40", linetype = "dotted") +
        geom_line(data = dfCI, aes(x = number.seq, y = number.ul90), 
                  size = 0.4, colour = "grey40", linetype = "dotted") +
        annotate("text", x = 0.3, y = dfCI$number.ll90[1], label = "90%", size = 2, colour = "grey40")
    } 
    
    if (95 %in% input$conf.range) {
      fp_ggplot <- fp_ggplot + 
        geom_line(data = dfCI, aes(x = number.seq, y = number.ll95), 
                  size = 0.4, colour = "grey40", linetype = "dashed") +
        geom_line(data = dfCI, aes(x = number.seq, y = number.ul95), 
                  size = 0.4, colour = "grey40", linetype = "dashed") +
        annotate("text", x = 0.3, y = dfCI$number.ll95[1], label = "95%", size = 2, colour = "grey40")
    } 
    
    if (99 %in% input$conf.range) {
      fp_ggplot <- fp_ggplot + 
        geom_line(data = dfCI, aes(x = number.seq, y = number.ll99), 
                  size = 0.4, colour = "grey40", linetype = "twodash") +
        geom_line(data = dfCI, aes(x = number.seq, y = number.ul99), 
                  size = 0.4, colour = "grey40", linetype = "twodash") +
        annotate("text", x = 0.3, y = dfCI$number.ll99[1], label = "99%", size = 2, colour = "grey40")
    } 
    
    if (99.9 %in% input$conf.range) {
      fp_ggplot <- fp_ggplot + 
        geom_line(data = dfCI, aes(x = number.seq, y = number.ll999), 
                  size = 0.4, colour = "grey40") +
        geom_line(data = dfCI, aes(x = number.seq, y = number.ul999), 
                  size = 0.4, colour = "grey40") +
        annotate("text", x = 0.3, y = dfCI$number.ll999[1], label = "99.9%", size = 2, colour = "grey40")
    }
    
    # Combine funnel plot and distribution
    plotly::subplot(fp_ggplot, fp_densigram,
            nrows = 1,
            widths = c(4/5,1/5),
            shareY = TRUE,
            titleX = TRUE,
            which_layout = 1)
  }) # End renderPlotly for funnelplot
  
  #################
  # LEAST SQUARES #
  #################
  
  model_lsr <- reactive({
    if(input$yVariable_m == "total_deaths"){
      ols_regress(reformulate(response=input$yVariable_m,
                              termlabels=input$xVariable_m_td),
                  data = death_df,
                  iterm = input$iterm)
    } else if (input$yVariable_m == "case_fatality_rate") {
      ols_regress(reformulate(response=input$yVariable_m,
                              termlabels=input$xVariable_m_cfr),
                  data = death_df,
                  iterm = input$iterm)
    } else if (input$yVariable_m == "deaths_per_test") {
      ols_regress(reformulate(response=input$yVariable_m,
                              termlabels=input$xVariable_m_dpt),
                  data = death_df,
                  iterm = input$iterm)
    } else if (input$yVariable_m == "deaths_per_population") {
      ols_regress(reformulate(response=input$yVariable_m,
                              termlabels=input$xVariable_m_dpp),
                  data = death_df,
                  iterm = input$iterm)
    }
  }) # End model_lsr reactive
  
  output$lsrResultsText <- renderPrint({
    
    model_lsr()
    
  }) # End renderPrint for lsr results
  
  ######################
  # VARIABLE SELECTION #
  ######################
  
  # create base model
  model <- reactive({
    if(input$yVariable_m == "total_deaths"){
      lm(reformulate(response=input$yVariable_m,
                     termlabels=input$xVariable_m_td),
         data = death_df)
    } else if (input$yVariable_m == "case_fatality_rate") {
      lm(reformulate(response=input$yVariable_m,
                     termlabels=input$xVariable_m_cfr),
         data = death_df)
    } else if (input$yVariable_m == "deaths_per_test") {
      lm(reformulate(response=input$yVariable_m,
                     termlabels=input$xVariable_m_dpt),
         data = death_df)
    } else if (input$yVariable_m == "deaths_per_population") {
      lm(reformulate(response=input$yVariable_m,
                     termlabels=input$xVariable_m_dpp),
         data = death_df)
    }
  }) # End model reactive
  
  # create model based on VSM chosen
  model_vsm <- reactive({
    if (input$vsMethod == "all_possible") {
      ols_step_all_possible(model())
    } else if (input$vsMethod == "best_subset") {
      ols_step_best_subset(model())
    } else if (input$vsMethod == "forward_p") {
      ols_step_forward_p(model(), 
                         pent = input$prem, 
                         prem = input$prem, 
                         progress = input$showStep)
    } else if (input$vsMethod == "backward_p") {
      ols_step_backward_p(model(), 
                          pent = input$prem, 
                          prem = input$prem, 
                          progress = input$showStep)
    } else if (input$vsMethod == "both_p") {
      ols_step_both_p(model(), 
                      pent = input$prem, 
                      prem = input$prem, 
                      progress = input$showStep)
    } else if (input$vsMethod == "forward_aic") {
      ols_step_forward_aic(model(), 
                           progress = input$showStep)
    } else if (input$vsMethod == "backward_aic") {
      ols_step_backward_aic(model(), 
                            progress = input$showStep)
    } else if (input$vsMethod == "both_aic") {
      ols_step_both_aic(model(), 
                        progress = input$showStep)
    }
  }) # End model_vsm reactive
  
  # Printing the results
  output$VSMResultsText <- renderPrint({
    model_vsm()
  }) # End renderPrint for vsmResultsText
  
  # Plotting the results
  output$VSMResultsPlot <- renderPlot({
    plot(model_vsm())
  }) # End renderPlot for vsmResultsPlot
  
  # Plotting the diagnostics for base model
  output$BaseDiagnosticsPlot1 <- renderPlot({
    p1 <- ols_plot_resid_fit(model())
    p2 <- ols_plot_resid_qq(model())
    
    grid.arrange(p1,p2, ncol=2)
  }) # End renderPlot for BaseDiagnosticsPlot1
  
  # Plotting the diagnostics for base model
  output$BaseDiagnosticsPlot2 <- renderPlot({
    p3 <- ols_plot_resid_fit_spread(model())
    p3
    
  }) # End renderPlot for BaseDiagnosticsPlot2
  
  output$BaseDiagnosticsPlot3 <- renderPlot({
    p4 <- ols_plot_cooksd_chart(model())
    p5 <- ols_plot_resid_stud_fit(model())
    
    grid.arrange(p4,p5, ncol=2)
  }) # End renderPlot for BaseDiagnosticsPlot3
  
  # Plotting the collinearity for base model
  output$BaseCollResultsText <- renderPrint({
    ols_coll_diag(model())
  }) # End renderPlot for BaseCollPlot
  
  
  
}

shinyApp(ui,server)






