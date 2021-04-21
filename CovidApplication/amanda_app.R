library(shiny)
library(tidyverse)
library(ggstatsplot)
library(ggExtra)
library(olsrr)
library(plotly)
library(recipes)

death_df <- read_csv("data/deaths_tidy.csv")

death_df <- death_df %>%
    mutate(deaths_per_test = total_deaths / total_tests,
           deaths_per_population = total_deaths / population) %>%
    rename("pop_young" = "0_to_14(%)",
           "pop_working" = "15_to_64(%)",
           "pop_old" = "65_and_above(%)")

ui <- fluidPage(
    hr(),
    
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
                            c("Cumulative death" = "total_deaths", 
                              "Log(Cumulative death)" = "total_deaths_log",
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
                                        "Cumulative positive cases" = "total_cases",
                                        "Cumulative tests conducted" = "total_tests",
                                        "Log(Cumulative positive cases)" = "total_cases_log",
                                        "Log(Cumulative tests conducted)" = "total_tests_log",
                                        "Positive rate" = "positive_rate"),
                                    'Healthcare' = list(
                                        "Hospital beds (per thousand)" = "hospital_beds_per_thousand",
                                        "Physicians (per thousand)" = "num_physicians_per_thousand",
                                        "Handwashing facilities" = "handwashing_facilities"),
                                    'Country indicators' = list(
                                        "Current health expenditure (% of GDP)" = "current_health_exp_%gdp",
                                        "% population living in extreme poverty" = "extreme_poverty",
                                        "GDP per capita" = "gdp_per_capita",
                                        "Government health expenditure (% of total govt. exp.)" = "govt_health_exp_%totalgovtexp",
                                        "Human development index (HDI)" = "human_development_index",
                                        "Log(GDP per capita)" = "gdp_per_capita_log"),
                                    'Population' = list(
                                        "Total population" = "population",
                                        "Population aged 0 to 14" = "pop_young",
                                        "Population aged 15 to 64" = "pop_working",
                                        "Population aged 65 and above" = "pop_old",
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
                                        "Cumulative positive cases" = "total_cases",
                                        "Cumulative tests conducted" = "total_tests"),
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
                            c("Cumulative death" = "total_deaths", 
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
                                        "Population aged 0 to 14" = "pop_young",
                                        "Population aged 15 to 64" = "pop_working",
                                        "Population aged 65 and above" = "pop_old",
                                        "Population density" = "population_density"),
                                    'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                ), # End list
                                multiple = TRUE,
                                selected = c("total_cases",
                                             "hospital_beds_per_thousand",
                                             "gdp_per_capita",
                                             "65_and_above(%)"),
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
                                        "Population aged 0 to 14" = "pop_young",
                                        "Population aged 15 to 64" = "pop_working",
                                        "Population aged 65 and above" = "pop_old",
                                        "Population density" = "population_density"),
                                    'Others' = list(
                                        "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                ), # End list
                                multiple = TRUE,
                                selected = c("positive_rate",
                                             "hospital_beds_per_thousand",
                                             "gdp_per_capita",
                                             "65_and_above(%)"),
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
                                           "Cumulative positive cases" = "total_cases",
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
                                           "Population aged 0 to 14" = "pop_young",
                                           "Population aged 15 to 64" = "pop_working",
                                           "Population aged 65 and above" = "pop_old",
                                           "Population density" = "population_density"),
                                       'Others' = list(
                                           "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                   ), # End list
                                   multiple = TRUE,
                                   selected = c("total_cases",
                                                "hospital_beds_per_thousand",
                                                "gdp_per_capita",
                                                "65_and_above(%)"),
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
                                           "Population aged 0 to 14" = "pop_young",
                                           "Population aged 15 to 64" = "pop_working",
                                           "Population aged 65 and above" = "pop_old",
                                           "Population density" = "population_density"),
                                       'Others' = list(
                                           "Annual international arrivals" = "annual_intl_arrivals_thousands")
                                   ), # End list
                                   multiple = TRUE,
                                   selected = c("total_cases",
                                                "hospital_beds_per_thousand",
                                                "gdp_per_capita",
                                                "65_and_above(%)"),
                                   options = list(
                                       plugins = list("remove_button")
                                   )
                    ) # End selectInput
                ), # End conditionalPanel for y variable: death rate (population)
                
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
                        
                        plotOutput("scatterplot")
                        
                    ), # End conditionalPanel
                    
                    conditionalPanel(
                        condition = "input.tabs_name == 'tab_b' && input.yVariable_b == 'rate'",
                        ns = NS(NULL),
                        
                        h3("Funnel plot with rate distribution"),
                        
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
                        
                        h5("Selected variables:"),
                        verbatimTextOutput("lsrVariables"),
                        
                        h5("Model output:"),
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
                            
                            plotOutput("vsmResultsPlot")
                            
                        ), # End conditionalPanel for plotResults
                        
                        conditionalPanel(
                            condition = "input.plotDiagnostics == 1",
                            ns = NS(NULL),
                            
                            plotOutput("BaseDiagnosticsPlot"),
                            
                            plotOutput("BaseCollPlot")
                            
                        ) # End conditionalPanel for plotDiagnostics
                        
                        
                        
                    ) # End conditionalPanel for variable selection
                 ) # Close tabpanel Multivariate
            ) # Close tabset tabs_names
        ) # End mainPanel
    )  # End sidebarLayout
) # End fluidPage




server <- function(input, output) {
    
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
                           title = "Scatterplot with marginal distribution",
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
                           title = "Scatterplot with marginal distribution",
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
                           title = "Scatterplot with marginal distribution",
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
        subplot(fp_ggplot, fp_densigram,
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
    })
    
    output$lsrResultsText <- renderPrint({
        
        model_lsr()
    
    }) # End renderPrint for lsr results
    
    ######################
    # VARIABLE SELECTION #
    ######################
    
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
    })
    
    model_vsm <- reactive({
        if (input$vsMethod == "all_possible") {
            model_vsm <- ols_step_all_possible(model, 
                                               prem = input$prem, 
                                               progress = input$showStep)
        }
        if (input$vsMethod == "best_subset") {
            model_vsm <- ols_step_best_subset(model, 
                                              prem = input$prem, 
                                              progress = input$showStep)
        } 
        if (input$vsMethod == "forward_p") {
            model_vsm <- ols_step_forward_p(model, 
                                            prem = input$prem, 
                                            progress = input$showStep)
        }
        if (input$vsMethod == "backward_p") {
            model_vsm <- ols_step_backward_p(model, 
                                             prem = input$prem, 
                                             progress = input$showStep)
        }
        if (input$vsMethod == "both_p") {
            model_vsm <- ols_step_both_p(model, 
                                         prem = input$prem, 
                                         progress = input$showStep)
        }
        if (input$vsMethod == "forward_aic") {
            model_vsm <- ols_step_forward_aic(model, 
                                              prem = input$prem, 
                                              progress = input$showStep)
        }
        if (input$vsMethod == "backward_aic") {
            model_vsm <- ols_step_backward_aic(model, 
                                               prem = input$prem, 
                                               progress = input$showStep)
        }
        if (input$vsMethod == "both_aic") {
            model_vsm <- ols_step_both_aic(model, 
                                           prem = input$prem, 
                                           progress = input$showStep)
        }
        
        
    })
    
    
    
    
    
    # Printing the results
    output$VSMResultsText <- renderPrint({
        model_vsm()
    }) # End renderPrint for vsmResultsText
    
    # Plotting the results
    output$vsmResultsPlotly <- renderPlot({
        plot(model_vsm())
    }) # End renderPlot for vsmResultsPlot
    
    # Plotting the diagnostics for base model
    output$BaseDiagnosticsPlot <- renderPlot({
        ols_plot_diagnostics(model())
    }) # End renderPlot for BaseDiagnosticsPlot
    
    # Plotting the collinearity for base model
    output$BaseCollPlot <- renderPlot({
        ols_coll_diag(model())
    }) # End renderPlot for BaseCollPlot
    
}

shinyApp(ui, server)

