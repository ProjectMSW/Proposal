library(shiny)
library(modeltime)
library(timetk)

ui <- basicPage(
  plotOutput("plot1")
)

server <- function(input, output) {
  confirmed_cases_raw <- read_csv("data/time_series_covid19_confirmed_global.csv")
  confirmed_cases_country_level <- confirmed_cases_raw %>%
    gather(Date,Total_Cases,-'Province/State',-'Country/Region',-Lat,-Long) %>%    #collecting all the date variables into a single date variable.
    group_by(`Country/Region`,Date) %>%
    summarize(total = sum(Total_Cases)) %>% 
    select(`Country/Region`, Date, total) %>%
    set_names(c("Country", "Date", "TotalCases")) %>%   #rename the variables
    ungroup()
  
  confirmed_cases_country_level$Date <- as.Date(confirmed_cases_country_level$Date,format="%m/%d/%y")  #converting the Date variable to Date format
  
  # finding the daily new confirmed cases.
  confirmed_cases_country_level <- confirmed_cases_country_level %>%
    group_by(Country) %>%
    arrange(Country,Date)%>%
    mutate(Daily_new_cases = TotalCases - lag(TotalCases, n =1))%>%
    drop_na()
  
  US_Confirmed_Cases <- filter(confirmed_cases_country_level,Country == "US")
  
  output$plot1 <- renderPlot({
    US_Confirmed_Cases%>%
      plot_time_series(Date, Daily_new_cases, 
                       .facet_ncol =2, .facet_scales = "free",
                       .interactive = TRUE,
                       .plotly_slider = FALSE)
  })
  
  
}

shinyApp(ui, server)