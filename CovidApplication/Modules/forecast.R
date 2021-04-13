

forecastUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarPanel(
      selectInput(NS(id, "Msea"), "South East Asia", choices = c("Brunei"="Brunei", "Burma"="Burma", "Cambodia"="Cambodia", 
                                                         "Timor-Leste"="Timor-Leste", "Indonesia"="Indonesia", "Laos"="Laos", "Malaysia"="Malaysia", 
                                                         "Philippines"="Philippines","Singapore"="Singapore", "Thailand"="Thailand", "Vietnam"="Vietnam")),
      
      selectInput(NS(id, "MUS"), "USA", choices = c("US"="US")),
      
      fileInput("file", "File input:"),
      textInput("txt", "Text input:", "general"),
      sliderInput("slider", "Slider input:", 1, 100, 30),
      tags$h5("Default actionButton:"),
      actionButton("action", "Search"),
      
      tags$h5("actionButton with CSS class:"),
      actionButton("action2", "Action button", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Tab 1",
                 plotOutput(NS(id, "hist"))
        ),
        tabPanel("Tab 2", "This panel is intentionally left blank"),
        tabPanel("Tab 3", "This panel is intentionally left blank")
      )
    )
  )
}




forecastServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(mtcars[[input$var]])
    output$hist <- renderPlot({
      hist(data(), breaks = input$bins, main = input$var)
    }, res = 96)
  })
}