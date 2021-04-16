
deathMultivariateServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    print("i am inside module server")
    
    output$input_type_text <- renderText({
      input$A_y_variable
    })
    
  })
}