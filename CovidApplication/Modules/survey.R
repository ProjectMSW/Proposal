

factorUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    selectInput(NS(id, "qn"), 'Select Question', choices = 
                  c("Proportion who are willing to take vaccine"="vac_1", 
                    "Proportion worried about getting COVID-19"="vac2_1", 
                    "Proportion worried about side effects of COVID-19 vaccines"="vac2_2", 
                    "Proportion confident government will provide effective COVID-19 vaccines"="vac2_3", 
                    "Proportion confident vaccine will completely protect recipients from health effects of COVID-19"="vac2_4",
                    "Proportion confident vaccine will completely prevent transmission of COVID-19 from recipient to others"="vac2_5",
                    "Proportion who feel they will regret if they do not take the vaccine"="vac2_6", 
                    "Proportion who will take the vaccine if available in 1 year"="vac_3"),
                width = '100%'),
    
    selectInput(NS(id, "responselvl"), 'Response Level', choices = 
                  c("Strongly Agreed"="5", 
                    "Agreed"="4", 
                    "Neutral"="3",
                    "Disagreed"="2",
                    "Strongly Disagreed" = "1"),
                width = '100%'),
    
    selectInput(NS(id, "confidlvl"), 'Response Level', choices = 
                  c("0.90"="0.90", 
                    "0.95"="0.95", 
                    "0.98"="0.98",
                    "0.99"="0.99"),
                width = '100%'),
    
    actionButton(ns("DgoButton"), label = "Go")
    
    
  )
}






surveysideUI <- function(id) {
  ns <- NS(id)
  tagList(
    
    selectInput(NS(id, "qn"), 'Select Question', choices = 
                c("Proportion who are willing to take vaccine"="vac_1", 
               "Proportion worried about getting COVID-19"="vac2_1", 
               "Proportion worried about side effects of COVID-19 vaccines"="vac2_2", 
               "Proportion confident government will provide effective COVID-19 vaccines"="vac2_3", 
               "Proportion confident vaccine will completely protect recipients from health effects of COVID-19"="vac2_4",
               "Proportion confident vaccine will completely prevent transmission of COVID-19 from recipient to others"="vac2_5",
               "Proportion who feel they will regret if they do not take the vaccine"="vac2_6", 
               "Proportion who will take the vaccine if available in 1 year"="vac_3"),
      width = '100%'),
    
    selectInput(NS(id, "responselvl"), 'Response Level', choices = 
                  c("Strongly Agreed"="5", 
                    "Agreed"="4", 
                    "Neutral"="3",
                    "Disagreed"="2",
                    "Strongly Disagreed" = "1"),
                width = '100%'),
    
    selectInput(NS(id, "confidlvl"), 'Response Level', choices = 
                  c("0.90"="0.90", 
                    "0.95"="0.95", 
                    "0.98"="0.98",
                    "0.99"="0.99"),
                width = '100%'),
    
    actionButton(ns("DgoButton"), label = "Go")

    
  )
}

surveysideServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
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
    
    
    a <- likert(Country ~ .,data = vac_1_df, ylab = NULL,
                RefernceZero = 3, as.percent=TRUE,
                positive.order=TRUE,
                main = list(div_chart_title, 
                            x =unit(.55,"npc")),
                sub = list("Response", x=unit(.57,"npc")),
                xlim = c(-100,-80,-60,-40,-20,0,20,40,60,80,100),
                strip = FALSE,
                par.strip.text=list(cex=.7)
    )
    
    return(a)
    
    
  })
}