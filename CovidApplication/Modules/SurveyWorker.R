
library(readr)
library(tidyverse)
library(dplyr)
library(HH)
library(naniar)
library(dlookr)
library(ggridges)
library(tidyr)
library(plotly)
library(UpSetR)
library(ggplot2)
library(forcats)



surveyMain_df<- read_csv("data/main_df.csv") 


main_gathered <- gather(surveyMain_df, measure, response, c(6:17))

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

aa<-likert(Country ~ .,data = vac_1_df, ylab = NULL,
       RefernceZero = 3, as.percent=TRUE,
       positive.order=TRUE,
       main = list(div_chart_title, 
                   x =unit(.55,"npc")),
       sub = list("Response", x=unit(.57,"npc")),
       xlim = c(-100,-80,-60,-40,-20,0,20,40,60,80,100),
       strip = FALSE,
       par.strip.text=list(cex=.7)
)

aa