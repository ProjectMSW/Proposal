
library(readr)
library(tidyverse)
library(dplyr)
library(HH)
library(naniar)
library(dlookr)
library(ggridges)
library(tidyr)
<<<<<<< HEAD
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
=======



loaddataset <- function(){
main_df= NULL
countrylist <- c("canada","denmark","finland",
                 "france","germany","israel",
                 "italy","japan","netherlands",
                 "norway","singapore","south-korea",
                 "spain","sweden","united-kingdom",
                 "united-states","australia")


fieldNames <- c("endtime", "age", "gender", "household_size",
                "household_children", "vac_1", "vac2_1", "vac2_2",
                "vac2_3", "vac2_4", "vac2_5", "vac2_6", "vac_3",
                "vac4", "vac5", "vac6", "vac7")

for(ctry in countrylist){ 
  # loop to read individual country datasets and append to main_df
  fileName <- paste0("data/",ctry,".csv")

  df <- read_csv(fileName,col_types = cols(
    age = col_integer(),
    .default = col_character() 
  ))
  
  tableName <- paste0(ctry,"_df")
  tableName <- df %>%
    select(any_of(fieldNames)) %>%
    mutate(country = ctry) %>%
    drop_na(vac_1)  
  # main purpose of analysis is to look at vaccination attitudes
  # so records with no response to vac_1 is filtered out
  
  main_df <- bind_rows(main_df,tableName)
  return(main_df)

  }
}


cleandataset <- function(main_df){
  
  #recode variables
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
  return(main_df)
  
  
}




a <- loaddataset()
glimpse(a)
b<-cleandataset(a)
glimpse(b)

>>>>>>> ed6d1c8e11981cb40526ffe8a387343981f77daf
