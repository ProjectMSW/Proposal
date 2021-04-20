
library(readr)
library(tidyverse)
library(dplyr)
library(HH)
library(naniar)
library(dlookr)
library(ggridges)
library(tidyr)



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

