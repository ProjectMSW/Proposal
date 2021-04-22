library(tidyverse)
library(readr)



confirmed_cases_raw <- read_csv("data/time_series_covid19_confirmed_global.csv") 

death_df <- read_csv("data/deaths_tidy.csv")

main_df <- read_csv("data/main_df.csv")

getMyDate <- function(d){
  e<-(rev(names(d))[1])
  temp <- strsplit(e,"/")
  day <-0
  mon<- 0
  year<-0
  for(i in temp){
    day <- i[2]
    mon <- i[1]
    year <- paste0("20",i[3])
  }
  newtemp <- paste(mon,day,year, sep="-")
  
  return(newtemp)
}

geYearMonth <- function(d){
  temp <- strsplit(d,"-")
  day <-0
  mon<- 0
  year<-0
  for(i in temp){
    day <- i[1]
    mon <- i[2]
    year <- i[3]
  }
  newtemp <- paste(year,mon, sep="-")
  return(newtemp)
  
}



formatraw <- function(raw){
  confirmed_cases_country_level <- raw %>%
    gather(Date,Total_Cases,-'Province/State',-'Country/Region',-Lat,-Long) %>%    #collecting all the date variables into a single date variable.
    group_by(`Country/Region`,Date) %>%
    summarize(total = sum(Total_Cases)) %>% 
    select(`Country/Region`, Date, total) %>%
    set_names(c("Country", "Date", "TotalCases")) %>%   #rename the variables
    ungroup()
  
  confirmed_cases_country_level$Date <- as.Date(confirmed_cases_country_level$Date,format="%m/%d/%y")  #converting the Date variable to Date format
  
  # finding the daily new confirmed cases.
  confirmed_cases_country_level <- confirmed_cases_country_level%>%
    group_by(Country)%>%
    arrange(Country,Date)%>%
    mutate(Daily_new_cases = TotalCases - lag(TotalCases, n =1))%>%
    drop_na()
  
  return(confirmed_cases_country_level)
}


getIndividualCountryData <- function(countryData,country){
  Confirmed_Cases <- countryData%>%
    filter(Country == country)
  return(Confirmed_Cases)
  
} 

createTrainData <- function(data){
  train_tbl <- training(initial_time_split(data, prop = 0.8)) # 80% training and 20% test
  return(train_tbl)
}

createTestData <- function(data){
  test_tbl <- testing(initial_time_split(data, prop = 0.8)) # 80% training and 20% test
  return(test_tbl)
}


createModel <- function(traindata,testdata,countrydata){
  
  model_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  model_fit_arima_boosted <- arima_boost(
    min_n = 2,
    learn_rate = 0.015
  ) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(Daily_new_cases ~ Date + as.numeric(Date) + factor(month(Date, label = TRUE), ordered = F),
        data = traindata)
  
  model_fit_ets <- exp_smoothing() %>%
    set_engine(engine = "ets") %>%
    fit(Daily_new_cases ~ Date , data = traindata)
  
  model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(Daily_new_cases ~ as.numeric(Date),
        data = traindata)
  
  model_spec_mars <- mars(mode = "regression") %>%
    set_engine("earth") 
  
  recipe_spec <- recipe(Daily_new_cases ~ Date, data = traindata) %>%
    step_date(Date, features = "month", ordinal = FALSE) %>%
    step_mutate(date_num = as.numeric(Date)) %>%
    step_normalize(date_num) %>%
    step_rm(Date)
  
  wflw_fit_mars <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec_mars) %>%
    fit(traindata)
  
  
  model_snaive <- naive_reg() %>%
    set_engine(engine = "snaive") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  
  model_ETS <- exp_smoothing( 
    error = "additive",
    trend = "additive",
    season = "none",) %>%
    set_engine(engine = "ets") %>%
    fit(Daily_new_cases ~ Date , data = traindata)
  
  
  # adding the models to table
  
  models_tbl <- modeltime_table(
    model_fit_arima_no_boost,
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm,
    wflw_fit_mars,
    model_snaive,
    model_ETS
  )
  
  calibration_tbl <- models_tbl 
  return(calibration_tbl)
}


PredictionModel <- function(traindata, modelselection){
  
  USmodel_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  USmodel_fit_arima_boosted <- arima_boost(
    min_n = 2,
    learn_rate = 0.015
  ) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(Daily_new_cases ~ Date + as.numeric(Date) + factor(month(Date, label = TRUE), ordered = F),
        data = traindata)
  
  
  USmodel_fit_ets <- exp_smoothing() %>%
    set_engine(engine = "ets") %>%
    fit(Daily_new_cases ~ Date , data = traindata)
  
  
  
  USmodel_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  
  
  USmodel_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    #fit(Daily_new_cases ~ as.numeric(Date) + factor(month(Date, label = TRUE), ordered = FALSE),
    fit(Daily_new_cases ~ as.numeric(Date),
        data = traindata)
  
  
  
  USmodel_spec_mars <- mars(mode = "regression") %>%
    set_engine("earth") 
  
  USrecipe_spec <- recipe(Daily_new_cases ~ Date, data = traindata) %>%
    step_date(Date, features = "month", ordinal = FALSE) %>%
    step_mutate(date_num = as.numeric(Date)) %>%
    step_normalize(date_num) %>%
    step_rm(Date)
  
  USwflw_fit_mars <- workflow() %>%
    add_recipe(USrecipe_spec) %>%
    add_model(USmodel_spec_mars) %>%
    fit(traindata)
  
  
  
  
  USmodel_snaive <- naive_reg() %>%
    set_engine(engine = "snaive") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  
  USmodel_ETS <- exp_smoothing( 
    error = "additive",
    trend = "additive",
    season = "none",) %>%
    set_engine(engine = "ets") %>%
    fit(Daily_new_cases ~ Date , data = traindata)
  
  
  # adding the models to table
  
  # USmodels_tbl <- modeltime_table(
  #   USmodel_fit_arima_no_boost,
  #  USmodel_fit_arima_boosted,
  #  USmodel_fit_ets,
  #   USmodel_fit_prophet,
  #  USmodel_fit_lm,
  #  USwflw_fit_mars,
  #   USmodel_snaive,
  #  USmodel_ETS
  # )
  
  
  a <-modeltime_table(USmodel_fit_arima_boosted)
  b <-modeltime_table(USmodel_fit_ets)
  c1 <-modeltime_table(USmodel_fit_prophet)
  d <- modeltime_table(USmodel_fit_lm)
  e<- modeltime_table(USwflw_fit_mars)
  f<- modeltime_table(USmodel_snaive)
  g<- modeltime_table(USmodel_ETS)
  
  
  USmodels_tbl <- modeltime_table(
    USmodel_fit_arima_no_boost
  )
  
  
  
  for(c in modelselection){
    if(c == "arima_boosted"){
      USmodels_tbl <- combine_modeltime_tables(USmodels_tbl,a)
    }
    if(c == "ets"){
      USmodels_tbl <- combine_modeltime_tables(USmodels_tbl,b)
    }
    if(c == "prophet"){
      USmodels_tbl <- combine_modeltime_tables(USmodels_tbl,c1)
    }
    
    if(c == "lm"){
      USmodels_tbl <- combine_modeltime_tables(USmodels_tbl,d)
    }
    if(c == "ETS"){
      USmodels_tbl <- combine_modeltime_tables(USmodels_tbl,g)
    }
    if(c == "snaive"){
      USmodels_tbl <- combine_modeltime_tables(USmodels_tbl,f)
    }
    if(c == "mars"){
      USmodels_tbl <- combine_modeltime_tables(USmodels_tbl,e)
    }
  }
  
  
  return(USmodels_tbl)
  
}

PredictionModel1 <- function(traindata){
  
  USmodel_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  USmodels_tbl <- modeltime_table(
    USmodel_fit_arima_no_boost
  )
  
  return(USmodels_tbl)
  
}



getDataByTime <- function(d,date1,date2){
  temp <- d%>%
    filter_by_time(Date, date1, date2)
  
  return(temp)
}





createModelforPrediction <- function(traindata, choices){
 
  model_fit_arima_no_boost <- arima_reg() %>%
    set_engine(engine = "auto_arima") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  model_fit_arima_boosted <- arima_boost(
    min_n = 2,
    learn_rate = 0.015
  ) %>%
    set_engine(engine = "auto_arima_xgboost") %>%
    fit(Daily_new_cases ~ Date + as.numeric(Date) + factor(month(Date, label = TRUE), ordered = F),
        data = traindata)
  
  model_fit_ets <- exp_smoothing() %>%
    set_engine(engine = "ets") %>%
    fit(Daily_new_cases ~ Date , data = traindata)
  
  model_fit_prophet <- prophet_reg() %>%
    set_engine(engine = "prophet") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  model_fit_lm <- linear_reg() %>%
    set_engine("lm") %>%
    fit(Daily_new_cases ~ as.numeric(Date),
        data = traindata)
  
  model_spec_mars <- mars(mode = "regression") %>%
    set_engine("earth") 
  
  recipe_spec <- recipe(Daily_new_cases ~ Date, data = traindata) %>%
    step_date(Date, features = "month", ordinal = FALSE) %>%
    step_mutate(date_num = as.numeric(Date)) %>%
    step_normalize(date_num) %>%
    step_rm(Date)
  
  wflw_fit_mars <- workflow() %>%
    add_recipe(recipe_spec) %>%
    add_model(model_spec_mars) %>%
    fit(traindata)
  
  
  model_snaive <- naive_reg() %>%
    set_engine(engine = "snaive") %>%
    fit(Daily_new_cases ~ Date, data = traindata)
  
  
  
  model_ETS <- exp_smoothing( 
    error = "additive",
    trend = "additive",
    season = "none",) %>%
    set_engine(engine = "ets") %>%
    fit(Daily_new_cases ~ Date , data = traindata)
  
  models_tbl <- modeltime_table()
  # adding the models to table
  for(c in choices){
    if(c == "arima_no_boost"){
      
      models_tbl %>%
        add_modeltime_model(model_fit_arima_no_boost)
    }
    
    if(c == "arima_boosted"){
      models_tbl %>%
        add_modeltime_model(model_fit_arima_boosted)
    }
    if(c == "ets"){
      models_tbl %>%
        add_modeltime_model(model_fit_ets)
    }
    if(c == "prophet"){
      models_tbl %>%
        add_modeltime_model(model_fit_prophet)
    }
    
    if(c == "lm"){
      models_tbl %>%
        add_modeltime_model(model_fit_lm)
    }
    if(c == "ETS"){
      models_tbl %>%
        add_modeltime_model(model_ETS)
    }
    if(c == "snaive"){
      models_tbl %>%
        add_modeltime_model(model_snaive)
    }
    if(c == "mars"){
      models_tbl %>%
        add_modeltime_model(wflw_fit_mars)
    }
  }
  
  models_tbl <- modeltime_table(
    model_fit_arima_no_boost,
    model_fit_arima_boosted,
    model_fit_ets,
    model_fit_prophet,
    model_fit_lm,
    wflw_fit_mars,
    model_snaive,
    model_ETS
  )
  
  
  UScalibration_tbl <- models_tbl %>%
    modeltime_calibrate(new_data = traindata)%>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(
      .interactive = TRUE,
      .title = "US Accuracy Table",
      .searchable = FALSE
    )
  #return(models_tbl)
  
}





