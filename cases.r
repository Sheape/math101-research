#library(sf) library(spatstat.geom) library(spatstat.random) library(dplyr)
#library(tidyr) library(purrr) library(zoo)

source("labels.r")

match_cases <- function(df) {
  df$Brgy <- population_data$BRGY[match(df$Brgy, toupper(population_data$BRGY))]
  return(df)
}

count_typhoid_cases <- function(typhoid_df) {
  cleaned_df <- cleanup_df(typhoid_df)
  result_df <- cleaned_df %>%
    mutate(LabResult = case_when(
      grepl("POSITIVE", toupper(LabResult), ignore.case = TRUE) ~ "POSITIVE",
      TRUE ~ LabResult
    )) %>%
    mutate(CaseClass = CASECLASS) %>%
    select(Week, Brgy, LabResult, CaseClass) %>%
    filter(Brgy %in% toupper(geodata$ADM4_EN)) %>%
    group_by(Brgy, Week, .drop = FALSE) %>%
    summarise(Cases = sum(
      toupper(CaseClass) == "CONFIRMED" |
        (toupper(CaseClass) == "PROBABLE" & toupper(LabResult) == "POSITIVE"),
      na.rm = TRUE
    )) %>%
    ungroup() %>%
    arrange(Week)
  result_df <- match_cases(result_df)
  return(result_df)
}

count_abd_cases <- function(abd_df) {
  cleaned_df <- cleanup_df(abd_df)
  result_df <- cleaned_df %>%
    select(Week, Brgy, StoolCulture) %>%
    filter(Brgy %in% toupper(geodata$ADM4_EN)) %>%
    group_by(Brgy, Week, .drop = FALSE) %>%
    summarise(Cases = sum(
      toupper(StoolCulture) == "POSITIVE"
    )) %>%
    ungroup() %>%
    arrange(Week)
  result_df <- match_cases(result_df)
  return(result_df)
}

count_typhoid_cases.monthly <- function(typhoid_df) {
  cleaned_df <- cleanup_df(typhoid_df, is_monthly = TRUE)
  result_df <- cleaned_df %>%
    mutate(LabResult = case_when(
      grepl("POSITIVE", toupper(LabResult), ignore.case = TRUE) ~ "POSITIVE",
      TRUE ~ LabResult
    )) %>%
    mutate(CaseClass = CASECLASS) %>%
    select(Month, Brgy, LabResult, CaseClass) %>%
    filter(Brgy %in% toupper(geodata$ADM4_EN)) %>%
    group_by(Brgy, Month, .drop = FALSE) %>%
    summarise(Cases = sum(
      toupper(CaseClass) == "CONFIRMED" |
        (toupper(CaseClass) == "PROBABLE" & toupper(LabResult) == "POSITIVE"),
      na.rm = TRUE #exclude missing values
    )) %>%
    ungroup() %>%
    arrange(Month)
  result_df <- match_cases(result_df)
  return(result_df)
}

count_abd_cases.monthly <- function(abd_df) {
  cleaned_df <- cleanup_df(abd_df, is_monthly = TRUE)
  result_df <- cleaned_df %>%
    select(Month, Brgy, StoolCulture) %>%
    filter(Brgy %in% toupper(geodata$ADM4_EN)) %>%
    group_by(Brgy, Month, .drop = FALSE) %>%
    summarise(Cases = sum(
      toupper(StoolCulture) == "POSITIVE"
    )) %>%
    ungroup() %>%
    arrange(Month)
  result_df <- match_cases(result_df)
  return(result_df)
}


##By DISTRICT##
count_typhoid_cases.monthly.district <- function(typhoid_df) {
  cleaned_df <- cleanup_df(typhoid_df, is_monthly = TRUE)
  result_df <- cleaned_df %>%
    mutate(LabResult = case_when(
      grepl("POSITIVE", toupper(LabResult), ignore.case = TRUE) ~ "POSITIVE",
      TRUE ~ LabResult
    )) %>%
    mutate(CaseClass = CASECLASS) %>%
    select(Month, Brgy, LabResult, CaseClass) %>%
    filter(Brgy %in% toupper(geodata$ADM4_EN))
  result_df <- match_cases(result_df) %>%
    left_join(population_data_district, by='Brgy') %>%
    group_by(DISTRICT, Month, .drop = FALSE) %>%
    summarise(Cases = sum(
      toupper(CaseClass) == "CONFIRMED" |
        (toupper(CaseClass) == "PROBABLE" & toupper(LabResult) == "POSITIVE"),
      na.rm = TRUE #exclude missing values
    )) %>%
    ungroup() %>%
    arrange(Month)
  return(result_df)
}

count_abd_cases.monthly.district <- function(abd_df) {
  cleaned_df <- cleanup_df(abd_df, is_monthly = TRUE)
  result_df <- cleaned_df %>%
    select(Month, Brgy, StoolCulture) %>%
    filter(Brgy %in% toupper(geodata$ADM4_EN))
  result_df <- match_cases(result_df) %>%
    left_join(population_data_district, by='Brgy') %>%
    group_by(DISTRICT, Month, .drop = FALSE) %>%
    summarise(Cases = sum(
      toupper(StoolCulture) == "POSITIVE"
    )) %>%
    ungroup() %>%
    arrange(Month)
  return(result_df)
}

approx_population <- function(population_data) {
  population_df <- population_data %>%
    select(Brgy = "BRGY", X2010:X2022) %>%
    pivot_longer(
      cols = X2010:X2022,
      names_to = "Year",
      values_to = "Population"
    ) %>%
    mutate(Year = as.numeric(gsub("X", "", Year)), Week = 1) %>%
    select(Year, Week, Population, Brgy) %>%
    group_by(Brgy) %>%
    filter(Brgy != "" & Brgy != " ") %>%
    complete(Year = 2010:2022, Week = 1:52) %>%
    mutate(Population = zoo::na.approx(Population, rule = 2, na.rm = FALSE)) %>%
    mutate(Population = ceiling(Population)) %>%
    ungroup()
  return(population_df)
}

approx_population.monthly <- function(population_data) {
  population_df <- population_data %>%
    select(Brgy = "BRGY", X2010:X2022) %>%
    pivot_longer(
      cols = X2010:X2022,
      names_to = "Year",
      values_to = "Population"
    ) %>%
    mutate(Year = as.numeric(gsub("X", "", Year)), Month = 1) %>%
    select(Year, Month, Population, Brgy) %>%
    group_by(Brgy) %>%
    filter(Brgy != "" & Brgy != " ") %>%
    complete(Year = 2010:2022, Month = 1:12) %>%
    mutate(Population = zoo::na.approx(Population, rule = 2, na.rm = FALSE)) %>%
    mutate(Population = ceiling(Population)) %>%
    ungroup()
  return(population_df)
}

approx_population.monthly.district <- function(population_data) {
  population_df <- population_data %>%
    select(DISTRICT, X2010:X2022) %>%
    pivot_longer(
      cols = X2010:X2022,
      names_to = "Year",
      values_to = "Population"
    ) %>%
    group_by(DISTRICT, Year) %>%
    summarise(across(c(Population), sum))%>%
    mutate(Year = as.numeric(gsub("X", "", Year)), Month = 1) %>%
    select(Year, Month, Population, DISTRICT) %>%
    group_by(DISTRICT) %>%
    filter(DISTRICT != "" & DISTRICT != " ") %>%
    complete(Year = 2010:2022, Month = 1:12) %>%
    mutate(Population = zoo::na.approx(Population, rule = 2, na.rm = FALSE)) %>%
    mutate(Population = ceiling(Population)) %>%
    ungroup()
  return(population_df)
}

preprocess_df2 <- function(base_df, cases_df, disease_type, rand_coords) {
  result_df <- base_df %>%
    mutate(DiseaseType = disease_type) %>%
    filter(Year <= 2018 & Year >= 2011) %>%
    left_join(cases_df, by = c(
      "Brgy" = "Brgy",
      "Week" = "Week",
      "Year" = "Year"
    )) %>%
    mutate(Cases = replace_na(Cases, 0)) %>%
    left_join(weather_data, by = c("Year" = "Year", "Week" = "MW")) %>%
    mutate(TMean = TMEAN, Humidity = RH, WindSpeed = WIND_SPEED) %>%
    bind_cols(rand_coords) %>%
    mutate(X = x, Y = y) %>%
    rowwise() %>%
    mutate(
      PopulationDensity = calc_population_density(Population, Brgy)
    ) %>%
    ungroup() %>%
    select(
      Year,
      Week,
      DiseaseType,
      X,
      Y,
      Cases,
      Rainfall,
      TMean,
      Humidity,
      WindSpeed,
      PopulationDensity
    ) %>%
    arrange(Year, Week)
  return(result_df)
}

preprocess_df2.monthly <- function(
    base_df,
    cases_df,
    disease_type,
    rand_coords,
    weather_data) {
  result_df <- base_df %>%
    mutate(DiseaseType = disease_type) %>%
    filter(Year <= 2018 & Year >= 2011) %>%
    left_join(cases_df, by = c(
      "Brgy" = "Brgy",
      "Month" = "Month",
      "Year" = "Year"
    )) %>%
    mutate(Cases = replace_na(Cases, 0)) %>%
    left_join(weather_data, by = c("Year" = "YEAR", "Month" = "MONTH")) %>%
    mutate(Humidity = RH) %>%
    bind_cols(rand_coords) %>%
    mutate(X = x, Y = y) %>%
    rowwise() %>%
    mutate(
      PopulationDensity = calc_population_density(Population, Brgy)
    ) %>%
    ungroup() %>%
    select(
      Year,
      Month,
      DiseaseType,
      X,
      Y,
      Cases,
      Rainfall,
      TMean,
      Humidity,
      WindSpeed,
      PopulationDensity
    ) %>%
    arrange(Year, Month)
  return(result_df)
}
