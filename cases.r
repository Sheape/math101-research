library(sf)
library(spatstat.geom)
library(spatstat.random)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)

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

calc_population_density <- function(population, brgy_name) {
  index <- match(
    brgy_name,
    geodata$ADM4_EN
  )

  area <- geodata[index, "AREA_SQKM"]

  return(population / area)
}

calc_x_y <- function(brgy_name) {
  index <- match(
    brgy_name,
    geodata$ADM4_EN
  )

  wkt_str <- geodata[index, "WKT"]
  multi_polygon <- st_as_sfc(wkt_str)
  sampling_region <- as.owin(multi_polygon)
  set.seed(112524)
  random_points <- runifpoint(416, win = sampling_region)
  return(random_points)
}

generate_rand_coords <- function(base_df) {
  rand_coords <- base_df %>%
    filter(Year <= 2018 & Year >= 2011) %>%
    group_by(Brgy) %>%
    summarise(RandCoords = list(calc_x_y(Brgy)), .groups = "drop") %>%
    mutate(
      X = map(RandCoords, ~ as.data.frame(.) %>% select(x)),
      Y = map(RandCoords, ~ as.data.frame(.) %>% select(y))
    ) %>%
    unnest(c(X, Y)) %>%
    select(-RandCoords, -Brgy)
  return(rand_coords)
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
      Population,
      PopulationDensity
    )
  return(result_df)
}
