# Utility functions for preprocessing and training.
library(stringr)
library(openxlsx)
library(tools)
library(dplyr)

source("labels.r")


#### Main Functions ####

import_year <- function(data, year) {
  year_data <- read.xlsx(data, sheet = year)
  return(year_data)
}

convert_dates <- function(df) {
  df[, 3] <- as.Date(df[, 3], origin = "1899-12-30")
  df[, 4] <- as.Date(df[, 4], origin = "1899-12-30")
  return(df)
}

rename_brgys <- function(col, patterns, replacements) {
  cleaned_col <- str_squish(col)
  for (i in seq_along(patterns)) {
    cleaned_col <- gsub(patterns[i], replacements[i], cleaned_col)
  }
  return(cleaned_col)
}

rename_labresults <- function(col) {
  cleaned_col <- str_squish(col)
  sub_col <- gsub("^POSITIVE T$", "POSITIVE", toupper(cleaned_col))
  return(sub_col)
}

cleanup_df <- function(df, is_monthly = FALSE) {
  temp_df <- df %>%
    mutate(
      DAdmit = as.Date(DAdmit, origin = "1899-12-30"),
      DOnset = as.Date(DOnset, origin = "1899-12-30")
    ) %>%
    mutate(Brgy = rename_brgys(.$Barangay, patterns, replacements))

  if (is_monthly) {
    result_df <- temp_df %>%
      mutate(Month = MorbidityMonth)
  } else {
    result_df <- temp_df %>%
      mutate(Week = MorbidityWeek)
  }

  return(result_df)
}

d2m_weather <- function(daily_weather_df) {
  result_df <- daily_weather_df %>%
    group_by(YEAR, MONTH) %>%
    summarise(
      Rainfall = mean(RAINFALL, na.rm = TRUE),
      TMax = mean(TMAX, na.rm = TRUE),
      TMin = mean(TMIN, na.rm = TRUE),
      TMean = mean(TMEAN, na.rm = TRUE),
      RH = mean(RH, na.rm = TRUE),
      WindSpeed = mean(WIND_SPEED, na.rm = TRUE)
    )
  return(result_df)
}

calc_population_density <- function(population, brgy_name) {
  index <- match(
    brgy_name,
    geodata$ADM4_EN
  )

  area <- geodata[index, "AREA_SQKM"]

  return(population / area)
}

calc_x_y <- function(brgy_name, n_points) {
  index <- match(
    brgy_name,
    geodata$ADM4_EN
  )

  wkt_str <- geodata[index, "WKT"]
  multi_polygon <- st_as_sfc(wkt_str)
  sampling_region <- as.owin(multi_polygon)
  set.seed(112524)
  random_points <- runifpoint(n_points, win = sampling_region)
  return(random_points)
}

generate_rand_coords <- function(base_df, is_monthly = FALSE) {
  if (is_monthly) {
    n_points <- 12 * 8
  } else {
    n_points <- 52 * 8
  }

  rand_coords <- base_df %>%
    filter(Year <= 2018 & Year >= 2011) %>%
    group_by(Brgy) %>%
    summarise(RandCoords = list(calc_x_y(Brgy, n_points)), .groups = "drop") %>%
    mutate(
      X = map(RandCoords, ~ as.data.frame(.) %>% select(x)),
      Y = map(RandCoords, ~ as.data.frame(.) %>% select(y))
    ) %>%
    unnest(c(X, Y)) %>%
    select(-RandCoords, -Brgy)
  return(rand_coords)
}
