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

get_area_sqkm.district <- function() {
  area_sqkm <- geodata %>%
    group_by(DISTRICT) %>%
    summarise(AREA_SQKM = sum(AREA_SQKM))
  return(area_sqkm)
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
  index <- brgy_match(brgy_name)
  area <- geodata[index, "AREA_SQKM"]

  return(population / area)
}

calc_population_density.district <- function(population, district, district_df) {
  index <- match(district, district_df$DISTRICT)
  area <- district_df[index, "AREA_SQKM"]

  return(as.numeric(population / area))
}

calc_x_y.any <- function(spat_obj, n_points) {
  sampling_region <- as.owin(spat_obj)
  set.seed(112524)
  random_points <- runifpoint(n_points, win = sampling_region)
  return(random_points)
}

calc_x_y <- function(brgy_name, n_points) {
  index <- brgy_match(brgy_name)

  wkt_str <- geodata[index, "WKT"]
  multi_polygon <- st_as_sfc(wkt_str)
  random_points <- calc_x_y.any(multi_polygon, n_points)
  return(random_points)
}

calc_morbidity_rate <- function(cases, population, per_pop = 10000) {
  return((cases / population) * per_pop)
}

brgy_match <- function(brgy) {
  index <- match(
    brgy,
    geodata$ADM4_EN
  )

  return(index)
}

district_match <- function(district) {
  index <- match(
    district,
    geodata$DISTRICT
  )

  return(index)
}

get_district <- function(brgy) {
  index <- brgy_match(brgy)
  district <- geodata[index, "DISTRICT"]
  return(district)
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

generate_rand_coords.district <- function(is_monthly = TRUE) {
  if (is_monthly) {
    n_points <- 12 * 8
  } else {
    n_points <- 52 * 8
  }

  rand_points.district <- geodata %>%
    select(Brgy = ADM4_EN, District = DISTRICT, everything()) %>%
    group_by(District) %>%
    summarise(MergedGeometry = st_union(st_as_sfc(WKT))) %>%
    mutate(RandCoords = map(MergedGeometry, ~ calc_x_y.any(.x, n_points))) %>%
    mutate(
      X = map(RandCoords, ~ as.data.frame(.) %>% pull(x)),
      Y = map(RandCoords, ~ as.data.frame(.) %>% pull(y))
    ) %>%
    unnest(c(X, Y)) %>%
    select(X, Y)

  return(rand_points.district)
}
