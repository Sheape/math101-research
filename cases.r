library(sf)
library(spatstat.geom)
library(spatstat.random)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo)

source("labels.r")
source("utils.r")

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
      na.rm = TRUE
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

count_to_district.monthly <- function(cases_df) {
  result_df <- cases_df %>%
    rowwise() %>%
    mutate(District = get_district(Brgy)) %>%
    group_by(District, Month) %>%
    summarise(Cases = sum(Cases)) %>%
    ungroup()
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
    mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    mutate(Month = if_else(Year == 2010, 5, 4)) %>%
    select(Year, Month, Population, Brgy) %>%
    group_by(Brgy) %>%
    filter(Brgy != "" & Brgy != " ") %>%
    complete(Year = 2010:2022, Month = 1:12) %>%
    mutate(Population = if_else(
      is.na(Population),
      estimate_population(Brgy, ((Year - 2010) * 12) + Month),
      Population
    )) %>%
    mutate(Population = ceiling(Population)) %>%
    ungroup()
  return(population_df)
}

# Currently only works with monthly
# TODO: Add support for weekly
estimate_population <- function(brgy, t, is_monthly = FALSE) {
  # Ensure brgy and t are vectors of the same length
  if (length(brgy) != length(t)) {
    stop("brgy and t must be vectors of the same length")
  }

  # Prepare population data by converting columns to integer
  population_data.local <- population_data %>%
    mutate(
      X2010 = as.integer(X2010),
      X2015 = as.integer(X2015),
      X2020 = as.integer(X2020),
      X2021 = as.integer(X2021),
      X2022 = as.integer(X2022)
    )
  if (is_monthly) {

  }

  # Vectorized function to estimate population for one brgy and one t
  estimate_for_one <- function(brgy_single, t_single) {
    if (is.na(brgy_single)) {
      return(NA)
    }

    index <- match(brgy_single, population_data.local$BRGY)

    if (is.na(index)) {
      return(NA)
    }

    pop_i <- pop_f <- t_i <- t_f <- NA

    if (t_single <= 5) {
      return(population_data.local[index, "X2010"])
    } else if (t_single == 64) {
      return(population_data.local[index, "X2015"])
    } else if (t_single == 124) {
      return(population_data.local[index, "X2020"])
    } else if (t_single == 136) {
      return(population_data.local[index, "X2021"])
    } else if (t_single == 144) {
      return(population_data.local[index, "X2022"])
    } else if (t_single > 5 && t_single < 64) {
      t_i <- 5
      t_f <- 64
      pop_i <- population_data.local[index, "X2010"]
      pop_f <- population_data.local[index, "X2015"]
    } else if (t_single > 64 && t_single < 124) {
      t_i <- 64
      t_f <- 124
      pop_i <- population_data.local[index, "X2015"]
      pop_f <- population_data.local[index, "X2020"]
    } else if (t_single > 124 && t_single < 136) {
      t_i <- 124
      t_f <- 136
      pop_i <- population_data.local[index, "X2020"]
      pop_f <- population_data.local[index, "X2021"]
    } else if (t_single > 136) {
      t_i <- 136
      t_f <- 148
      pop_i <- population_data.local[index, "X2021"]
      pop_f <- population_data.local[index, "X2022"]
    }

    if (!is.na(pop_i) && !is.na(pop_f)) {
      growth_rate <- exp(log(pop_f / pop_i) / (t_f - t_i))
      return(pop_i * (growth_rate^(t_single - t_i)))
    } else {
      return(NA)
    }
  }

  # Apply the function over vectors
  mapply(estimate_for_one, brgy, t)
}

cases_to_district <- function(cases_df) {
  result_df <- cases_df %>%
    mutate(District = get_district(Brgy)) %>%
    group_by(District, Month, Cases) %>%
    ungroup()
  return(result_df)
}

population.district <- function(approx_df) {
  result_df <- approx_df %>%
    rowwise() %>%
    mutate(District = get_district(Brgy)) %>%
    group_by(District, Year, Month) %>%
    summarise(Population = sum(Population))
  return(result_df)
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

preprocess_df2.district <- function(
    base_df,
    cases_df,
    disease_type,
    rand_coords,
    weather_data,
    area_sqkm_df) {
  result_df <- base_df %>%
    mutate(DiseaseType = disease_type) %>%
    filter(Year <= 2018 & Year >= 2011) %>%
    ungroup() %>%
    left_join(cases_df, by = c("District" = "District", "Year" = "Year", "Month" = "Month")) %>%
    mutate(Cases = replace_na(Cases, 0)) %>%
    group_by(District) %>%
    left_join(weather_data, by = c("Year" = "YEAR", "Month" = "MONTH")) %>%
    ungroup() %>%
    mutate(Humidity = RH) %>%
    bind_cols(rand_coords) %>%
    rowwise() %>%
    mutate(
      PopulationDensity = calc_population_density.district(Population, District, area_sqkm_df)
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
      Population,
      PopulationDensity
    )
  return(result_df)
}

cases_to_morbidity <- function(df, per_pop = 100000) {
  result_df <- df %>%
    mutate(MorbidityRate = calc_morbidity_rate(Cases, Population, per_pop))
  return(result_df)
}
