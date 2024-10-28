library(sf)
library(spatstat.geom)
library(spatstat.random)
library(dplyr)
library(tidyr)
library(zoo)

source("labels.r")

add_case <- function(df, idx, row, match_list) {
  index <- match(
    df[idx, "Barangay"],
    toupper(match_list)
  )

  if (!is.na(index)) {
    row[index + 1] <- row[index + 1] + 1
  }

  return(row)
}

count_cases <- function(
    disease_type,
    cols,
    init_values,
    brgy_list,
    proper_brgy_list,
    original_df) {
  result_df <- data.frame(matrix(ncol = length(cols), nrow = 0))
  colnames(result_df) <- cols

  for (i in 1:52) {
    week_df <- original_df[original_df$MorbidityWeek == i, ]

    row <- init_values
    row[1] <- i

    for (j in seq_len(nrow(week_df))) {
      if (toupper(disease_type) == "TYPHOID") {
        if (is.na(week_df[j, "CASECLASS"])) {
          next
        }
        if (week_df[j, "CASECLASS"] == "CONFIRMED" ||
          (week_df[j, "CASECLASS"] == "PROBABLE" &&
            week_df[j, "LabResult"] == "POSITIVE")) {
          row <- add_case(week_df, j, row, proper_brgy_list)
        }
      } else if (toupper(disease_type) == "ABD") {
        if (toupper(week_df[j, "StoolCulture"]) == "POSITIVE") {
          row <- add_case(week_df, j, row, proper_brgy_list)
        }
      }
    }

    result_df <- rbind(result_df, row)
    colnames(result_df) <- cols
  }

  return(result_df)
}

approx_population <- function(population_data) {
  population_df <- population_data %>%
    select(brgy = "BRGY", X2010:X2022) %>%
    pivot_longer(
      cols = X2010:X2022,
      names_to = "year",
      values_to = "population"
    ) %>%
    mutate(year = as.numeric(gsub("X", "", year)), week = 1) %>%
    select(year, week, population, brgy) %>%
    group_by(brgy) %>%
    filter(brgy != "" & brgy != " ") %>%
    complete(year = 2010:2022, week = 1:52) %>%
    mutate(population = zoo::na.approx(population, rule = 2, na.rm = FALSE)) %>%
    mutate(population = ceiling(population)) %>%
    ungroup()
  return(population_df)
}


get_week_population <- function(brgy_name, year, week, interpolated_pop) {
  weekly_pop <- interpolated_pop %>%
    filter(brgy == brgy_name, year == year, week == week) %>%
    pull(population)
  return(weekly_pop)
}

calc_population_density <- function(population, area) {
  return(population / area)
}

calc_x_y <- function(short_brgy_name, geodata) {
  index <- match(
    short_brgy_name,
    geodata$ADM4_SHORT
  )

  wkt_str <- geodata[index, "WKT"]
  multi_polygon <- st_as_sfc(wkt_str)
  sampling_region <- as.owin(multi_polygon)
  random_point <- runifpoint(1, win = sampling_region)
  coords <- c(random_point$x, random_point$y)
  return(coords)
}

parse_table <- function(df, disease_type, year) {
  pop_data <- approx_population(population_data)
  cols <- c(
    "Year", "Week", "DiseaseType", "X", "Y", "Cases", "Rainfall",
    "TMean", "Humidity", "WindSpeed", "Population",
    "PopulationDensity"
  )
  result_df <- data.frame(matrix(ncol = 12, nrow = 0))

  colnames(result_df) <- cols
  for (i in 1:52) {
    week_df <- df[df$MorbidityWeek == i, ]
    for (j in seq_len(2:ncol(week_df))) {
      row <- c(
        year, i, disease_type, 0, 0, 0, 0, 0, 0, 0, 0
      )
      row[4:5] <- calc_x_y(week_df$Barangay[j], geodata)
      row[6] <- sum(week_df[j])
      row[7] <- weather_data$Rainfall[weather_data$MW == i]
      row[8] <- weather_data$TMEAN[weather_data$MW == i]
      row[9] <- weather_data$RH[weather_data$MW == i]
      row[10] <- weather_data$WIND_SPEED[weather_data$MW == i]
      row[11] <- get_week_population(
        week_df$Barangay[j],
        year,
        i,
        pop_data
      )
      row[12] <- calc_population_density(row[11], 0.5)
      result_df <- rbind(result_df, row)
      colnames(result_df) <- cols
    }
  }
}
