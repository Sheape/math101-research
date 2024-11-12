# Utility functions for preprocessing and training.
library(stringr)
library(openxlsx)
library(tools)
library(dplyr)
library(spdep)
library(sf)

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

get_district.geom <- function() {
  result_df <- geodata %>%
    select(Brgy = ADM4_EN, District = DISTRICT, everything()) %>%
    group_by(District) %>%
    summarise(MergedGeometry = st_union(st_as_sfc(WKT)))

  return(result_df)
}

generate_rand_coords.district <- function(is_monthly = TRUE) {
  if (is_monthly) {
    n_points <- 12 * 8
  } else {
    n_points <- 52 * 8
  }

  district_geom_df <- get_district.geom()

  rand_points.district <- district_geom_df %>%
    mutate(RandCoords = map(MergedGeometry, ~ calc_x_y.any(.x, n_points))) %>%
    mutate(
      X = map(RandCoords, ~ as.data.frame(.) %>% pull(x)),
      Y = map(RandCoords, ~ as.data.frame(.) %>% pull(y))
    ) %>%
    unnest(c(X, Y)) %>%
    select(X, Y)

  return(rand_points.district)
}

extract_moran_components <- function(moran_test) {
  data.frame(
    standard_deviate = moran_test$statistic,
    p_value = moran_test$p.value,
    moran_I = moran_test$estimate[1],
    expectation = moran_test$estimate[2],
    variance = moran_test$estimate[3], # Not directly available, typically part of the test
    range = paste(moran_test$range1, moran_test$range2, sep = " - ")
  )
}

global_moran_i_temporal <- function(df, variable, queen = FALSE, district = TRUE) {
  if (district) {
    district_geom_df <- geodata %>%
      select(Brgy = ADM4_EN, District = DISTRICT, everything()) %>%
      group_by(District) %>%
      summarise(geom = st_union(st_as_sfc(WKT)))

    df_geom <- df %>%
      left_join(district_geom_df, by = "District")
  } else {
    brgy_geom <- st_read("geodata/baguio_city_2023.shp")
    brgy_geom_df <- brgy_geom %>%
      select(Brgy = ADM4_EN, geom = geometry)
    df_geom <- df %>%
      left_join(brgy_geom_df, by = c("Brgy" = "ADM4_EN"))
  }

  st_obj <- st_as_sf(df_geom)

  moransi_results <- list()
  for (year in 2011:2018) {
    for (month in 1:12) {
      tryCatch(
        {
          current_data <- st_obj %>%
            filter(Year == year, Month == month)

          # Check if all values are zero
          if (all(current_data[[variable]] == 0)) {
            message(paste("All values are zero for year", year, "month", month, "- skipping"))
            next
          }

          nb <- poly2nb(current_data, queen = queen)
          swm <- nb2listw(nb)

          m_range <- moran_range(swm)

          if (nrow(current_data) > 0) {
            moran_test <- moran.test(x = current_data[[variable]], listw = swm, randomisation = TRUE)
            moransi_results[[paste(year, month, sep = "-")]] <- c(moran_test, range = m_range)
          }
        },
        error = function(e) {
          message(paste("Error in year", year, "month", month, ":", e$message))
        }
      )
    }
  }

  moran_df <- do.call(rbind, lapply(moransi_results, extract_moran_components))
  moran_df$date <- rownames(moran_df)
  rownames(moran_df) <- NULL
  result_df <- moran_df %>%
    select(date, everything())
  return(result_df)
}

moran_range <- function(listw) {
  mat <- listw2mat(listw)
  tmat <- t(mat)
  temp <- (mat + tmat) / 2
  eigenvalues <- eigen(temp)$values
  return(range(eigenvalues))
}
