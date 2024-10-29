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

cleanup_df <- function(df) {
  result_df <- df %>%
    mutate(
      DAdmit = as.Date(DAdmit, origin = "1899-12-30"),
      DOnset = as.Date(DOnset, origin = "1899-12-30")
    ) %>%
    mutate(Brgy = rename_brgys(.$Barangay, patterns, replacements)) %>%
    mutate(Week = MorbidityWeek)
  return(result_df)
}

## parse_table <- function(df, disease_type, year)
