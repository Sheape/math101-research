# Utility functions for preprocessing and training.
library(stringr)
library(openxlsx)

source("labels.r")

import_year <- function(data, year) {
    year_data <- read.xlsx(data, sheet = year)
    return(year_data)
}

rename_barangays <- function(col, patterns, replacements) {
    cleaned_col <- str_squish(col)
    for (i in seq_along(patterns)) {
        cleaned_col <- gsub(patterns[i], replacements[i], cleaned_col)
    }
    return(cleaned_col)
}

count_cases <- function(disease_type,
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
            if (is.na(week_df[j, "CASECLASS"])) {
                next
            }
            if (toupper(disease_type) == "TYPHOID") {
                if (week_df[j, "CASECLASS"] != "SUSPECT") {
                    index <- match(
                        week_df[j, "Barangay"],
                        toupper(proper_brgy_list)
                    )
                    # For debugging only
                    ## print(sprintf("Week: %d", i))
                    ## print(week_df[j, "Barangay"])
                    ## print(sprintf("Index: %d", index))
                    if (is.na(index)) {
                        print(week_df[j, "Barangay"])
                        print(index)
                    }

                    if (!is.na(index)) {
                        row[index + 1] <- row[index + 1] + 1
                    }
                }
            } else if (toupper(disease_type) == "ABD") {
                if (toupper(week_df[j, "StoolCulture"]) == "POSITIVE") {
                    index <- match(
                        week_df[j, "Barangay"],
                        toupper(proper_brgy_list)
                    )
                    if (is.na(index)) {
                        print(week_df[j, "Barangay"])
                        print(index)
                    }
                    if (!is.na(index)) {
                        row[index + 1] <- row[index + 1] + 1
                    }
                }
            }
        }

        result_df <- rbind(result_df, row)
        colnames(result_df) <- cols
    }

    return(result_df)
}

convert_dates <- function(df) {
    df[, 3] <- as.Date(df[, 3], origin = "1899-12-30")
    df[, 4] <- as.Date(df[, 4], origin = "1899-12-30")
    return(df)
}
