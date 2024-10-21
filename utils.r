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

count_cases <- function(cols,
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
            if (week_df[j, "CASECLASS"] != "SUSPECT") {
                # For debugging only
                index <- match(
                    week_df[j, "Barangay"],
                    toupper(proper_brgy_list)
                )
                print(sprintf("Week: %d", i))
                print(week_df[j, "Barangay"])
                print(sprintf("Index: %d", index))
                if (is.na(index)) {
                    print(week_df[j, "Barangay"])
                    print(index)
                }

                if (!is.na(index)) {
                    row[index + 1] <- row[index + 1] + 1
                }
            }
        }

        result_df <- rbind(result_df, row)
        colnames(result_df) <- cols
    }

    return(result_df)
}
