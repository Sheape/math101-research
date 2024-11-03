# Install packages
# install.packages("readxl")
# install.packages("dplyr")
#install.packages("tidygeocoder")

# Load Readxl Library
# library(readxl)
# library(dplyr)
#library(tidygeocoder)
#library(ggplot2)

options(tibble.width = Inf)

<<<<<<< Updated upstream
get_cases_per_year = function(file_path) {
    excel_file <- file_path
    sheets <- excel_sheets(excel_file)
=======
get_cases_per_year <- function(file_path) {
  excel_file <- file_path
  sheets <- excel_sheets(excel_file) #what this is
>>>>>>> Stashed changes

    cases_per_year <- data.frame(
        Sheet = character(),
        RowCount = numeric(),
        stringsAsFactors = FALSE
    )

    for (sheet in sheets) {
        df <- read_excel(excel_file, sheet = sheet)
        cases_per_year <- rbind(cases_per_year, data.frame(Sheet = sheet, RowCount = nrow(df)))
    }

    return(cases_per_year)
}

# Load Typhoid Fever Data from Excel
plot_line <- function(file_path, title) {
    cases_per_year <- get_cases_per_year(file_path)

    ggplot(cases_per_year, aes(x = Sheet, y = RowCount, group = 1)) +
        geom_line(color = "red", linewidth = 1) +
        geom_point(size = 3) +
        labs(title = title,
             x = "Year",
             y = "Number of Cases") +
        theme_minimal()
}

plot_box <- function(file_path, title) {
    cases_per_year <- get_cases_per_year(file_path)
    print(cases_per_year)

    boxplot(cases_per_year$RowCount, main = title, ylab = "Number of Cases")
}

### Charts ###
typhoid_excel <- "Typhoid Fever (2011 - 2018).xlsx"
abd_excel <- "Acute Bloody Diarrhea (2011 - 2018).xlsx"

plot_line(typhoid_excel, "Typhoid Fever Cases (2011 - 2018)")
plot_line(abd_excel, "Acute Bloody Diarrhea Cases (2011 - 2018)")
plot_box(typhoid_excel, "Typhoid Fever Cases (2011 - 2018)")
plot_box(abd_excel, "Acute Bloody Diarrhea Cases (2011 - 2018)")
