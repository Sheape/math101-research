library(dplyr)

source("utils.r")
source("labels.r")

# Load the data
# MAKE SURE TO CREATE A COPY OF THE FILES FIRST
typhoid <- "Typhoid Fever (2011 - 2018).xlsx"
abd <- "Acute Bloody Diarrhea (2011 - 2018).xlsx"
geodata <- read.csv("geodata/baguio_city_geodata.csv")


#### Preprocessing ####
typhoid11 <- import_year(typhoid, "2011")
typhoid12 <- import_year(typhoid, "2012")
typhoid13 <- import_year(typhoid, "2013")
typhoid14 <- import_year(typhoid, "2014")
typhoid15 <- import_year(typhoid, "2015")
typhoid16 <- import_year(typhoid, "2016")
typhoid17 <- import_year(typhoid, "2017")
typhoid18 <- import_year(typhoid, "2018")

abd11 <- import_year(abd, "2011")
abd12 <- import_year(abd, "2012")
abd13 <- import_year(abd, "2013")
abd14 <- import_year(abd, "2014")
abd15 <- import_year(abd, "2015")
abd16 <- import_year(abd, "2016")
abd17 <- import_year(abd, "2017")
abd18 <- import_year(abd, "2018")

typhoid11[, 3] <- as.Date(typhoid11[, 3], origin = "1899-12-30")
typhoid11[, 4] <- as.Date(typhoid11[, 4], origin = "1899-12-30")

cleaned_typhoid11 <- rename_barangays(typhoid11$Barangay, patterns, replacements)
typhoid11$Barangay <- cleaned_typhoid11

df_cleaned_typhoid11 <- typhoid11[
    cleaned_typhoid11 %in% toupper(geodata$ADM4_EN),
]

df_cleaned_typhoid11 <- df_cleaned_typhoid11[!is.na(df_cleaned_typhoid11$Barangay),]

not_typhoid11 <- typhoid11[
    !(cleaned_typhoid11 %in% toupper(geodata$ADM4_EN)),
]

View(not_typhoid11)
View(df_cleaned_typhoid11)


#### Typhoid 2011 ####
column_names <- c("MorbidityWeek", short_brgy_names)
init_values <- numeric(130)

typhoid11_df_cases <- count_cases(
  column_names,
  init_values,
  short_brgy_names,
  proper_brgy_names,
  df_cleaned_typhoid11
)

row_sum <- rowSums(typhoid11_df_cases[, -1])
print(row_sum)
print(sum(row_sum))
View(typhoid11_df_cases)

