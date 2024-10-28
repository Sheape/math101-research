library(dplyr)

source("utils.r")
source("cases.r")
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

column_names <- c("MorbidityWeek", short_brgy_names)
init_values <- numeric(130)

for (i in 11:18) {
  typhoid <- get(paste0("typhoid", i))
  abd <- get(paste0("abd", i))

  typhoid <- convert_dates(typhoid)
  abd <- convert_dates(abd)

  cleaned_typhoid <- rename_barangays(typhoid$Barangay, patterns, replacements)
  typhoid$Barangay <- cleaned_typhoid
  cleaned_labresults <- rename_labresults(typhoid$LabResult)
  typhoid$LabResult <- cleaned_labresults

  cleaned_abd <- rename_barangays(abd$Barangay, patterns, replacements)
  abd$Barangay <- cleaned_abd

  df_cleaned_typhoid <- typhoid[
    cleaned_typhoid %in% toupper(geodata$ADM4_EN),
  ]

  df_cleaned_typhoid <- df_cleaned_typhoid[!is.na(df_cleaned_typhoid$Barangay), ]

  df_cleaned_abd <- abd[
    cleaned_abd %in% toupper(geodata$ADM4_EN),
  ]

  df_cleaned_abd <- df_cleaned_abd[!is.na(df_cleaned_abd$Barangay), ]

  assign(paste0("df_cleaned_typhoid", i), df_cleaned_typhoid)
  assign(paste0("df_cleaned_abd", i), df_cleaned_abd)

  typhoid_cases <- count_cases(
    "typhoid",
    column_names,
    init_values,
    short_brgy_names,
    proper_brgy_names,
    df_cleaned_typhoid
  )

  abd_cases <- count_cases(
    "abd",
    column_names,
    init_values,
    short_brgy_names,
    proper_brgy_names,
    df_cleaned_abd
  )

  assign(paste0("typhoid", i, "_cases"), typhoid_cases)
  assign(paste0("abd", i, "_cases"), abd_cases)
}

preproc_typhoid11 <- parse_table(df_cleaned_typhoid11, "typhoid", 2011)

# Legacy
typhoid11 <- convert_dates(typhoid11)

cleaned_typhoid11 <- rename_barangays(typhoid11$Barangay, patterns, replacements)
typhoid11$Barangay <- cleaned_typhoid11
cleaned_labresults11 <- rename_labresults(typhoid11$LabResult)
typhoid11$LabResult <- cleaned_labresults11

df_cleaned_typhoid11 <- typhoid11[
  cleaned_typhoid11 %in% toupper(geodata$ADM4_EN),
]

df_cleaned_typhoid11 <- df_cleaned_typhoid11[!is.na(df_cleaned_typhoid11$Barangay), ]

not_typhoid11 <- typhoid11[
  !(cleaned_typhoid11 %in% toupper(geodata$ADM4_EN)),
]



#### Typhoid 2011 ####
column_names <- c("MorbidityWeek", short_brgy_names)
init_values <- numeric(130)

typhoid11_df_cases <- count_cases(
  "typhoid",
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
