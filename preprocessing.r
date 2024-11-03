library(dplyr)

source("utils.r")
source("labels.r")

# Load the data
# MAKE SURE TO CREATE A COPY OF THE FILES FIRST
typhoid <- "Typhoid Fever (2011 - 2018).xlsx"
abd <- "Acute Bloody Diarrhea (2011 - 2018).xlsx"
geodata <- read.csv("geodata/baguio_city_geodata.csv")

<<<<<<< Updated upstream

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
=======
base_df <- approx_population.monthly.district(population_data)
rand_coords_df <- generate_rand_coords(base_df, is_monthly = TRUE)
monthly_weather_df <- d2m_weather(daily_weather_data)
>>>>>>> Stashed changes

for (i in 11:18) {
    typhoid <- get(paste0("typhoid", i))
    abd <- get(paste0("abd", i))

<<<<<<< Updated upstream
    typhoid <- convert_dates(typhoid)
    abd <- convert_dates(abd)
=======
  typhoid_cases <- count_typhoid_cases.monthly.district(typhoid_df)
  abd_cases <- count_abd_cases.monthly.district(abd_df)
>>>>>>> Stashed changes

    cleaned_typhoid <- rename_barangays(typhoid$Barangay, patterns, replacements)
    typhoid$Barangay <- cleaned_typhoid

    cleaned_abd <- rename_barangays(abd$Barangay, patterns, replacements)
    abd$Barangay <- cleaned_abd

    df_cleaned_typhoid <- typhoid[
        cleaned_typhoid %in% toupper(geodata$ADM4_EN),
    ]

    df_cleaned_typhoid <- df_cleaned_typhoid[!is.na(df_cleaned_typhoid$Barangay),]

    not_typhoid <- typhoid[
        !(cleaned_typhoid %in% toupper(geodata$ADM4_EN)),
    ]

    df_cleaned_abd <- abd[
        cleaned_abd %in% toupper(geodata$ADM4_EN),
    ]

    df_cleaned_abd <- df_cleaned_abd[!is.na(df_cleaned_abd$Barangay),]

    not_abd <- abd[
        !(cleaned_abd %in% toupper(geodata$ADM4_EN)),
    ]

    cases_typhoid <- count_cases(
        "typhoid",
        column_names,
        init_values,
        short_brgy_names,
        proper_brgy_names,
        df_cleaned_typhoid
    )

    assign(paste0("df_cleaned_typhoid", i), df_cleaned_typhoid)
    assign(paste0("not_typhoid", i), not_typhoid)
    assign(paste0("df_cleaned_abd", i), df_cleaned_abd)
    assign(paste0("not_abd", i), not_abd)

    # print(paste0("Typhoid ", i))
    # print(sum(rowSums(df_cleaned_typhoid[, -1])))
    # print(paste0("ABD ", i))
    # print(sum(rowSums(df_cleaned_abd[, -1])))

    # View(not_typhoid)
    # View(not_abd)
    # View(df_cleaned_typhoid)
    # View(df_cleaned_abd)
}

typhoid11 <- convert_dates(typhoid11)

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


# #### Typhoid 2011 ####
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
