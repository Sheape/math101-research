library(dplyr)
library(openxlsx)

source("utils.r")
source("cases.r")
source("labels.r")

#### Preprocessing ####
# Load the data
# MAKE SURE TO CREATE A COPY OF THE FILES FIRST

base_df <- approx_population.monthly(population_data)
rand_coords_df <- generate_rand_coords(base_df, is_monthly = TRUE)
monthly_weather_df <- d2m_weather(daily_weather_data)

for (i in 11:18) {
  typhoid_df <- import_year(typhoid, paste0("20", i))
  abd_df <- import_year(abd, paste0("20", i))

  typhoid_cases <- count_typhoid_cases.monthly(typhoid_df)
  abd_cases <- count_abd_cases.monthly(abd_df)

  typhoid_cases_year <- typhoid_cases %>%
    mutate(Year = as.integer(paste0("20", i))) %>%
    select(Year, Month, everything())

  abd_cases_year <- abd_cases %>%
    mutate(Year = as.integer(paste0("20", i))) %>%
    select(Year, Month, everything())

  assign(paste0("typhoid", i, "_cases"), typhoid_cases_year)
  assign(paste0("abd", i, "_cases"), abd_cases_year)
}

all_typhoid <- bind_rows(
  typhoid11_cases,
  typhoid12_cases,
  typhoid13_cases,
  typhoid14_cases,
  typhoid15_cases,
  typhoid16_cases,
  typhoid17_cases,
  typhoid18_cases
)

all_abd <- bind_rows(
  abd11_cases,
  abd12_cases,
  abd13_cases,
  abd14_cases,
  abd15_cases,
  abd16_cases,
  abd17_cases,
  abd18_cases
)

## typhoid_final <- preprocess_df2(base_df, all_typhoid, "typhoid", rand_coords_df)
## abd_final <- preprocess_df2(base_df, all_abd, "abd", rand_coords_df)
typhoid_final <- preprocess_df2.monthly(
  base_df,
  all_typhoid,
  "typhoid",
  rand_coords_df,
  monthly_weather_df
)
abd_final <- preprocess_df2.monthly(
  base_df,
  all_abd,
  "abd",
  rand_coords_df,
  monthly_weather_df
)

write.xlsx(typhoid_final, "Typhoid Preprocessed Stage 2 Monthly (2011-2018).xlsx")
write.xlsx(abd_final, "ABD Preprocessed Stage 2 Monthly (2011-2018).xlsx")
