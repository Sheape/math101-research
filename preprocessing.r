library(dplyr)
library(openxlsx)

source("utils.r")
source("cases.r")
source("labels.r")

#### Preprocessing ####
# Load the data
# MAKE SURE TO CREATE A COPY OF THE FILES FIRST

approx_df <- approx_population.monthly(population_data)
base_df <- population.district(approx_df)
rand_coords_df <- generate_rand_coords.district()
area_sqkm_df <- get_area_sqkm.district()
monthly_weather_df <- d2m_weather(daily_weather_data)

for (i in 11:18) {
  typhoid_df <- import_year(typhoid, paste0("20", i))
  abd_df <- import_year(abd, paste0("20", i))

  typhoid_cases <- count_typhoid_cases.monthly(typhoid_df)
  abd_cases <- count_abd_cases.monthly(abd_df)

  typhoid_district_cases <- count_to_district.monthly(typhoid_cases)
  abd_district_cases <- count_to_district.monthly(abd_cases)

  typhoid_cases_year <- typhoid_district_cases %>%
    mutate(Year = as.integer(paste0("20", i))) %>%
    select(Year, Month, everything())

  abd_cases_year <- abd_district_cases %>%
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
typhoid_final <- preprocess_df2.district(
  base_df,
  all_typhoid,
  "typhoid",
  rand_coords_df,
  monthly_weather_df,
  area_sqkm_df
)
abd_final <- preprocess_df2.district(
  base_df,
  all_abd,
  "abd",
  rand_coords_df,
  monthly_weather_df,
  area_sqkm_df
)

typhoid_final_morbidity <- cases_to_morbidity(typhoid_final, 50000)
abd_final_morbidity <- cases_to_morbidity(typhoid_final, 50000)

write.xlsx(typhoid_final, "Typhoid Preprocessed Stage 2 District Monthly (2011-2018).xlsx")
write.xlsx(abd_final, "ABD Preprocessed Stage 2 District Monthly (2011-2018).xlsx")
write.xlsx(typhoid_final_morbidity, "Typhoid Preprocessed Stage 2 District Monthly Morbidity per 50k (2011-2018).xlsx")
write.xlsx(abd_final_morbidity, "ABD Preprocessed Stage 2 District Monthly Morbidity per 50k (2011-2018).xlsx")
