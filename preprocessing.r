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
rand_coords_df_brgy <- generate_rand_coords(approx_df, is_monthly = TRUE)
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

  typhoid_cases_year_brgy <- typhoid_cases %>%
    mutate(Year = as.integer(paste0("20", i))) %>%
    select(Year, Month, everything())

  abd_cases_year_brgy <- abd_cases %>%
    mutate(Year = as.integer(paste0("20", i))) %>%
    select(Year, Month, everything())

  assign(paste0("typhoid", i, "_cases_brgy"), typhoid_cases_year_brgy)
  assign(paste0("abd", i, "_cases_brgy"), abd_cases_year_brgy)
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

all_typhoid_brgy <- bind_rows(
  typhoid11_cases_brgy,
  typhoid12_cases_brgy,
  typhoid13_cases_brgy,
  typhoid14_cases_brgy,
  typhoid15_cases_brgy,
  typhoid16_cases_brgy,
  typhoid17_cases_brgy,
  typhoid18_cases_brgy
)

all_abd_brgy <- bind_rows(
  abd11_cases_brgy,
  abd12_cases_brgy,
  abd13_cases_brgy,
  abd14_cases_brgy,
  abd15_cases_brgy,
  abd16_cases_brgy,
  abd17_cases_brgy,
  abd18_cases_brgy
)

typhoid_final_brgy <- preprocess_df2.monthly(
  approx_df,
  all_typhoid_brgy,
  "typhoid",
  rand_coords_df_brgy,
  monthly_weather_df
)
abd_final_brgy <- preprocess_df2.monthly(
  approx_df,
  all_abd_brgy,
  "abd",
  rand_coords_df_brgy,
  monthly_weather_df
)
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
abd_final_morbidity <- cases_to_morbidity(abd_final, 50000)
typhoid_final_brgy_morbidity <- cases_to_morbidity(typhoid_final_brgy, 50000)
abd_final_brgy_morbidity <- cases_to_morbidity(abd_final_brgy, 50000)

wb <- createWorkbook()
addWorksheet(wb, "Typhoid Monthly (Brgy)")
addWorksheet(wb, "ABD Monthly (Brgy)")
addWorksheet(wb, "Typhoid Monthly (Cases)")
addWorksheet(wb, "ABD Monthly (Cases)")
addWorksheet(wb, "Typhoid Monthly per 50k")
addWorksheet(wb, "ABD Monthly per 50k")
writeData(wb, "Typhoid Monthly (Brgy)", typhoid_final_brgy_morbidity)
writeData(wb, "ABD Monthly (Brgy)", abd_final_brgy_morbidity)
writeData(wb, "Typhoid Monthly (Cases)", typhoid_final)
writeData(wb, "ABD Monthly (Cases)", abd_final)
writeData(wb, "Typhoid Monthly per 50k", typhoid_final_morbidity)
writeData(wb, "ABD Monthly per 50k", abd_final_morbidity)
saveWorkbook(wb, "Preprocessed Stage 2 District Monthly (2011-2018).xlsx", overwrite = TRUE)
