# This file is associated with replacements, patterns, and names for each
# barangay.

<<<<<<< Updated upstream
geodata <- read.csv("geodata/baguio_city_geodata.csv")
=======
typhoid <- "Typhoid Fever (2011-2018).xlsx"
abd <- "Acute Bloody Diarrhea (2011-2018).xlsx"
geodata <- read.csv("geodata/baguio_city_geodata.csv")
population_data <- read.csv("geodata/baguio_city_population.csv")
weather_data <- read.csv("geodata/baguio_city_weather_data.csv")
daily_weather_data <- read.csv("geodata/baguio_city_daily_weather_data.csv")

population_data$X2020 <- as.integer(gsub(
  ",",
  "",
  population_data$X2020
))
>>>>>>> Stashed changes

population_data_district = select(population_data, Brgy = BRGY, DISTRICT)

patterns <- toupper(c(
    "^Sto Tomas Proper$",
    "^Sto. Tomas Proper$",
    "^Lower Bokawkan$",
    "^Bakakeng Norte$",
    "^Bakakeng$",
    "^Dominican -Mirador Hill$",
    "^Harrison-Claudio$",
    "^Sta Escolastica Village$",
    "^Happy Homes-Campo Sioco$",
    "^Aurora Hill Proper$",
    "^Dizon Subd$",
    "^Quirino Hill Lower$",
    "^Modernsite East$",
    "^Quezon Hill Upper$",
    "^Quezon Hill$",
    "^St Joseph Village$",
    "^Sanitary Camp North$",
    "^Lourdes Subdivision Extention$",
    "^Palma- Urbano$",
    "^Dagsian Upper$",
    "^Upper Market$",
    "^Honeymoon- Holyghost$",
    "^AZCKO$",
    "^Quirino-Magsaysay,$",
    "^Dominican Hil-Mirador Hill$",
    "^Andres Bonifacio$",
    "^Lourdes Extension$",
    "^Ferguson$",
    "^Roman Ayson Road$",
    "^Engineer\'s Hill$",
    "^General Emilio Aguinaldo$",
    "^Quezon Hill, Middle$",
    "^Modernsite, East$",
    "^General Emilio Aguinaldo \\(Lower QM\\)$",
    "^DPS Compound$",
    "^Holyghost Extension$",
    "^Ferdinand Happy Homes \\(Campo Sioco\\)$",
    "^Burnham-Legarda-Kisad$",
    "^Imelda Marcos$",
    "^Loakan-Apugan$",
    "^Bayan Park, West$",
    "^Camp$",
    "^Navy Base$",
    "^Atab$",
    "^Roman Ayson$",
    "^Happy Homes-Lucban$",
    "^Dominican- Mirador Hill$"
))

replacements <- toupper(c(
    "Santo Tomas Proper",
    "Santo Tomas Proper",
    "Andres Bonifacio (Lower Bokawkan)",
    "Bakakeng North",
    "Bakakeng Central",
    "Dominican Hill-Mirador",
    "Harrison-Claudio Carantes",
    "Santa Escolastica",
    "Ferdinand (Happy Homes-Campo Sioco)",
    "Aurora Hill Proper (Malvar-Sgt. Floresca)",
    "Dizon Subdivision",
    "Quirino Hill, Lower",
    "Modern Site, East",
    "Quezon Hill, Upper",
    "Quezon Hill Proper",
    "Saint Joseph Village",
    "Sanitary Camp, North",
    "Lourdes Subdivision Extension",
    "Palma-Urbano (Cariño-Palma)",
    "Dagsian, Upper",
    "Market Subdivision, Upper",
    "Honeymoon (Honeymoon-Holy Ghost)",
    "Abanao-Zandueta-Kayong-Chugum-Otek (AZKCO)",
    "General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)",
    "Dominican Hill-Mirador",
    "Andres Bonifacio (Lower Bokawkan)",
    "Lourdes Subdivision Extension",
    "Guisad Sorong",
    "Campo Filipino",
    "Engineers' Hill",
    "General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)",
    "Middle Quezon Hill Subdivision(Quezon Hill Middle)",
    "Modern Site, East",
    "General Emilio F. Aguinaldo (Quirino-Magsaysay, Lower)",
    "DPS Area",
    "Holy Ghost Extension",
    "Ferdinand (Happy Homes-Campo Sioco)",
    "Legarda-Burnham-Kisad",
    "Imelda R. Marcos (La Salle)",
    "Apugan-Loakan",
    "Bayan Park West (Bayan Park)",
    "City Camp Proper",
    "Saint Joseph Village",
    "Santo Tomas Proper",
    "Campo Filipino",
    "Happy Homes-Lucban",
    "Dominican Hill-Mirador"
))

proper_brgy_names <- geodata$ADM4_EN
short_brgy_names <- geodata$ADM4_SHORT
