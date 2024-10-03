library(openxlsx)
library(dplyr)
library(stringr)

# Load the data
# MAKE SURE TO CREATE A COPY OF THE FILES FIRST
typhoid <- "Typhoid Fever (2011 - 2018).xlsx"
abd <- "Acute Bloody Diarrhea (2011 - 2018).xlsx"
geodata <- read.csv("geodata/baguio_city_geodata.csv")

import_year <- function(data, year) {
    year_data <- read_excel(data, sheet = year)
    return(year_data)
}

#### Preprocessing ####
typhoid11 <- import_year(abd, "2011")
## FIXME: Convert numeric into Date
## typhoid11[, 3:4] <- as.Date(typhoid11[, 3:4])
cleanup_data <- function(col, patterns, replacements) {
    cleaned_col <- str_squish(col)
    for (i in seq_along(patterns)) {
        cleaned_col <- gsub(patterns[i], replacements[i], cleaned_col)
    }
    return(cleaned_col)
}

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
    "^Roman Ayson$"
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
    as.character(geodata[108, 2]),
    as.character(geodata[53, 2]),
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
    "Campo Filipino"
))

cleaned_typhoid11 <- cleanup_data(typhoid11$Barangay, patterns, replacements)
print(unique(cleaned_typhoid11))

not_typhoid11 <- typhoid11[
    !(cleaned_typhoid11 %in% toupper(geodata$ADM4_EN)),
]
print(unique(not_typhoid11$Barangay))

View(not_typhoid11)
warnings()
## print(toupper(geodata$ADM4_EN))
