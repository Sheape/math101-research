library(openxlsx)
library(dplyr)

typhoid_final <- read.xlsx("Typhoid Preprocessed Stage 2 (2011-2018).xlsx")
## abd_final <- read.xlsx("ABD Preprocessed Stage 2 (2011-2018).xlsx")

train_df <- typhoid_final
coords_df <- train_df %>%
    select(X, Y)

View(train_df %>% filter(Cases > 2))
