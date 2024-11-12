library(openxlsx)

source("utils.r")

all_typhoid <- read.xlsx("Preprocessed Stage 2 District Monthly (2011-2018).xlsx", sheet = "Typhoid Monthly per 50k")
all_abd <- read.xlsx("Preprocessed Stage 2 District Monthly (2011-2018).xlsx", sheet = "ABD Monthly per 50k")

# Rook's Case
typhoid_moran_rook <- global_moran_i_temporal(all_typhoid, "MorbidityRate", queen = FALSE)
abd_moran_rook <- global_moran_i_temporal(all_abd, "MorbidityRate", queen = FALSE)
typhoid_moran_queen <- global_moran_i_temporal(all_typhoid, "MorbidityRate", queen = TRUE)
abd_moran_queen <- global_moran_i_temporal(all_abd, "MorbidityRate", queen = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "Typhoid Moran's I (Rook)")
addWorksheet(wb, "ABD Moran's I (Rook)")
addWorksheet(wb, "Typhoid Moran's I (Queen)")
addWorksheet(wb, "ABD Moran's I (Queen)")
writeData(wb, "Typhoid Moran's I (Rook)", typhoid_moran_rook)
writeData(wb, "ABD Moran's I (Rook)", abd_moran_rook)
writeData(wb, "Typhoid Moran's I (Queen)", typhoid_moran_queen)
writeData(wb, "ABD Moran's I (Queen)", abd_moran_queen)

# Conditional formatting to highlight p-value < 0.05 and < 0.01
apply_conditional_formatting <- function(sheet_name, moran_df) {
  # Assuming p_value is in the third column, adjust the rule accordingly
  conditionalFormatting(wb,
    sheet = sheet_name,
    cols = 1:ncol(moran_df), rows = 2:(nrow(moran_df) + 1),
    rule = "$C2<0.05", type = "expression",
    style = createStyle(bgFill = "yellow")
  )

  conditionalFormatting(wb,
    sheet = sheet_name,
    cols = 1:ncol(moran_df), rows = 2:(nrow(moran_df) + 1),
    rule = "$C2<0.01", type = "expression",
    style = createStyle(bgFill = "green")
  )
}

# Apply conditional formatting to both sheets
apply_conditional_formatting("Typhoid Moran's I (Rook)", typhoid_moran_rook)
apply_conditional_formatting("ABD Moran's I (Rook)", abd_moran_rook)
apply_conditional_formatting("Typhoid Moran's I (Queen)", typhoid_moran_queen)
apply_conditional_formatting("ABD Moran's I (Queen)", abd_moran_queen)

saveWorkbook(wb, "Moran's I District Monthly (2011-2018).xlsx", overwrite = TRUE)
