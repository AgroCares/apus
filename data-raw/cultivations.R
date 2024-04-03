
# Load packages -----------------------------------------------------------
library(pandex)
library(data.table)


# Preprocess dataset ------------------------------------------------------

cultivations <- pandex::b_lu
cultivations <- cultivations[, c('B_LU', 'B_LU_NAME', 'B_LU_YIELD')]
setnames(cultivations, colnames(cultivations), tolower(colnames(cultivations)))

# For now set a default price per kg
cultivations[, b_lu_price := 1]

# Remove cultivations with NA
cultivations <- na.omit(cultivations)

# Export table ------------------------------------------------------------
usethis::use_data(cultivations, overwrite = TRUE, version = 3, compress = 'xz')
