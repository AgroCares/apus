
# Load packages -----------------------------------------------------------
library(pandex)
library(data.table)


# Create default dataset based on Dutch rules -----------------------------
# WARNING: THESE FINES DO NOT INCLUDE COMBINATIONS YET!
fines <- data.table(
  norm = c('d_n_norm', 'd_n_norm_man', 'd_p_norm'),
  fine = c(7, 7, 11)
)

# Export table ------------------------------------------------------------
usethis::use_data(fines, overwrite = TRUE, version = 3, compress = 'xz')
