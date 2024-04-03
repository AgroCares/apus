
# Load packages -----------------------------------------------------------
library(pandex)
library(data.table)


# Settings ----------------------------------------------------------------

token <- ''

# Preprocess dataset ------------------------------------------------------

fertilizers <- fread(paste0('https://raw.githubusercontent.com/AgroCares/pandex/main/data-raw/b_fp/b_fp_srm.csv?token=', token))
setnames(fertilizers, colnames(fertilizers), tolower(colnames(fertilizers)))

# Export table ------------------------------------------------------------
usethis::use_data(fertilizers, overwrite = TRUE, version = 3, compress = 'xz')
