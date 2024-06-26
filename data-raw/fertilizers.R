
# Load packages -----------------------------------------------------------
library(pandex)
library(data.table)


# Settings ----------------------------------------------------------------

token <- ''

# Preprocess dataset ------------------------------------------------------

fertilizers <- fread(paste0('https://raw.githubusercontent.com/AgroCares/pandex/main/data-raw/b_fp/b_fp_srm.csv?token=', token))
setnames(fertilizers, colnames(fertilizers), tolower(colnames(fertilizers)))

fertilizers[, p_type_manure := fifelse(p_type_manure, 1, 0)]
fertilizers[, p_p_wcl := 1]
fertilizers[p_type_compost == TRUE, p_p_wcl := 0.25]
fertilizers[p_name_nl == 'Champost', p_p_wcl := 0.75]
fertilizers[p_name_nl == 'Rundvee vaste mest', p_p_wcl := 0.75]


# Export table ------------------------------------------------------------
usethis::use_data(fertilizers, overwrite = TRUE, version = 3, compress = 'xz')
