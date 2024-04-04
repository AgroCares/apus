
# Load packages -----------------------------------------------------------
library(pandex)
library(data.table)


# Preprocess dataset ------------------------------------------------------

parameters <- pandex::nmi_parameters
parameters <- parameters[code %in% c('B_ID', 'B_AREA', 'B_LU', 'D_N_REQ', 'D_P_REQ', 'D_K_REQ', 'D_N_NORM', 'D_N_NOR_MAN', 'D_P_NORM')]
parameters[, code := tolower(code)]


# Export table ------------------------------------------------------------
usethis::use_data(parameters, overwrite = TRUE, version = 3, compress = 'xz')
