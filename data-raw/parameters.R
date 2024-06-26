
# Load packages -----------------------------------------------------------
library(pandex)
library(data.table)


# Preprocess dataset ------------------------------------------------------

parameters <- pandex::nmi_parameters
parameters <- parameters[code %in% c('B_ID', 'B_AREA', 'B_LU', 'D_N_REQ', 'D_P_REQ', 'D_K_REQ', 'D_N_NORM', 'D_N_NORM_MAN', 'D_P_NORM' , 'B_LU_YIELD')]
parameters[, code := tolower(code)]
parameters <- rbindlist(list(parameters, data.table(code = c('b_lu_price'))), fill = TRUE)

# Set min max explicit ----------------------------------------------------
parameters[code == 'b_area', value_max := 20000]
parameters[code == 'd_k_req', value_max := 100]
parameters[code == 'd_n_req', value_max := 500]
parameters[code == 'd_p_req', value_max := 200]

parameters[code == 'b_lu_yield', value_min := 100]
parameters[code == 'b_lu_yield', value_max := 100000]
parameters[code == 'b_lu_price', value_min := 0.01]
parameters[code == 'b_lu_price', value_max := 10]

# Export table ------------------------------------------------------------
usethis::use_data(parameters, overwrite = TRUE, version = 3, compress = 'xz')
