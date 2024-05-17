
# Set colnames for fertilizers --------------------------------------------

cols.fertilizers <- c('p_stored', 'p_price', 'p_n_rt', 'p_n_wc', 'p_p_rt', 'p_k_rt')
usethis::use_data(cols.fertilizers, overwrite = TRUE, version = 3)


# Set the column names for fields -----------------------------------------

cols.fields <- c('b_area', 'd_n_req', 'd_p_req', 'd_k_req', 'd_n_norm', 'd_n_norm_man', 'd_p_norm', 'b_lu_yield', 'b_lu_price')
usethis::use_data(cols.fields, overwrite = TRUE, version = 3)
