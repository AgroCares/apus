#' A torch dataset for apus
#'
#' @description
#' Creates a torch dateset to be used for apus model
#'
#' @param fields (data.table)
#' @param device (character)
#'
#' @import checkmate
#' @import cli
#' @import data.table
#' @import torch
#'
#'@export
createApusDataset <- function(farms = NULL, device) {

  transformfieldsToTensor = createSyntheticfields = code = fields_count = self = NULL
  size = value_max = value_min = P_PRICE = P_STORED = b_id_field = NULL

  # Check arguments ---------------------------------------------------------
  # TODO


  # Settings ----------------------------------------------------------------
  fields_max <- 3


  # Define torch dataset ----------------------------------------------------
  apus_dataset <- torch::dataset(
    name = "apus_dataset",

    initialize = function(farms = NULL, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fields_max, device) {

      # Check arguments -----------------------------------------------------
      # TODO


      # Store the data ------------------------------------------------------

      self$fields_max <- fields_max
      self$device <- device
      if (length(farms) > 0 ) {
        self$farms <- transformFieldsToTensor(farms, device = device)
        self$farms_count <- 1
      } else {
        self$farms <- NULL
        self$farms_count <- 100
      }

      # Set temporary
      fertilizers[, p_stored := 0]
      fertilizers[, p_price := 1]

      fertilizers <- fertilizers[, c('p_stored', 'p_price')]

      self$fertilizers <- torch::torch_tensor(as.matrix(fertilizers), device = device)
    },

    .getitem = function(index) {

      if (length(self$fields) == 0) {
        farms <- createSyntheticFarms(1, self$fields_max, self$cultivations, apus::parameters)
        t.fields <- transformFieldsToTensor(farms, self$device)
      } else {
        t.fields <- self$fields[b_id_farm == index, ]
      }

      return(list(fields = t.fields, fertilizers = self$fertilizers))
    },

    .length = function() {
      self$farms_count
    }
  )


  # Create torch dataset for apus -------------------------------------------
  dataset <- apus_dataset(farms = farms, cultivations = cultivations, fertilizers = fertilizers, fields_max, device)

  return(dataset)
}


transformFieldsToTensor = function(fields, device) {

  # Select only relevant columns and define column order --------------------
  col.fields <- c('b_area', 'd_n_req', 'd_p_req', 'd_k_req', 'd_n_norm', 'd_n_norm_man', 'd_p_norm')
  fields <- fields[, mget(col.fields)]


  # Create torch_tensor from array ------------------------------------------
  t.tensor <- torch::torch_tensor(as.matrix(fields), device = device)


  return(t.tensor)
}

createSyntheticFarms = function (farms_count, fields_max, cultivations = apus::cultivations, parameters = apus::parameters) {

  code = value_min = value_max = NULL

  size <- farms_count * fields_max
  fields <- data.table(
    b_id_farm = rep(1:farms_count, each = fields_max),
    b_id_field = rep(1:fields_max, times = farms_count),
    # b_lu = sample(x = cultivations$b_lu, fields_max = fields_max, replace = TRUE),
    b_area = stats::runif(n = size, min = parameters[code == 'b_area', value_min], max = parameters[code == 'b_area', value_max]),
    d_n_req =  stats::runif(n = size, min = parameters[code == 'd_n_req', value_min], max = parameters[code == 'd_n_req', value_max]),
    d_p_req = stats::runif(n = size, min = parameters[code == 'd_p_req', value_min], max = parameters[code == 'd_p_req', value_max]),
    d_k_req = stats::runif(n = size, min = parameters[code == 'd_k_req', value_min], max = parameters[code == 'd_k_req', value_max]),
    d_n_norm = stats::runif(n = size, min = parameters[code == 'd_n_norm', value_min], max = parameters[code == 'd_n_norm', value_max]),
    d_n_norm_man = stats::runif(n = size, min = parameters[code == 'd_n_norm_man', value_min], max = parameters[code == 'd_n_norm_man', value_max]),
    d_p_norm = stats::runif(n = size, min = parameters[code == 'd_p_norm', value_min], max = parameters[code == 'd_p_norm', value_max]),
    b_lu_yield = stats::runif(n = size, min = parameters[code == 'b_lu_yield', value_min], max = parameters[code == 'b_lu_yield', value_max]),
    b_lu_price = stats::runif(n = size, min = parameters[code == 'b_lu_price', value_min], max = parameters[code == 'b_lu_price', value_max])
  )

  return(fields)
}

