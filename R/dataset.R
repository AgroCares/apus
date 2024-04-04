#' A torch dataset for apus
#'
#' @description
#' Creates a torch dateset to be used for apus model
#'
#' @param farms (data.table)
#' @param device (character)
#'
#' @import checkmate
#' @import cli
#' @import data.table
#' @import torch
#'
#'@export
createApusDataset <- function(farms = NULL, device) {

  transformFarmsToTensor = createSyntheticFarms = code = farms_count = self = size = value_max = value_min = NULL

  # Check arguments ---------------------------------------------------------
  # TODO


  # Settings ----------------------------------------------------------------
  fields_max <- 5


  # Define torch dataset ----------------------------------------------------
  apus_dataset <- torch::dataset(
    name = "apus_dataset",

    initialize = function(farms = NULL, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fields_max, device) {

      # Check arguments -----------------------------------------------------
      # TODO


      # Store the data ------------------------------------------------------
      self$fields_max <- fields_max
      self$device <- device
      if (length(farms) >0 ) {
        self$farms <- transformFarmsToTensor(farms, device = device)
        self$farms_count <- uniqueN(farms$b_id_farm)
      } else {
        # self$farms <- NULL
        self$farms_count <- 10
      }
    },

    .getitem = function(index) {

      if (length(farms) == 0) {
        farms <- createSyntheticFarms(self$farms_count, self$fields_max, self$cultivations, apus::parameters)
        t.farms <- transformFarmsToTensor(farms, self$device)
      } else {
        t.farms <- self$farms[index]
      }

      return(list(farms = t.farms))
    },

    .length = function() {
      self$farms_count
    }
  )


  # Create torch dataset for apus -------------------------------------------
  dataset <- apus_dataset(farms = NULL, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fields_max, device)

  return(dataset)
}


transformFarmsToTensor = function(farms, device) {

  # Select only relevant columns and define column order --------------------
  col.farms <- c('b_id_farm', 'b_id_field', 'b_area', 'd_n_req', 'd_p_req', 'd_k_req', 'd_n_norm', 'd_n_norm_man', 'd_p_norm')
  farms <- farms[, mget(col.farms)]

  # Convert farms to array with dimension: [farm, field, parameter] ---------
  ar.farms <- split(farms, by = 'b_id_farm')
  ar.farms  <- abind::abind(ar.farms , along = 3)
  ar.farms  <- aperm(ar.farms, c(3,1,2))


  # Create torch_tensor from array ------------------------------------------
  t.tensor <- torch::torch_tensor(ar.farms, device = 'cpu')


  return(t.tensor)
}

createSyntheticFarms = function (farms_count, fields_max, cultivations, parameters) {

  size <- farms_count * fields_max
  farms <- data.table(
    b_id_farm = rep(1:farms_count, each = fields_max),
    b_id_field = rep(1:fields_max, times = farms_count),
    # b_lu = sample(x = cultivations$b_lu, size = size, replace = TRUE),
    b_area = stats::runif(n = size, min = parameters[code == 'b_area', value_min], max = parameters[code == 'b_area', value_max]),
    d_n_req =  stats::runif(n = size, min = parameters[code == 'd_n_req', value_min], max = parameters[code == 'd_n_req', value_max]),
    d_p_req = stats::runif(n = size, min = parameters[code == 'd_p_req', value_min], max = parameters[code == 'd_p_req', value_max]),
    d_k_req = stats::runif(n = size, min = parameters[code == 'd_k_req', value_min], max = parameters[code == 'd_k_req', value_max]),
    d_n_norm = stats::runif(n = size, min = parameters[code == 'd_n_norm', value_min], max = parameters[code == 'd_n_norm', value_max]),
    d_n_norm_man = stats::runif(n = size, min = parameters[code == 'd_n_norm_man', value_min], max = parameters[code == 'd_n_norm_man', value_max]),
    d_p_norm = stats::runif(n = size, min = parameters[code == 'd_p_norm', value_min], max = parameters[code == 'd_p_norm', value_max])
  )

  return(farms)
}

