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
createApusDataset <- function(fields = NULL, device) {

  transformfieldsToTensor = createSyntheticfields = code = fields_count = self = size = value_max = value_min = NULL

  # Check arguments ---------------------------------------------------------
  # TODO


  # Settings ----------------------------------------------------------------
  fields_max <- 5


  # Define torch dataset ----------------------------------------------------
  apus_dataset <- torch::dataset(
    name = "apus_dataset",

    initialize = function(fields = NULL, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fields_max, device) {

      # Check arguments -----------------------------------------------------
      # TODO


      # Store the data ------------------------------------------------------
      self$fields_max <- fields_max
      self$device <- device
      if (length(fields) >0 ) {
        self$fields <- transformFieldsToTensor(fields, device = device)
      } else {
        # self$fields <- NULL
      }

      fertilizers[, P_ID := 1:.N]
      fertilizers <- fertilizers[, c('P_ID', 'P_N_RT', 'P_N_WC', 'P_P_RT')]
      self$fertilizers <- torch::torch_tensor(as.matrix(fertilizers), device = device)

    },

    .getitem = function(index) {

      if (length(self$fields) == 0) {
        fields <- createSyntheticFields(self$fields_max, self$cultivations, apus::parameters)
        t.fields <- transformFieldsToTensor(fields, self$device)
      } else {
        fields <- self$fields[b_id_field == index, ]
        t.fields <- transformFieldsToTensor(fields, self$device)
      }

      return(list(fields = t.fields, fertilizers = self$fertilizers))
    },

    .length = function() {
      self$fields_max
    }
  )


  # Create torch dataset for apus -------------------------------------------
  dataset <- apus_dataset(fields = NULL, cultivations = apus::cultivations, fertilizers = apus::fertilizers, fields_max, device)

  return(dataset)
}


transformFieldsToTensor = function(fields, device) {

  # Select only relevant columns and define column order --------------------
  col.fields <- c('b_id_field', 'b_area', 'd_n_req', 'd_p_req', 'd_k_req', 'd_n_norm', 'd_n_norm_man', 'd_p_norm')
  fields <- fields[, mget(col.fields)]


  # Create torch_tensor from array ------------------------------------------
  t.tensor <- torch::torch_tensor(as.matrix(fields), device = device)


  return(t.tensor)
}

createSyntheticFields = function (fields_max, cultivations, parameters) {

  fields <- data.table(
    b_id_field = 1:fields_max,
    # b_lu = sample(x = cultivations$b_lu, fields_max = fields_max, replace = TRUE),
    b_area = stats::runif(n = fields_max, min = parameters[code == 'b_area', value_min], max = parameters[code == 'b_area', value_max]),
    d_n_req =  stats::runif(n = fields_max, min = parameters[code == 'd_n_req', value_min], max = parameters[code == 'd_n_req', value_max]),
    d_p_req = stats::runif(n = fields_max, min = parameters[code == 'd_p_req', value_min], max = parameters[code == 'd_p_req', value_max]),
    d_k_req = stats::runif(n = fields_max, min = parameters[code == 'd_k_req', value_min], max = parameters[code == 'd_k_req', value_max]),
    d_n_norm = stats::runif(n = fields_max, min = parameters[code == 'd_n_norm', value_min], max = parameters[code == 'd_n_norm', value_max]),
    d_n_norm_man = stats::runif(n = fields_max, min = parameters[code == 'd_n_norm_man', value_min], max = parameters[code == 'd_n_norm_man', value_max]),
    d_p_norm = stats::runif(n = fields_max, min = parameters[code == 'd_p_norm', value_min], max = parameters[code == 'd_p_norm', value_max])
  )

  return(fields)
}

