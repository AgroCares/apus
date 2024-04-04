#' A torch model for apus
#'
#' @description
#' Creates a torch model to be used for apus
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
createApusModel <- function(device) {

  # Check arguments ---------------------------------------------------------
  # TODO

  # Define torch dataset ----------------------------------------------------
  apus_model <- torch::nn(
    name = "apus_dataset",

    initialize = function(dim_start, width, dim_end, depth, device) {

      # Check arguments -----------------------------------------------------
      # TODO


      # Setup the layers --------------------------------------------------------
      self$fc_in <- torch::nn_linear(in_features = dim_start, out_features = width)
      self$fc <- torch::nn_linear(width, width)
      self$fc_out <- torch::nn_linear(in_features = width, out_features = dim_end)

      self$activation <- torch::nn_prelu()
      self$activation_end <- torch::nn_relu()


    },

    forward = function (farms) {

      x <- self$fc_in(farms)
      y <- self$fc(x)
      z <- self$fc_end(y)

      return(z)
    }
  )

  # Create torch dataset for apus -------------------------------------------
  model <- apus_model()

  return(dataset)
}

