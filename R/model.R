#' A torch model for apus
#'
#' @description
#' Creates a torch model to be used for apus
#'
#' @param fields (data.table)
#' @param fertilizers (data.table)
#' @param
#' @param device (character)
#'
#' @import checkmate
#' @import cli
#' @import data.table
#' @import torch
#'
#'@export
createApusModel <- function(fields, fertilizers, width, depth, device) {

  # Check arguments ---------------------------------------------------------
  # TODO

  # Define torch dataset ----------------------------------------------------
  apus_model <- torch::nn(
    name = "apus_model",

    initialize = function(fields, fertilizers, width, depth, device) {

      # Check arguments -----------------------------------------------------
      # TODO

      self$fields_count <- ncol(fields)
      self$fertilizers_count <- ncol(fertilizers)


      # Setup the layers --------------------------------------------------------
      self$fc_in_fields <- torch::nn_linear(in_features = self$fields_count, out_features = width)
      self$fc_in_fertilizers <- torch::nn_linear(in_features = self$fertilizers_count, out_features = width)
      self$fc_cat <- torch::nn_linear(in_features = 2 * width, out_features = width)
      self$fc <- torch::nn_linear(width, width)
      self$fc_out <- torch::nn_linear(in_features = width, out_features = self$fertilizers_count * self$fields_count)

      self$activation <- torch::nn_prelu()
      self$activation_end <- torch::nn_relu()


    },

    forward = function (fields, fertilizers) {

      x.fields <- self$fc_in_fields(fields)
      x.fields <- self$activation(x.fields)

      x.fertilizers <- self$fc_in_fertilizers(fertilizers)
      x.fertilizers <- self$activation(x.fertilizers)

      x <- torch::torch_cat(list(x.fields, x.fertilizers))
      y <- self$fc_cat(x)
      y <- self$activation(y)

      z <- self$fc(y)
      z <- self$activation(z)

      a <- self$fc_out(z)
      a <- self$activation_end(a)
      a <- torch::torch_reshape(a, list(self$fields_count, self$fertilizers_count))

      return(a)
    }
  )

  # Create torch dataset for apus -------------------------------------------
  model <- apus_model(fields, fertilizers, width, depth, device)


  # Train the model ---------------------------------------------------------



  return(model)
}

