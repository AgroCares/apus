#' A torch model for apus
#'
#' @description
#' Creates a torch model to be used for apus
#'
#' @param dataset (ApusDataset) Dataset created with createApusDataset
#' @param width (integer)
#' @param layers (integer)
#' @param device (character)
#'
#' @import checkmate
#' @import cli
#' @import data.table
#' @import torch
#'
#'@export
createApusModel <- function(dataset, width = 12, layers = 1, epochs = 100, device) {

  self = NULL

  # Check arguments ---------------------------------------------------------
  # TODO

  # Define torch dataset ----------------------------------------------------
  apus_model <- torch::nn_module(
    name = "apus_model",

    initialize = function(dataset, width, layers, device) {

      # Check arguments -----------------------------------------------------
      # TODO


      batch <- dataset$.getitem(1)
      self$fields_count <- as.integer(dim(batch$fields)[1])
      self$fields_parameters_count <- dim(batch$fields)[2]
      self$fertilizers_count <- dim(batch$fertilizers)[1]
      self$fertilizers_parameters_count <- dim(batch$fertilizers)[2]


      # Setup the layers --------------------------------------------------------
      self$fc_in_fields <- torch::nn_linear(in_features = self$fields_parameters_count, out_features = width)
      self$fc_in_fertilizers <- torch::nn_linear(in_features = self$fertilizers_parameters_count * self$fertilizers_count, out_features = width)
      self$fc_cat <- torch::nn_linear(in_features = 2 * width, out_features = width)
      self$fc <- torch::nn_linear(width, width)
      self$fc_out <- torch::nn_linear(in_features = width, out_features = self$fertilizers_count)

      self$activation <- torch::nn_prelu()
      self$activation_end <- torch::nn_relu()


    },

    forward = function (fields, fertilizers) {

      x.fields <- self$fc_in_fields(fields)
      x.fields <- self$activation(x.fields)
      # print(dim(x.fields))

      x.fertilizers <- fertilizers$flatten(start_dim = 2)
      # dim(x.fertilizers)
      x.fertilizers <- x.fertilizers$reshape(c(dim(fertilizers)[1], 1, self$fertilizers_parameters_count * self$fertilizers_count))
      # print(dim(x.fertilizers))
      x.fertilizers <- x.fertilizers$repeat_interleave(self$fields_count, dim = 2)
      # print(dim(x.fertilizers))

      x.fertilizers <- self$fc_in_fertilizers(x.fertilizers)
      x.fertilizers <- self$activation(x.fertilizers)
      # print(dim(x.fertilizers))

      x <- torch::torch_cat(list(x.fields, x.fertilizers), dim = 3L)
      # print(dim(x))
      y <- self$fc_cat(x)
      # print(dim(y))
      y <- self$activation(y)

      z <- self$fc(y)
      z <- self$activation(z)
      # print(dim(z))

      a <- self$fc_out(z)
      a <- self$activation_end(a)
      # print(dim(a))

      return(a)
    }
  )

  # Create torch model for apus -------------------------------------------
  model <- apus_model(dataset, width, layers, device)
  optimizer <- torch::optim_adam(model$parameters, lr = 0.001)
  dl <- torch::dataloader(dataset, batch_size = 1)



  # Train the model ---------------------------------------------------------
  for (epoch in 1:epochs) {

    # Training loop
    losses.train <- c()
    coro::loop(for (b in dl) {

      # For testing
      # b <- dl$.iter()
      # b <- b$.next()

      # Forward pass
      optimizer$zero_grad()
      doses <- model(b$fields, b$fertilizers)
      cost <- calculateCost(doses, b$fields, b$fertilizers)

      # Backward pass
      cost$backward()
      optimizer$step()

      losses.train <- c(losses.train, cost$item())

    })

    cat(sprintf("Loss at epoch %d: %3f\n", epoch, mean(losses.train)))

    # Validation loop

  }

  return(model)
}

calculateCost <- function(doses, fields, fertilizers, sum_batches = TRUE) {


  # Check arguments ---------------------------------------------------------
  # TODO


  # Module 1: Purchase of fertilizers ---------------------------------------
  module1 <- calculateCostModule1(doses, fertilizers)



  # Combine the modules -----------------------------------------------------
  cost <- torch::torch_zeros(dim(doses)[1]) + module1


  # Reduce batches to single value ------------------------------------------
  if (sum_batches) {
    cost <- torch::torch_sum(cost)
  }

  return(cost)
}

# Module 1: Purchase of fertilizers ---------------------------------------
calculateCostModule1 <- function(doses, fertilizers) {

  # Sum dose per fertilizer -------------------------------------------------
  fertilizers.dose <- torch::torch_sum(doses, dim = 2L)


  # Calculate cost per fertilizer -------------------------------------------
  fertilizers.price <- fertilizers[,,2]
  fertilzers.cost <- fertilizers.dose * fertilizers.price


  # Sum cost for farm -------------------------------------------------------
  module1 <- torch::torch_sum(fertilzers.cost, dim = 2L)


  return(module1)
}
