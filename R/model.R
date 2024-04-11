#' A torch model for apus
#'
#' @description
#' Creates a torch model to be used for apus
#'
#' @param dataset.train (ApusDataset) Dataset created with createApusDataset to train the model
#' @param dataset.valid (ApusDataset) Dataset created with createApusDataset to validate the model
#' @param width (integer)
#' @param layers (integer)
#' @param epochs (integer)
#' @param device (character)
#'
#' @import checkmate
#' @import cli
#' @import data.table
#' @import torch
#'
#'@export
createApusModel <- function(dataset.train, dataset.valid, width = 12, layers = 1, epochs = 100, device) {

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
  model <- apus_model(dataset.train, width, layers, device)
  optimizer <- torch::optim_adam(model$parameters, lr = 0.001)
  dl.train <- torch::dataloader(dataset.train, batch_size = 10)
  dl.valid <- torch::dataloader(dataset.valid, batch_size = 10)



  # Train the model ---------------------------------------------------------
  for (epoch in 1:epochs) {

    # Training loop
    losses.train <- c()
    model$train(TRUE)
    coro::loop(for (b in dl.train) {

      cli::cli_progress_bar(paste0('Training model [', epoch, '/', epochs, ']'), total = dl.train$.length())

      # For testing
      # b <- dl.train$.iter()
      # b <- b$.next()

      # Forward pass
      optimizer$zero_grad()
      doses <- model(b$fields, b$fertilizers)
      cost <- calculateCost(doses, b$fields, b$fertilizers)

      # Backward pass
      cost$backward()
      optimizer$step()

      losses.train <- c(losses.train, cost$item())

      cli::cli_progress_update(status = paste0('Loss: ', signif(mean(losses.train, 4))))

    })
    cli::cli_progress_done()

    # Validation loop
    model$eval()
    losses.validation <- c()
    coro::loop(for (b in dl.valid) {

      cli::cli_progress_bar(paste0('Validating model [', epoch, '/', epochs, ']'), total = dl.valid$.length())

      # For testing
      # b <- dl.valid$.iter()
      # b <- b$.next()

      # Forward pass
      doses <- model(b$fields, b$fertilizers)
      cost <- calculateCost(doses, b$fields, b$fertilizers)

      losses.validation <- c(losses.validation, cost$item())

      cli::cli_progress_update(status = paste0('Loss: ', signif(mean(losses.validation, 4))))

    })

    loss.train <- signif(mean(losses.train), 4)
    loss.validation <- signif(mean(losses.validation), 4)
    cli::cli_alert_info('Epoch [{epoch}/{epochs}] Training loss: {loss.train}; Validation loss: {loss.validation}')

  }
  cli::cli_alert_success('Finished model training')

  return(model)
}

calculateCost <- function(doses, fields, fertilizers, sum_batches = TRUE) {


  # Check arguments ---------------------------------------------------------
  # TODO


  # Module 1: Purchase of fertilizers ---------------------------------------
  module1 <- calculateCostModule1(doses, fertilizers)


  # Module 4: Yield of cultivations -----------------------------------------



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

# Module 4: Yield of cultivations -------------------------------------------
calculateCostModule4 <- function(doses, fields, fertilizers) {

  # Calculate N dose per fields
  fertilizers.p_n_rt <- fertilizers[,,3]
  fertilizers.p_n_wc <- fertilizers[,,4]
  fertilizers.p_n_workable <- fertilizers.p_n_rt * fertilizers.p_n_wc
  fertilizers.p_n_workable <- torch::torch_unsqueeze(fertilizers.p_n_workable, 2)
  fertilizers.p_n_workable <- torch::torch_repeat_interleave(fertilizers.p_n_workable, repeats = dim(doses)[2], dim =2)
  fields.fertilizers.dose.n_workable <- doses * fertilizers.p_n_workable
  fields.dose.n_workable <- torch::torch_sum(fields.fertilizers.dose.n_workable, dim = 3)

  # Calculate N requirement realization
  fields.d_n_req <- fields[,,2]
  fields.d_n_gap <- fields.d_n_req - fields.dose.n_workable
  fields.d_n_gap <- torch::torch_where(fields.d_n_gap < 0, torch::torch_zeros(dim(fields.d_n_gap)), fields.d_n_gap)
  fields.d_n_realized <- torch::torch_ones(dim(fields.d_n_gap)) - (fields.d_n_gap / fields.d_n_req)

  # Calculate P dose per fields
  fertilizers.p_p_rt <- fertilizers[,,5]
  fertilizers.p_p_rt <- torch::torch_unsqueeze(fertilizers.p_p_rt, 2)
  fertilizers.p_p_rt <- torch::torch_repeat_interleave(fertilizers.p_p_rt, repeats = dim(doses)[2], dim =2)
  fields.fertilizers.dose.p <- doses * fertilizers.p_p_rt
  fields.dose.p <- torch::torch_sum(fields.fertilizers.dose.p, dim = 3)

  # Calculate P requirement realization
  fields.d_p_req <- fields[,,3]
  fields.d_p_gap <- fields.d_p_req - fields.dose.p
  fields.d_p_gap <- torch::torch_where(fields.d_p_gap < 0, torch::torch_zeros(dim(fields.d_p_gap)), fields.d_p_gap)
  fields.d_p_realized <- torch::torch_ones(dim(fields.d_p_gap)) - (fields.d_p_gap / fields.d_p_req)


  # Calculate K dose per fields
  fertilizers.p_k_rt <- fertilizers[,,6]
  fertilizers.p_k_rt <- torch::torch_unsqueeze(fertilizers.p_k_rt, 2)
  fertilizers.p_k_rt <- torch::torch_repeat_interleave(fertilizers.p_k_rt, repeats = dim(doses)[2], dim =2)
  fields.fertilizers.dose.k <- doses * fertilizers.p_k_rt
  fields.dose.k <- torch::torch_sum(fields.fertilizers.dose.k, dim = 3)

  # Calculate K requirement realization
  fields.d_k_req <- fields[,,4]
  fields.d_k_gap <- fields.d_k_req - fields.dose.k
  fields.d_k_gap <- torch::torch_where(fields.d_k_gap < 0, torch::torch_zeros(dim(fields.d_k_gap)), fields.d_k_gap)
  fields.d_k_realized <- torch::torch_ones(dim(fields.d_k_gap)) - (fields.d_k_gap / fields.d_k_req)

  # Calculate the forecasted yield
  fields.d_realized <- torch::torch_cat(list(fields.d_n_realized, fields.d_p_realized, fields.d_k_realized), dim=3)
  fields.d_realized <- torch_minimum(fields.d_realized, dim = 3)
  fields.b_area <- fields[,,1]
  fields.b_lu_yield <- fields[,,7]
  fields.b_lu_price <- fields[,,8]
  module4 <- fields.b_area *  fields.b_lu_yield * fields.b_lu_price * fields.d_n_realized
  module4 <- torch::torch_sum(module4, dim  = 2)

  return(module4)
}

