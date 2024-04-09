#' R6 Class Representing a farm to optimize the fertilizer mix for
#'
#' @description
#' A farm has a name
#'
#' @details
#' A farm needs to have fields, fertilizer etc
#'
#' @importFrom R6 R6Class
#' @import checkmate
#' @import cli
#' @import data.table
#'
#' @export
Apus <- R6::R6Class(
  "Apus",
  public = list (

    #' @field farm_name (character) Name of the farm
    farm_name = NULL,

    #'@field fields (data.table) A table with the fields and their properties
    fields = NULL,

    #'@field cultivations (data.table) A table with the properties of the cultivations
    cultivations = NULL,

    #'@field fertilizers (data.table) A table with the properties of the fertilizers
    fertilizers = NULL,

    #'@field fines (data.table) A table with the properties of the fines
    fines = NULL,

    #'@field model (nn_module) The model to use for optimization of fertilizer choice
    model = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param farm_name (character) The name of the farm
    #' @export
    initialize = function(farm_name) {

      # Check arguments ------------------------------------------------------
      checkmate::assertCharacter(farm_name, len = 1)


      # Assign farm name to object ----------------------------------------------
      self$farm_name <- farm_name


      # Add default tables to object --------------------------------------------
      self$cultivations <- apus::cultivations
      self$fertilizers <- apus::fertilizers
      self$fines <- apus::fines

      return(TRUE)
    },

    #' @description
    #' Add a field to the apus object
    #'
    #' @param b_id (character) ID or (unique) name of the field
    #' @param b_area (number) The area of the field (m^2)
    #' @param b_lu (character) The cultivation code for this field
    #' @param d_n_req (number) The required amount of Nitrogen for this field (kg N / ha)
    #' @param d_p_req (number) The required amount of Phosphate for this field (kg P2O5 / ha)
    #' @param d_k_req (number) The required amount of Potassium for this field (kg K2O / ha)
    #' @param d_n_norm (number) The legal limit for workable Nitrogen (kg N / ha)
    #' @param d_n_norm_man (number) The legal limit for total Nitrogen from manure (kg N / ha
    #' @param d_p_norm (number) The legal limit for Phosphate (kg P2O5 / ha)
    #'
    #' @export
    addField = function(b_id, b_area, b_lu, d_n_req = NA, d_p_req = NA, d_k_req = NA, d_n_norm = NA, d_n_norm_man = NA, d_p_norm = NA) {

      # Check arguments ---------------------------------------------------------
      # TODO

      # Create table with the data ----------------------------------------------
      field <- data.table(
        b_id = b_id,
        b_area = b_area,
        b_lu = b_lu,
        d_n_req = d_n_req,
        d_p_req = d_p_req,
        d_k_req = d_k_req,
        d_n_norm = d_n_norm,
        d_n_norm_man = d_n_norm_man,
        d_p_norm = d_p_norm
      )

      # Append field to fields --------------------------------------------------
      if (length(self$fields) == 0) {
        setkey(field, b_id)
        self$fields <- field
      } else {

        # Check if b_id is not already used ----------------------------------------
        if (b_id %in% self$fields$b_id) {
          cli_abort('The field {b_id} is already present and duplicate fields are not allowed.')
        }

        self$fields <- rbindlist(list(self$fields, field))
      }

      return(TRUE)
    },

    #' @description
    #' Train a model
    #'
    #' @param width (integer)
    #' @param layers (integer)
    #' @param epochs (integer)
    #' @param device (characer)
    #'
    #' @export
    trainModel = function(width = 12, layers = 1, epochs = 3, device = 'cpu') {

      # Check arguments ---------------------------------------------------------
      # TODO


      # Select device -----------------------------------------------------------
      if (device == 'cuda' & torch::cuda_is_available()){
        device <- 'cuda'
        cli::cli_alert_info('Apus model will run on  GPU')
      } else if  (device == 'cuda' & ! torch::cuda_is_available()) {
        device <- 'cpu'
        cli::cli_alert_warning('cuda is not available. Apus model will therefore run on CPU instead of GPU')
      } else {
        device <- 'cpu'
      }


      # Create an Apus dataset --------------------------------------------------
      dataset <- createApusDataset(fields = NULL, device = device)


      # Create an Apus model ----------------------------------------------------------
      model <- createApusModel(dataset, width = width, layers = layers, epochs = epochs, device = device)

      self$model <- model

      return(TRUE)
    }
  )
)
