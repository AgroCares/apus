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

    #'@field device (character) Whether to run the model on `cpu` or `cuda`
    device = 'cpu',

    #'@field fields_max (integer) Maximum number of fields for a farm
    fields_max = 5,


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
    #' @param b_id_field (integer) ID of the field
    #' @param b_area (number) The area of the field (ha)
    #' @param b_lu (character) The cultivation code for this field
    #' @param d_n_req (number) The required amount of Nitrogen for this field (kg N / ha / year)
    #' @param d_p_req (number) The required amount of Phosphate for this field (kg P2O5 / ha / year)
    #' @param d_k_req (number) The required amount of Potassium for this field (kg K2O / ha / year)
    #' @param d_n_norm (number) The legal limit for workable Nitrogen (kg N / ha / year)
    #' @param d_n_norm_man (number) The legal limit for total Nitrogen from manure (kg N / ha / year)
    #' @param d_p_norm (number) The legal limit for Phosphate (kg P2O5 / ha / year)
    #' @param b_lu_yield (number) Expected harvest (kg  dry matter/ ha)
    #' @param b_lu_price (number) Expected price for harvest (€/ kg)
    #'
    #' @export
    addField = function(b_id_field, b_area, b_lu, d_n_req = NA, d_p_req = NA, d_k_req = NA, d_n_norm = NA, d_n_norm_man = NA, d_p_norm = NA, b_lu_yield = NA, b_lu_price = NA) {

      # Check arguments ---------------------------------------------------------
      checkmate::assert_integerish(b_id_field,len = 1)
      checkmate::assert_numeric(b_area,lower = 0, upper = 50000)
      checkmate::assert_subset(b_lu,choices = apus::cultivations$b_lu)
      checkmate::assert_numeric(d_n_req,lower = 0, upper = 400)
      checkmate::assert_numeric(d_p_req,lower = 0, upper = 150)
      checkmate::assert_numeric(d_k_req,lower = 0, upper = 800)
      checkmate::assert_numeric(d_n_norm,lower = 0, upper = 400)
      checkmate::assert_numeric(d_n_norm_man,lower = 0, upper = 250)
      checkmate::assert_numeric(d_p_norm,lower = 0, upper = 100)
      checkmate::assert_numeric(b_lu_yield,lower = 0, upper = 100000)
      checkmate::assert_numeric(b_lu_price,lower = 0, upper = 100)

      # Create table with the data ----------------------------------------------
      field <- data.table(
        b_id_farm = 1,
        b_id_field = b_id_field,
        b_area = b_area,
        b_lu = b_lu,
        d_n_req = d_n_req,
        d_p_req = d_p_req,
        d_k_req = d_k_req,
        d_n_norm = d_n_norm,
        d_n_norm_man = d_n_norm_man,
        d_p_norm = d_p_norm,
        b_lu_yield = b_lu_yield,
        b_lu_price = b_lu_price
      )

      setkey(field, b_id_field)

      # Append field to fields --------------------------------------------------
      if (length(self$fields) == 0) {

        # add field to self
        self$fields <- field

      } else {

        # Check if b_id_field is not already used ----------------------------------------
        if (b_id_field %in% self$fields$b_id_field) {
          cli_abort('The field {b_id_field} is already present and duplicate fields are not allowed.')
        }

        # add new fields to apus object
        self$fields <- rbindlist(list(self$fields, field))
      }

      return(TRUE)
    },

    #' @description
    #' Update a fertilizer
    #'
    #' @param p_id (character) ID of the fertilizer
    #' @param p_price (number)
    #' @param p_stored (number)
    #' @param p_storage_available (number)
    #'
    #' @export
    updateFertilizer = function(p_id, p_price = NA_real_, p_stored = NA_real_, p_storage_available = NA_real_) {

      # Check arguments ---------------------------------------------------------
      #TODO

      # update the fertilizers table with data for that fertilizer --------------
      fertilizers.new <- self$fertilizers
      p_id.new <- p_id

      # Update price
      if(! is.na(p_price)) {
        p_price.new <- p_price
        fertilizers.new[p_id == p_id.new, p_price := p_price.new]
      }

      # Update stored amount of fertilizer
      if(! is.na(p_stored)) {
        p_stored.new <- p_stored
        fertilizers.new[p_id == p_id.new, p_stored := p_stored.new]
      }

      # Update available storages for fertilizer
      if(! is.na(p_storage_available)) {
        p_storage_available.new <- p_storage_available
        fertilizers.new[p_id == p_id.new, p_storage_available := p_storage_available.new]
      }

      # Return back updated fertilizer
      self$fertilizers <- fertilizers.new

      return(TRUE)
    },

    #' @description
    #' Train a model to
    #'
    #' @param width (integer)
    #' @param layers (integer)
    #' @param epochs (integer)
    #' @param device (character)
    #'
    #' @export
    trainModel = function(width = 12, layers = 1, epochs = 3, device = 'cpu') {

      # Check arguments ---------------------------------------------------------
      # TODO


      # Select device -----------------------------------------------------------
      if (device == 'cuda') {
        if (torch::cuda_is_available()){
          device <- 'cuda'
          cli::cli_alert_info('Apus model will run on  GPU')
        } else if  (! torch::cuda_is_available()) {
          device <- 'cpu'
          cli::cli_alert_warning('cuda is not available. Apus model will therefore run on CPU instead of GPU')
        } else {
          device <- 'cpu'
        }
      } else {
        device <- 'cpu'
      }
      self$device <- device

      # Create an Apus dataset --------------------------------------------------
      dataset.train <- createApusDataset(farms = NULL,
                                         cultivation = self$cultivation,
                                         fertilizers = self$fertilizers,
                                         fines = self$fines,
                                         fields_max = self$fields_max,
                                         device = device)

      farms.valid <- createSyntheticFarms(farms_count = 1000,
                                          fields_max = self$fields_max)

      dataset.valid <- createApusDataset(farms = farms.valid,
                                         cultivation = self$cultivation,
                                         fertilizers = self$fertilizers,
                                         fines = self$fines,
                                         fields_max = self$fields_max,
                                         device = device)


      # Create an Apus model ----------------------------------------------------------
      model <- createApusModel(dataset.train,
                               dataset.valid,
                               width = width,
                               layers = layers,
                               epochs = epochs,
                               device = device)

      self$model <- model

      return(TRUE)
    },

    #' @description
    #' Optimize fertilizer choice
    #'
    #' @export
    optimizeFertilizerChoice = function() {

      # Check arguments ---------------------------------------------------------
      if (length(self$model) == 0) {
        cli::cli_abort('No model available currently. Please create a model first with the function `trainModel`')
      }


      # Create dataset ----------------------------------------------------------
      fields <- self$fields
      if (nrow(fields) < self$fields_max) {
        extra_fields <- self$fields_max - nrow(fields)
        fields <- rbindlist(list(fields, data.table(
          b_id_farm = 1,
          b_id_field = (max(fields$b_id_field) +1): (self$fields_max)
          )), fill = TRUE)
        fields[is.na(fields)] <- 0
      }

      dataset <- createApusDataset(farms = fields, cultivations = self$cultivations, fertilizers = self$fertilizers, fines = self$fines, fields_max = self$fields_max, device = self$device)
      dl <- torch::dataloader(dataset, batch_size = 1)


      # Predict optimal fertilizer choice ---------------------------------------
      batch <- dl$.iter()
      batch <- batch$.next()
      doses <- self$model(batch$fields, batch$fertilizers)


      # Format to output --------------------------------------------------------
      dt <- data.table::as.data.table(as.array(doses)[1,,])
      colnames(dt) <- self$fertilizers$p_name_nl
      dt <- dt[, lapply(.SD, round)]
      dt[, b_id_field := fields$b_id_field]
      dt <- dt[1:nrow(self$fields), ]
      data.table::setcolorder(dt, 'b_id_field')


      return(dt)
    }
  )
)
