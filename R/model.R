#' R6 Class Representing a model that can be used to optimize
#'
#' @description
#' A model has
#'
#' @details
#' A model requires
#'
#' @importFrom R6 R6Class
#' @import checkmate
#' @import cli
#' @import data.table
#'
#' @export
ApusModel <- R6::R6Class(
  "ApusModel",
  public = list (

    #' @field model (character) Name of the farm
    model = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param cultivations (data.table) The list of cultivations available to train the model for. Should have similiar structure
    #' @param fertilizers (data.table)
    #' @param fines (data.table)
    #'
    #' @export
    initialize = function(cultivations = apus::cultivations, fertilizers = apus::fertilizers, fines = apus::fines) {

      # Check arguments ------------------------------------------------------
      checkmate::assertDataTable(cultivations, col.names = colnames(apus::cultivations))
      checkmate::assertDataTable(fertilizers, col.names = colnames(apus::fertilizers))
      checkmate::assertDataTable(fines, col.names = colnames(apus::fines))

      return(TRUE)
    },

    #' @description
    #' Train an apus model
    #'
    #' @export
    trainModel = function() {

      # Check arguments ---------------------------------------------------------
      # TODO

      return(TRUE)
    }
  )
)
