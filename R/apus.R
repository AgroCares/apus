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
#'
#' @export
Apus <- R6::R6Class(
  "Apus",
  public = list (

    #' @field farm_name (character) Name of the farm
    farm_name = NULL,


    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param farm_name (character) The name of the farm
    initialize = function(farm_name) {

      # Check arguments ------------------------------------------------------
      checkmate::assertCharacter(farm_name, len = 1)


      # Assign farm name to object ----------------------------------------------
      self$farm_name <- farm_name

      return(TRUE)
    }
  )
)
