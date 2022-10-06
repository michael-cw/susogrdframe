#' stratum_variable_selection UI Function
#'
#' @description This is the module UI to select the stratum variable from a provided shape file.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_stratum_variable_selection
#' @noRd
#'
#' @importFrom shiny NS tagList
stratumVariableUI <- function(id){
  # Create a namespace function using the provided id
  ns <- NS(id)

  selectizeInput(ns("strat_var"), "Stratification Variable", choices = c(""),
                 options = list(
                   placeholder = 'Load Boundary file first',
                   onInitialize = I('function() { this.setValue(""); }')
                 )
  )

}

#' stratum_variable_selection Server Functions
#'
#' @description This is the module server to select the stratum variable from a provided list.
#'
#' @param dataset The sf data object
#'
#' @rdname mod_stratum_variable_selection
#' @noRd
stratumVariableUpdateSvr <- function(id,  dataset = NULL){
  moduleServer( id, function(input, output, session){
    FF<-dataset
    if (is.null(FF)) {
      updateSelectizeInput(session = session,
                           "strat_var",
                           "Stratification Variable",
                           choices = c(""),
                           options = list(
                             placeholder = 'Load Boundary file first',
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
    } else {
      shiny::validate(need(FF, message = F))
      #################################
      ## Domain
      updateSelectizeInput(session = session,
                           inputId = "strat_var",
                           label = "Stratification Variable",
                           choices = names(FF),
                           options = list(
                             placeholder = 'Select variable bellow',
                             onInitialize = I('function() { this.setValue(""); }')
                           )
      )
    }

  })
}

## To be copied in the UI
# mod_stratum_variable_selection_ui("stratum_variable_selection_1")

## To be copied in the server
# mod_stratum_variable_selection_server("stratum_variable_selection_1")
