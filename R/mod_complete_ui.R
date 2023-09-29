#' complete UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_complete_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("Complete"),
    shiny::tags$p(
      "Thank you for completing the exercise.",
      "Your results are below, but you may",
      shiny::actionLink(
        ns("restart"),
        "return"
      ),
      "back to the tool to update values."
    ),
    gt::gt_output(ns("results"))
  )
}
