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
      "Your results are below.",
      "You may",
      shiny::actionLink(
        ns("restart"),
        "return"
      ),
      "back to the tool to change any values if you wish."
    ),
    shiny::tags$p(
      shiny::tags$strong(
        "The app will close on Tuesday 17",
        shiny::tags$sup("th"),
        "October, 2023 (midnight)."
      )
    ),
    shinycssloaders::withSpinner(
      gt::gt_output(ns("results"))
    )
  )
}
