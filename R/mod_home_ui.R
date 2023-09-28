#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("NHP National Elicitation Tool"),
    shiny::tags$p("TEXT TO GO HERE"),
    shiny::textInput(
      ns("email"),
      "Email"
    ),
    shiny::textOutput(
      ns("selected_mitigators")
    ),
    shiny::actionButton(
      ns("start"),
      "Start"
    )
  )
}
