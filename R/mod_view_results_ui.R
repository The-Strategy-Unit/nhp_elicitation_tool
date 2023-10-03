#' view_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_view_results_ui <- function(id) {
  strategies <- get_golem_config("strategies") |>
    purrr::map(\(.x) {
      .x |>
        dplyr::bind_rows(.id = "strategy") |>
        dplyr::arrange(.data[["label"]], .data[["name"]]) |>
        dplyr::select("name", "strategy") |>
        tibble::deframe()
    }) |>
    purrr::flatten()


  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::selectInput(
      ns("strategies"),
      "Strategies",
      c(Test = "test", strategies)
    ),
    shiny::fluidRow(
      bs4Dash::bs4Card(
        title = "Distributions",
        width = 4,
        collapsible = FALSE,
        shiny::plotOutput(ns("distributions"), height = "600px")
      ),
      bs4Dash::bs4Card(
        title = "Individuals",
        width = 4,
        collapsible = FALSE,
        shiny::plotOutput(ns("individuals"), height = "600px")
      ),
      bs4Dash::bs4Card(
        title = "Table",
        width = 4,
        collapsible = FALSE,
        gt::gt_output(
          ns("table")
        )
      )
    )
  )
}
