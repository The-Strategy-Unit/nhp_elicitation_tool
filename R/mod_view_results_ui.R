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
    shiny::fluidRow(
      shiny::selectInput(
        ns("strategy"),
        "Strategies",
        strategies
      )
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
        title = "Completed",
        width = 4,
        collapsible = FALSE,
        shiny::fluidRow(
          bs4Dash::infoBox(
            "Total Users",
            shiny::textOutput(ns("total_users")),
            icon = shiny::icon(
              "user",
              lib = "glyphicon"
            ),
            color = "primary"
          ),
          bs4Dash::infoBox(
            "Partialy Completed",
            shiny::textOutput(ns("partially_completed")),
            icon = shiny::icon(
              "flag",
              lib = "glyphicon"
            ),
            color = "warning"
          ),
          bs4Dash::infoBox(
            "Fully Completed",
            shiny::textOutput(ns("fully_completed")),
            icon = shiny::icon(
              "ok",
              lib = "glyphicon"
            ),
            color = "success"
          )
        ),
        shiny::plotOutput(
          ns("completed_plot")
        )
      )
    ),
    shiny::fluidRow(
      shiny::downloadButton(
        ns("download_results"),
        "Download results"
      )
    )
  )
}
