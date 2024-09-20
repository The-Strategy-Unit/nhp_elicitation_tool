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
  
  bslib::layout_columns(
    col_widths = c(3, 3, 3, 3, 6, 6),
    
    bslib::value_box(
      title = "Total Users",
      value = shiny::textOutput(ns("total_users")),
      showcase = bsicons::bs_icon("people")
    ),
    
    bslib::value_box(
      title = "Partialy Completed",
      value = shiny::textOutput(ns("partially_completed")),
      showcase = bsicons::bs_icon("flag")
    ),
    
    bslib::value_box(
      title = "Fully Completed",
      value = shiny::textOutput(ns("fully_completed")),
      showcase = bsicons::bs_icon("check")
    ),
    
    bslib::card(
      shiny::downloadButton(ns("download_results"), "Download results")
    ),
    
    bslib::card(
      bslib::card_header("Mitigator selection"),
      shiny::selectInput(
        ns("strategy"),
        "Strategies",
        strategies
      ), 
      shiny::plotOutput(ns("distributions"), height = "600px"),
      shiny::plotOutput(ns("individuals"), height = "600px")
    ),
  
    bslib::card(
    shiny::plotOutput(
      ns("completed_plot")
    )
  )
  )
}
