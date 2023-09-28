#' complete Server Functions
#'
#' @noRd
mod_complete_server <- function(id, email) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      session$userData$complete("restart")
    }) |>
      shiny::bindEvent(input$restart)

    output$results <- shiny::renderTable({
      get_latest_results(email()) |>
        dplyr::select(
          "strategy",
          "lo",
          "hi",
          "comments_lo",
          "comments_hi"
        )
    })
  })
}
