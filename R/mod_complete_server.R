#' complete Server Functions
#'
#' @noRd
mod_complete_server <- function(id, email, strategies) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observe({
      session$userData$complete("restart")
    }) |>
      shiny::bindEvent(input$restart)

    results_data <- shiny::reactive({
      s <- strategies() |>
        dplyr::bind_rows(.id = "strategy")

      r <- get_latest_results(email()) |>
        dplyr::select(
          "strategy",
          "lo",
          "hi",
          "comments_lo",
          "comments_hi"
        )

      s |>
        dplyr::inner_join(r, by = dplyr::join_by("strategy")) |>
        dplyr::select(-"strategy", -"min_year")
    })

    output$results <- gt::render_gt({
      results_data() |>
        gt::gt(groupname_col = "label", rowname_col = "strategy") |>
        gt::cols_label(
          lo = "Low",
          hi = "High",
          comments_lo = "Low",
          comments_hi = "High"
        ) |>
        gt::tab_spanner("Values", c("lo", "hi")) |>
        gt::tab_spanner("Comments", c("comments_lo", "comments_hi")) |>
        gt::fmt_integer(c("lo", "hi"))
    })
  })
}
