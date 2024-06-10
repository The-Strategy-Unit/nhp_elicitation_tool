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
    }) |>
      shiny::bindEvent(session$userData$last_saved())

    output$results <- gt::render_gt({
      results_data() |>
        gt::gt(groupname_col = "label", rowname_col = "strategy") |>
        gt::cols_label(
          name = "Mitigator",
          lo = "Low",
          hi = "High",
          comments_lo = "High",
          comments_hi = "Low"
        ) |>
        gt::tab_spanner("Values", c("lo", "hi")) |>
        gt::tab_spanner("Comments", c("comments_lo", "comments_hi")) |>
        gt::fmt_integer(c("lo", "hi")) |>
        gt::tab_options(
          row_group.border.top.width = gt::px(2),
          row_group.border.top.color = "black",
          row_group.border.bottom.color = "black",
          row_group.background.color = "#686f73",
        )
    })
  })
}
