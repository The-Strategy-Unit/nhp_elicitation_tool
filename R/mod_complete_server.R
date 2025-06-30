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
        dplyr::filter(
          !(.data[["low_0_5"]] == 0 &
            .data[["low_5_10"]] == 0 &
            .data[["high_0_5"]] == 0 &
            .data[["high_5_10"]] == 0)
        ) |>
        dplyr::select(
          "strategy",
          "low_avg",
          "high_avg",
          "comments_low",
          "comments_high"
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
          name = "Service Area",
          low_avg = "Low",
          high_avg = "High",
          comments_low = "Low",
          comments_high = "High"
        ) |>
        gt::tab_spanner("Values", c("low_avg", "high_avg")) |>
        gt::tab_spanner("Comments", c("comments_low", "comments_high")) |>
        gt::fmt_number(c("lo", "hi"), decimals = 1) |>
        gt::tab_options(
          row_group.border.top.width = gt::px(2),
          row_group.border.top.color = "black",
          row_group.border.bottom.color = "black",
          row_group.background.color = "#686f73",
        )
    })
  })
}
