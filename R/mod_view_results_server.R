#' view_results Server Functions
#'
#' @noRd
mod_view_results_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    .data <- rlang::.data

    results_data <- shiny::reactive({
      input$strategies |>
        shiny::req() |>
        get_all_users_results() |>
        dplyr::arrange(.data[["lo"]], .data[["hi"]]) |>
        dplyr::mutate(rn = dplyr::row_number())
    }) |>
      shiny::bindEvent(input$strategies)

    output$distributions <- shiny::renderPlot({
      results_data() |>
        tidyr::pivot_longer(-"rn") |>
        ggplot2::ggplot(
          ggplot2::aes(
            .data[["value"]],
            colour = .data[["name"]]
          )
        ) +
        ggplot2::geom_histogram(
          ggplot2::aes(
            fill = ggplot2::after_scale(
              colorspace::lighten(colour, 0.5) # nolint
            )
          ),
          binwidth = 1
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none")
    })

    output$individuals <- shiny::renderPlot({
      results_data() |>
        ggplot2::ggplot(
          ggplot2::aes(x = .data[["lo"]], y = .data[["rn"]])
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(xend = .data[["hi"]], yend = .data[["rn"]])
        ) +
        ggplot2::geom_point(
          data = \(.x) tidyr::pivot_longer(.x, -"rn"),
          ggplot2::aes(x = .data[["value"]])
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          panel.grid.minor.y = ggplot2::element_blank()
        )
    })

    output$table <- gt::render_gt({
      results_data() |>
        dplyr::select(-"rn") |>
        gt::gt()
    })
  })
}
