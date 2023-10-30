#' view_results Server Functions
#'
#' @noRd
mod_view_results_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    .data <- rlang::.data

    all_data <- shiny::reactive({
      get_all_users_results() |>
        dplyr::mutate(
          which_phase = ifelse(
            .data[["timestamp"]] <= get_phase_1_end(),
            1,
            2
          ),
          .after = "timestamp"
        )
    })

    output$download_results <- shiny::downloadHandler(
      file = "results.csv",
      content = \(filename) {
        readr::write_csv(all_data(), filename)
      }
    )

    users_completed <- shiny::reactive({
      users <- app_sys("app", "data", "user_mappings.json") |>
        jsonlite::read_json(simplifyVector = TRUE) |>
        purrr::map_depth(2, length) |>
        purrr::map(purrr::compose(sum, purrr::flatten_dbl)) |>
        purrr::simplify() |>
        tibble::enframe("email", "total")

      all_data() |>
        dplyr::count(.data[["email"]]) |>
        dplyr::right_join(
          users,
          by = dplyr::join_by("email")
        ) |>
        tidyr::replace_na(list(n = 0)) |>
        dplyr::mutate(
          pcnt = .data[["n"]] / .data[["total"]],
          dplyr::across(
            "email",
            \(.x) forcats::fct_reorder(.x, .data[["pcnt"]])
          )
        ) |>
        dplyr::arrange(dplyr::desc(.data[["pcnt"]]))
    })

    results_data <- shiny::reactive({
      all_data() |>
        dplyr::filter(.data[["strategy"]] == input$strategy) |>
        dplyr::select("lo", "hi") |>
        dplyr::arrange(.data[["lo"]], .data[["hi"]]) |>
        dplyr::mutate(rn = dplyr::row_number())
    }) |>
      shiny::bindEvent(input$strategy)

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

    output$total_users <- shiny::renderText({
      users_completed() |>
        nrow()
    })

    output$partially_completed <- shiny::renderText({
      users_completed() |>
        dplyr::filter(
          .data[["pcnt"]] > 0,
          .data[["pcnt"]] < 1
        ) |>
        nrow()
    })

    output$fully_completed <- shiny::renderText({
      users_completed() |>
        dplyr::filter(
          .data[["pcnt"]] == 1
        ) |>
        nrow()
    })

    output$completed_plot <- shiny::renderPlot({
      users_completed() |>
        ggplot2::ggplot(
          ggplot2::aes(.data[["pcnt"]], .data[["email"]])
        ) +
        ggplot2::geom_col() +
        ggplot2::scale_x_continuous(labels = scales::percent) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.y = ggplot2::element_blank()
        )
    })
  })
}
