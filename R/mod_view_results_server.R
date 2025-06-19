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
        ) |>
        dplyr::mutate(group = get_golem_config("group"))
    })

    output$download_results <- shiny::downloadHandler(
      file = paste0(get_golem_config("group"), "_results.csv"),
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
        dplyr::select(
          #"low_0_5",
          #"low_6_10",
          "low_avg",
          #"high_0_5",
          #"high_6_10",
          "high_avg"
        ) |>
        dplyr::arrange(.data[["low_avg"]], .data[["high_avg"]]) |>
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
        #dplyr::filter(
        #  !(.data[["lo"]] == get_golem_config("range")$low &
        #    .data[["hi"]] == get_golem_config("range")$high)
        #) |> # Remove any values left at default
        ggplot2::ggplot(
          ggplot2::aes(x = .data[["low_avg"]], y = .data[["rn"]])
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(xend = .data[["high_avg"]], yend = .data[["rn"]]),
          lwd = 2
        ) +
        ggplot2::geom_point(
          data = \(.x) tidyr::pivot_longer(.x, -"rn"),
          ggplot2::aes(x = .data[["value"]]),
          size = 4
        ) +
        ggplot2::theme_minimal(base_size = 16) +
        #ggplot2::xlim(
        #  get_golem_config("range")$low,
        #  get_golem_config("range")$high
        #) +
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
