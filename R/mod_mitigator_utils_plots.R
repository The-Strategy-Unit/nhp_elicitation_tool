mitigator_trend_plot <- function(data, param_table, value_format, min_year, y_title) {
  .data <- rlang::.data # suppress lintr warnings

  fyear_format <- scales::number_format(
    decimal.mark = "/",
    big.mark = "",
    accuracy = 0.01,
    scale = 0.01
  )

  data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["year"]]),
    ) +
    ggplot2::annotate(
      "ribbon",
      x = param_table$year,
      ymin = param_table$value_lo[[1]] * c(1, 0),
      ymax = param_table$value_lo[[1]] * c(1, 1),
      fill = "#2c2825",
      alpha = 0.05
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["rate"]]),
      colour = "#2c2825"
    ) +
    ggplot2::geom_ribbon(
      data = param_table,
      ggplot2::aes(
        ymin = .data[["value_lo"]],
        ymax = .data[["value_hi"]]
      ),
      fill = "#fcdf83",
      colour = "#2c2825"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        y = .data[["rate"]],
        text = glue::glue(
          "Year: {fyear_format(year)}\n",
          "Value: {value_format(rate)}\n",
          "N: {scales::comma(n)}"
        )
      ),
      shape = "circle filled",
      fill = "#686f73",
      colour = "#2c2825",
      size = 3
    ) +
    ggplot2::scale_x_continuous(
      breaks = c(min_year, param_table$year),
      labels = fyear_format,
      expand = ggplot2::expansion(c(0.05, 0)),
      limits = c(min_year, param_table$year[[2]])
    ) +
    ggplot2::scale_y_continuous(
      labels = value_format,
      sec.axis = ggplot2::dup_axis(
        breaks = c(
          param_table$value_lo[[2]],
          param_table$value_hi[[2]]
        ),
        name = NULL
      )
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      x = "Financial Year of Admission",
      y = y_title
    ) +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "#2c2825"),
      axis.ticks = ggplot2::element_line(colour = "#2c2825"),
      axis.line.y.right = ggplot2::element_blank(),
      axis.ticks.y.right = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "#e8e9ea"
      ),
      panel.grid.minor.y = ggplot2::element_line(
        colour = "#e8e9ea",
        linetype = "dashed"
      )
    )
}

mitigator_results_plot <- function(data, values, email) {
  data |>
    dplyr::mutate(
      is_me = .data[["email"]] == .env[["email"]],
      mean = (.data[["lo"]] + .data[["hi"]]) / 2
    ) |>
    dplyr::filter(
      .data[["is_me"]] | !(
        .data[["lo"]] == 0 & .data[["hi"]] == 100
      )
    ) |>
    dplyr::arrange(.data[["mean"]]) |>
    dplyr::mutate(
      y = dplyr::row_number()
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        y = .data[["y"]],
        colour = .data[["is_me"]]
      )
    ) +
    ggplot2::geom_rect(
      xmin = values[[1]],
      xmax = values[[2]],
      ymin = 0, ymax = nrow(data) + 1,
      fill = "#fef2cd",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(xintercept = values[[1]], colour = "#fcdf83") +
    ggplot2::geom_vline(xintercept = values[[2]], colour = "#fcdf83") +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data[["lo"]],
        text = .data[["comments_lo"]]
      ),
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data[["hi"]],
        text = .data[["comments_hi"]]
      ),
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data[["lo"]], xend = .data[["hi"]],
        yend = .data[["y"]]
      ),
      lwd = 1.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "#5881c1", "FALSE" = "#9d928a")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
}
