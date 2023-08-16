mitigator_trend_plot <- function(data, param_table, value_format) {
  .data <- rlang::.data # suppress lintr warnings

  data |>
    ggplot2::ggplot(
      ggplot2::aes(x = .data[["year"]]),
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data[["value"]]),
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
      ggplot2::aes(y = .data[["value"]]),
      shape = "circle filled",
      fill = "#686f73",
      colour = "#2c2825",
      size = 3
    ) +
    ggplot2::annotate(
      "point",
      x = param_table$year[[1]],
      y = param_table$value_lo[[1]],
      shape = "circle filled",
      fill = "#f9bf07",
      colour = "#2c2825",
      size = 3
    ) +
    ggplot2::scale_x_continuous(
      breaks = param_table$year,
      labels = scales::number_format(
        decimal.mark = "/",
        big.mark = "",
        accuracy = 0.01,
        scale = 0.01
      ),
      expand = ggplot2::expansion(c(0.05, 0))
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
