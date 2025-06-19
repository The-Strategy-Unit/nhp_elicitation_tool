mitigator_results_table <- function(data, values, email) {
  data |>
    dplyr::mutate(
      is_me = .data[["email"]] == .env[["email"]],
      mean = (.data[["low_avg"]] + .data[["high_avg"]]) / 2
    ) |>
    dplyr::arrange(.data[["mean"]]) |>
    dplyr::mutate(
      y = dplyr::row_number()
    ) |>
    dplyr::select(-y, -email, -timestamp, -strategy)
}

mitigator_results_plot <- function(data, values, email) {
  data |>
    dplyr::mutate(
      is_me = .data[["email"]] == .env[["email"]],
      mean = (.data[["low_avg"]] + .data[["high_avg"]]) / 2
    ) |>
    #dplyr::filter(
    #  .data[["is_me"]] |
    #    !(.data[["lo"]] == get_golem_config("range")$low &
    #      .data[["hi"]] == get_golem_config("range")$high)
    #) |>
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
      ymin = 0,
      ymax = nrow(data) + 1,
      fill = "#fef2cd",
      text = "hi",
      show.legend = FALSE
    ) +
    ggplot2::geom_vline(xintercept = values[[1]], colour = "#fcdf83") +
    ggplot2::geom_vline(xintercept = values[[2]], colour = "#fcdf83") +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data[["low_avg"]],
        text = .data[["comments_low"]]
      ),
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data[["high_avg"]],
        text = .data[["comments_high"]]
      ),
      size = 3,
      show.legend = FALSE
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = .data[["low_avg"]],
        xend = .data[["high_avg"]],
        yend = .data[["y"]]
      ),
      lwd = 1.5,
      show.legend = FALSE
    ) +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "#5881c1", "FALSE" = "#9d928a")
    ) +
    ggplot2::xlim(
      get_golem_config("range")$low,
      get_golem_config("range")$high
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
}
