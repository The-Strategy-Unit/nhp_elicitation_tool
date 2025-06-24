mitigator_results_table <- function(data, values, email) {
  data |>
    dplyr::mutate(
      is_me = .data[["email"]] == .env[["email"]],
      mean = (.data[["low_avg"]] + .data[["high_avg"]]) / 2 # This mean is just used for arranging the rows
    ) |>
    dplyr::arrange(.data[["mean"]]) |>
    dplyr::mutate(
      y = dplyr::row_number()
    ) |>
    dplyr::select(-y, -email, -timestamp, -strategy, -mean)
}

mitigator_results_plot <- function(data, values, email) {
  data |>
    dplyr::mutate(
      is_me = .data[["email"]] == .env[["email"]],
      mean = (.data[["low_avg"]] + .data[["high_avg"]]) / 2
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
      ymax = 0,
      ymin = -(nrow(data) + 1),
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
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "none",
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::scale_y_reverse()
}

index_plot <- function(hist_data, disc_data, proj) {
  df_all <- dplyr::bind_rows(
    hist_data |>
      dplyr::select(Year, Productivity_Index) |>
      dplyr::mutate(Source = "Historical"),
    disc_data |>
      dplyr::select(Year, Productivity_Index) |>
      dplyr::mutate(Source = "Estimate")
  )

  p <- ggplot2::ggplot(
    df_all,
    ggplot2::aes(Year, Productivity_Index, color = Source, linetype = Source)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(
      values = c(Historical = "#444", Estimate = "purple")
    ) +
    ggplot2::scale_linetype_manual(
      values = c(Historical = "solid", Estimate = "twodash")
    ) +
    ggplot2::theme_minimal()

  dfp <- proj
  lo <- hi <- numeric(nrow(dfp))
  base <- tail(disc_data$Productivity_Index, 1)
  for (i in seq_len(nrow(dfp))) {
    lo[i] <- if (i == 1) {
      base * (1 + dfp$Low[i] / 100)
    } else {
      lo[i - 1] * (1 + dfp$Low[i] / 100)
    }
    hi[i] <- if (i == 1) {
      base * (1 + dfp$High[i] / 100)
    } else {
      hi[i - 1] * (1 + dfp$High[i] / 100)
    }
  }

  df_r <- data.frame(Year = dfp$Year, lo, hi)
  p <- p +
    ggplot2::geom_ribbon(
      data = df_r,
      ggplot2::aes(x = Year, ymin = lo, ymax = hi),
      inherit.aes = FALSE,
      fill = "steelblue",
      alpha = 0.2
    )

  plotly::ggplotly(p)
}


growth_plot <- function(hist_data, disc_data, proj, long_term_avg) {
  df_bar <- dplyr::bind_rows(
    hist_data |>
      dplyr::filter(!is.na(Growth) & Year <= 2021) |>
      dplyr::mutate(Source = "Historical"),
    disc_data |>
      dplyr::filter(Year %in% c(2022, 2023, 2024)) |>
      dplyr::select(Year, Growth) |>
      dplyr::mutate(Source = "Estimate")
  )
  p <- ggplot2::ggplot(df_bar, ggplot2::aes(Year, Growth, fill = Source)) +
    ggplot2::geom_col(position = "identity", alpha = 0.8) +
    ggplot2::geom_hline(
      yintercept = long_term_avg,
      linetype = "dotted",
      color = "#333"
    ) +
    ggplot2::scale_fill_manual(
      values = c(Historical = "gray70", Estimate = "tomato")
    ) +
    ggplot2::theme_minimal()

  dfp <- proj
  p <- p +
    ggplot2::geom_col(
      data = dfp,
      ggplot2::aes(x = Year - 0.2, y = Low),
      width = 0.4,
      fill = "forestgreen",
      alpha = 0.7
    ) +
    ggplot2::geom_col(
      data = dfp,
      ggplot2::aes(x = Year + 0.2, y = High),
      width = 0.4,
      fill = "blue",
      alpha = 0.7
    )

  plotly::ggplotly(p)
}
