.data <- rlang::.data


generate_user_mappings <- function(file) {
  mappings <- list(
    "all" = list(
      thf = c(
        "a_productivity"
      )
    )
  ) |>
    purrr::map_depth(1, tibble::enframe, "type", "strategy") |>
    tibble::enframe("code", "mappings")

  output_file <- "inst/app/data/user_mappings.json"

  readxl::read_excel(file) |>
    janitor::clean_names() |>
    tidyr::drop_na(tidyselect::any_of(c("name"))) |>
    dplyr::select(
      "email",
      "all"
    ) |>
    tidyr::pivot_longer("all", names_to = "code", values_to = "include") |>
    dplyr::filter(.data[["include"]] == "Y") |>
    dplyr::inner_join(
      mappings,
      by = dplyr::join_by("code")
    ) |>
    tidyr::unnest("mappings") |>
    tidyr::unnest("strategy") |>
    dplyr::select("email", "type", "strategy") |>
    dplyr::arrange("email", "type", "strategy") |>
    dplyr::summarise(
      dplyr::across("strategy", list),
      .by = c("email", "type")
    ) |>
    dplyr::group_nest(.data[["email"]]) |>
    dplyr::mutate(
      dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))
    ) |>
    dplyr::mutate(
      dplyr::across("email", \(.x) purrr::map_chr(.x, hash_email))
    ) |>
    tibble::deframe() |>
    jsonlite::write_json(
      output_file,
      auto_unbox = TRUE,
      pretty = TRUE
    )

  output_file
}
