get_england_population_by_year <- function() {
  tf <- withr::local_tempfile(fileext = ".xlsx")

  httr::modify_url(
    "https://www.ons.gov.uk/file",
    query = list(
      uri = paste(
        sep = "/",
        "",
        "peoplepopulationandcommunity",
        "populationandmigration",
        "populationestimates",
        "datasets",
        "populationestimatesforukenglandandwalesscotlandandnorthernireland",
        "mid2001tomid2020detailedtimeseries",
        "regionalpopestimatesenglandandwales19712020.xlsx"
      )
    )
  ) |>
    download.file(tf, mode = "wb")

  df <- readxl::read_excel(tf, "Table 4", skip = 4, na = ":") |>
    dplyr::rename(
      "M85" = "M85/85+",
      "F85" = "F85/85+",
      "M90" = "M90+",
      "F90" = "F90+"
    ) |>
    janitor::clean_names() |>
    dplyr::filter(.data[["year_code"]] != "W92000004") |>
    dplyr::select(
      -2,
      -tidyselect::starts_with("all_")
    ) |>
    dplyr::mutate(
      dplyr::across(
        "year_code",
        ~ as.numeric(
          ifelse(
            stringr::str_detect(.x, "^\\d{4}$"),
            .x,
            NA
          )
        )
      )
    ) |>
    tidyr::fill("year_code") |>
    tidyr::pivot_longer(-"year_code") |>
    tidyr::drop_na("value") |>
    tidyr::separate_wider_position("name", c("sex" = 1, "age" = 2), too_few = "align_start") |>
    dplyr::rename("year" = "year_code") |>
    dplyr::mutate(
      dplyr::across("age", as.integer),
      dplyr::across("sex", ~ ifelse(.x == "m", 1, 2))
    ) |>
    dplyr::arrange("year", "sex", "age") |>
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = -"value"
    )

  age_rates <- df |>
    dplyr::filter(
      max(.data[["age"]]) == 90,
      .data[["age"]] >= 85,
      .by = "year"
    ) |>
    dplyr::summarise(
      dplyr::across("value", sum),
      .by = c("sex", "age")
    ) |>
    dplyr::mutate(
      r = .data[["value"]] / sum(.data[["value"]]),
      .by = c("sex")
    ) |>
    dplyr::select(-"value")


  fixed_rows <- df |>
    dplyr::filter(
      max(.data[["age"]]) == 85,
      .data[["age"]] == 85,
      .by = "year"
    ) |>
    tidyr::complete(year, sex, age = 85:90) |>
    tidyr::fill("value") |>
    dplyr::inner_join(
      age_rates,
      by = dplyr::join_by("sex", "age")
    ) |>
    dplyr::mutate(
      dplyr::across("value", ~ round(.x * .data[["r"]]))
    ) |>
    dplyr::select(-"r")

  filename <- "inst/app/data/england_pop.csv"
  df |>
    dplyr::anti_join(
      fixed_rows,
      by = dplyr::join_by("year", "sex", "age")
    ) |>
    dplyr::bind_rows(fixed_rows) |>
    readr::write_csv(filename)

  filename
}
