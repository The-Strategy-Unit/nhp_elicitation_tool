.data <- rlang::.data

db_con <- function(database = Sys.getenv("DB_DATABASE"),
                   envir = parent.frame()) {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = Sys.getenv("DB_SERVER"),
    Database = database,
    Trusted_Connection = "True"
  )

  withr::defer(DBI::dbDisconnect(con), envir = envir)

  con
}

get_strategies <- function() {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("INFORMATION_SCHEMA", "VIEWS")) |>
    dplyr::filter(.data[["TABLE_SCHEMA"]] == "nhp_strategies") |>
    dplyr::select("TABLE_NAME") |>
    dplyr::collect() |>
    dplyr::pull(1)
}

get_strategies_subset <- function(strategies, ...) {
  stringr::str_subset(
    strategies,
    paste0(
      "^ip_(",
      paste(
        sep = "|",
        ...
      ),
      ")$"
    )
  )
}

fix_ages <- function(x, ...) {
  x |>
    dplyr::mutate(
      dplyr::across("age", ~ pmax(0, pmin(90, .x)))
    ) |>
    dplyr::group_by(
      dplyr::across(c("fyear", "age", "sex", ...))
    ) |>
    dplyr::summarise(
      dplyr::across(tidyselect::everything(), sum),
      .groups = "drop"
    )
}

get_values <- function(strategy, fyear) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy)) |>
    dplyr::filter(
      .data[["FYEAR"]] == fyear,
      !is.na(.data[["AGE"]])
    ) |>
    dplyr::count(
      .data[["FYEAR"]],
      .data[["AGE"]],
      .data[["SEX"]],
      .data[["strategy"]],
      wt = .data[["fraction"]]
    ) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    fix_ages("strategy")
}

get_values_los <- function(strategy, fyear) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy)) |>
    dplyr::filter(
      .data[["FYEAR"]] == fyear,
      !is.na(.data[["AGE"]]),
      !is.na(.data[["SPELDUR"]])
    ) |>
    dplyr::summarise(
      days = sum(.data[["SPELDUR"]], na.rm = TRUE),
      n = dplyr::n(),
      .by = c("FYEAR", "AGE", "SEX", "strategy")
    ) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    fix_ages("strategy")
}

get_values_pcnts <- function(strategy, fyear) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy)) |>
    dplyr::filter(
      .data[["FYEAR"]] == fyear,
      !is.na(.data[["AGE"]])
    ) |>
    dplyr::summarise(
      n = sum(.data[["fraction"]], na.rm = TRUE),
      d = dplyr::n(),
      .by = c("FYEAR", "AGE", "SEX", "strategy")
    ) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    fix_ages("strategy")
}

get_total_admissions <- function(fyear) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::filter(
      .data[["FYEAR"]] == fyear,
      !is.na(.data[["AGE"]])
    ) |>
    dplyr::count(
      .data[["FYEAR"]],
      .data[["AGE"]],
      .data[["SEX"]],
      name = "admissions"
    ) |>
    dplyr::collect() |>
    janitor::clean_names() |>
    fix_ages()
}

get_age_standardised_rates <- function(values, pop_final_year,
                                       total_admissions) {
  total_admissions |>
    dplyr::inner_join(
      pop_final_year,
      by = dplyr::join_by("age", "sex")
    ) |>
    dplyr::left_join(
      values,
      by = dplyr::join_by("fyear", "age", "sex")
    ) |>
    dplyr::mutate(
      dplyr::across("n", ~ tidyr::replace_na(.x, 0)),
      r = .data[["n"]] / .data[["admissions"]] * .data[["pop_final"]]
    ) |>
    dplyr::summarise(
      n = sum(.data[["r"]]),
      d = sum(.data[["pop_final"]]),
      rate = .data[["n"]] / .data[["d"]],
      .by = c("fyear", "strategy")
    )
}

get_age_standardised_los <- function(values_los, pop_final_year) {
  values_los |>
    tidyr::complete(
      .data[["fyear"]],
      age = 0:90,
      .data[["sex"]],
      .data[["strategy"]]
    ) |>
    dplyr::mutate(
      dplyr::across(c("days", "n"), ~ tidyr::replace_na(.x, 0))
    ) |>
    dplyr::filter(
      .data[["age"]] >= min(
        ifelse(.data[["n"]] > 0, .data[["age"]], NA),
        na.rm = TRUE
      ),
      .by = c("strategy")
    ) |>
    dplyr::inner_join(
      pop_final_year,
      by = dplyr::join_by("age", "sex")
    ) |>
    dplyr::mutate(
      r = ifelse(
        .data[["n"]] > 0,
        .data[["days"]] / .data[["n"]] * .data[["pop_final"]],
        0
      )
    ) |>
    dplyr::summarise(
      n = sum(.data[["pop_final"]]),
      rate = sum(.data[["r"]]) / .data[["n"]],
      .by = c("fyear", "strategy")
    )
}

create_trend_data <- function(...) {
  filename <- "inst/app/data/trend_data.csv"

  dplyr::bind_rows(...) |>
    dplyr::rename(year = "fyear") |>
    readr::write_csv(filename)

  filename
}
