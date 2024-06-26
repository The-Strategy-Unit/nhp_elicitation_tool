.data <- rlang::.data
.env <- rlang::.env

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
    dplyr::select(name = "TABLE_NAME", view = "VIEW_DEFINITION") |>
    dplyr::collect() |>
    dplyr::mutate(
      dplyr::across(
        "view",
        ~ purrr::map_chr(stringr::str_squish(.x), rlang::hash)
      )
    ) |>
    purrr::array_tree()
}

get_strategies_subset <- function(strategies, pattern) {
  purrr::keep(
    strategies,
    ~ stringr::str_detect(.x$name, pattern)
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

get_values_rates <- function(strategy, start, end) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy$name)) |>
    dplyr::filter(
      .data[["FYEAR"]] |> dplyr::between(start, end),
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

get_values_los <- function(strategy, start, end) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy$name)) |>
    dplyr::filter(
      .data[["FYEAR"]] |> dplyr::between(start, end),
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

get_values_pcnts <- function(strategy, start, end) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy$name)) |>
    dplyr::filter(
      .data[["FYEAR"]] |> dplyr::between(start, end),
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

get_values_op <- function(strategy, start, end) {
  con <- db_con()

  dplyr::tbl(
    con,
    dbplyr::in_schema(
      "nhp_strategies",
      strategy$name
    )
  ) |>
    dplyr::filter(
      .data[["FYEAR"]] |> dplyr::between(start, end),
      !is.na(.data[["age"]])
    ) |>
    dplyr::summarise(
      n = sum(.data[["fraction"]], na.rm = TRUE),
      d = dplyr::n(),
      .by = c("fyear", "age", "sex", "strategy")
    ) |>
    dplyr::collect() |>
    fix_ages("strategy")
}

get_fixed_values_op <- function(values_op) {
  values_op |>
    dplyr::mutate(
      dplyr::across(
        "d",
        ~ .x - ifelse(
          stringr::str_starts(.data[["strategy"]], "followup_reduction"),
          .data[["n"]],
          0
        )
      )
    )
}

get_values_aae <- function(strategy, start, end) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy$name)) |>
    dplyr::filter(
      .data[["FYEAR"]] |> dplyr::between(start, end),
      !is.na(.data[["age"]])
    ) |>
    dplyr::summarise(
      n = sum(.data[["fraction"]], na.rm = TRUE),
      d = dplyr::n(),
      .by = c("fyear", "age", "sex", "strategy")
    ) |>
    dplyr::collect() |>
    fix_ages("strategy")
}

get_fixed_values_pcnts <- function(values_pcnts) {
  values_pcnts |>
    dplyr::summarise(
      dplyr::across(c("n", "d"), sum),
      .by = -c("n", "d")
    )
}

get_total_admissions <- function(start, end) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients")) |>
    dplyr::filter(
      .data[["FYEAR"]] |> dplyr::between(start, end),
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

get_age_standardised_rates <- function(values_rates, values_los, values_pcnts,
                                       values_op, values_aae,
                                       total_admissions) {
  dplyr::bind_rows(
    values_rates |>
      dplyr::inner_join(
        total_admissions |>
          dplyr::rename(d = "admissions"),
        by = dplyr::join_by("fyear", "age", "sex")
      ),
    values_los |>
      dplyr::rename(d = "n", n = "days"),
    values_pcnts,
    values_op,
    values_aae
  ) |>
    tidyr::complete(
      .data[["fyear"]],
      age = 0:90,
      .data[["sex"]],
      .data[["strategy"]],
      fill = list(n = 0)
    ) |>
    dplyr::filter(
      .data[["age"]] >= min(
        ifelse(.data[["n"]] > 0, .data[["age"]], NA),
        na.rm = TRUE
      ),
      .data[["age"]] <= max(
        ifelse(.data[["n"]] > 0, .data[["age"]], NA),
        na.rm = TRUE
      ),
      .by = c("strategy")
    ) |>
    dplyr::filter(
      sum(.data[["n"]]) > 0,
      .by = c("strategy", "sex")
    ) |>
    dplyr::filter(.data[["n"]] > 0) |>
    dplyr::inner_join(
      total_admissions |>
        dplyr::filter(.data[["fyear"]] == 201920) |>
        dplyr::select(-"fyear") |>
        dplyr::rename(p = "admissions"),
      by = dplyr::join_by("age", "sex")
    ) |>
    dplyr::mutate(
      rate = .data[["n"]] / .data[["d"]] * .data[["p"]]
    ) |>
    dplyr::summarise(
      dplyr::across(c("n", "d", "p", "rate"), sum),
      rate = .data[["rate"]] / .data[["p"]],
      .by = c("fyear", "strategy")
    )
}

create_trend_data <- function(age_standardised_rates) {
  filename <- "inst/app/data/trend_data.csv"

  age_standardised_rates |>
    dplyr::rename(year = "fyear") |>
    readr::write_csv(filename)

  filename
}
