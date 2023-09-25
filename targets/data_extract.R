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

get_values_pcnts_bads_opa <- function(fyear) {
  con <- db_con()

  opa <- dplyr::tbl(
    con,
    dbplyr::in_schema("nhp_modelling", "outpatients")
  ) |>
    dplyr::filter(.data[["fyear"]] == .env[["fyear"]])

  opp <- dplyr::tbl(
    con,
    "tbOutpatientsProcedures"
  ) |>
    dplyr::filter(.data[["OPORDER"]] == 1)
  bpl <- dplyr::tbl(
    con,
    dbplyr::in_schema("nhp_modelling_reference", "bads_procedure_lists")
  ) |>
    dplyr::filter(.data[["bads_type"]] %LIKE% "outpatients%")

  opa |>
    dplyr::inner_join(
      opp,
      by = dplyr::join_by("attendkey")
    ) |>
    dplyr::inner_join(
      bpl,
      by = dplyr::join_by("opcode" == "procedure_code_1")
    ) |>
    dplyr::count(
      .data[["fyear"]],
      age = .data[["apptage"]],
      .data[["sex"]],
      strategy = .data[["bads_type"]]
    ) |>
    dplyr::collect() |>
    fix_ages("strategy") |>
    dplyr::mutate(
      dplyr::across("strategy", ~ paste0("bads_", .x)),
      d = .data[["n"]],
      n = 0
    )
}

get_fixed_values_pcnts <- function(values_pcnts, values_pcnts_bads_opa) {
  values_pcnts |>
    dplyr::bind_rows(
      values_pcnts_bads_opa
    ) |>
    dplyr::summarise(
      dplyr::across(c("n", "d"), sum),
      .by = -c("n", "d")
    )
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

get_age_standardised_rates <- function(values_rates, values_los, values_pcnts,
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
    values_pcnts
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
