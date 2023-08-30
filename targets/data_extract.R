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

get_values <- function(strategy, fyear) {
  con <- db_con()

  dplyr::tbl(con, dbplyr::in_schema("nhp_strategies", strategy)) |>
    dplyr::filter(.data[["FYEAR"]] == fyear) |>
    dplyr::count(
      .data[["FYEAR"]],
      .data[["AGE"]],
      .data[["SEX"]],
      .data[["strategy"]],
      wt = .data[["fraction"]]
    ) |>
    dplyr::collect() |>
    janitor::clean_names()
}

get_preop_los_denominator <- function(fyear) {
  con <- db_con()

  tbl_ip <- dplyr::tbl(con, dbplyr::in_schema("nhp_modelling", "inpatients"))
  tbl_procs <- dplyr::tbl(con, "tbInpatientsProcedures") |>
    dplyr::select(-"FYEAR")

  tbl_ip |>
    dplyr::inner_join(tbl_procs, by = dplyr::join_by("EPIKEY")) |>
    dplyr::filter(
      .data[["FYEAR"]] == fyear,
      .data[["ADMIMETH"]] %LIKE% "1%",
      !.data[["OPCODE"]] %LIKE% "[UYZ]%",
      dplyr::between(
        .data[["OPDATE"]],
        .data[["ADMIDATE"]],
        .data[["DISDATE"]]
      )
    ) |>
    dplyr::count() |>
    dplyr::collect() |>
    dplyr::mutate(fyear = fyear)
}

get_rates <- function(values, england_pop, preop_los_denominator) {
  .data <- rlang::.data

  fyears <- unique(values$fyear)

  pop <- england_pop |>
    readr::read_csv(col_types = "icii") |>
    dplyr::transmute(
      fyear = year_to_fyear(.data[["year"]]),
      .data[["sex"]],
      .data[["age"]],
      pop = .data[["value"]]
    ) |>
    dplyr::filter(.data[["fyear"]] %in% fyears)

  pop_final_year <- pop |>
    dplyr::filter(.data[["fyear"]] == max(fyears)) |>
    dplyr::select(-"fyear") |>
    dplyr::rename(pop_final = "pop")

  filename <- "inst/app/data/trend_data.csv"

  values |>
    dplyr::right_join(
      pop,
      by = dplyr::join_by("fyear", "age", "sex")
    ) |>
    dplyr::right_join(
      pop_final_year,
      by = dplyr::join_by("age", "sex")
    ) |>
    dplyr::mutate(
      dplyr::across("n", ~ tidyr::replace_na(.x, 0)),
      r = .data[["n"]] / .data[["pop"]] * .data[["pop_final"]]
    ) |>
    dplyr::summarise(
      n = sum(.data[["r"]]),
      d = sum(.data[["pop_final"]]),
      rate = .data[["n"]] / .data[["d"]],
      .by = c("fyear", "strategy")
    ) |>
    dplyr::rename(year = "fyear") |>
    readr::write_csv(filename)

  filename
}
