year_to_fyear <- function(year) {
  as.integer(year * 100 + (year + 1) %% 100)
}

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
      .data[["strategy"]],
      wt = .data[["fraction"]]
    ) |>
    dplyr::collect()
}

get_england_pop <- function() {
  url <- httr::modify_url(
    "https://www.ons.gov.uk/generator",
    query = list(
      format="csv",
      uri=paste(
        sep = "/",
        "",
        "peoplepopulationandcommunity",
        "populationandmigration",
        "populationestimates",
        "timeseries",
        "enpop",
        "pop"
      )
    )
  )
  
  filename <- "inst/app/data/england_pop.csv"

  df <- readr::read_csv(
    url,
    skip = 8,
    col_names = c("year", "pop"),
    col_types = "dd"
  )
  
  readr::write_csv(df, filename)

  filename
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
  fyears <- unique(values$FYEAR)

  pop <- england_pop |>
    readr::read_csv(col_types = "ii") |>
    dplyr::transmute(
      fyear = year_to_fyear(.data[["year"]]),
      n = .data[["pop"]]
    ) |>
    dplyr::filter(fyear %in% fyears)

  denominators <- dplyr::bind_rows(
    .id = "strategy",
    "alcohol_partially_attributable-acute" = pop,
    "alcohol_partially_attributable-chronic" = pop,
    "alcohol_wholly_attributable" = pop, 
    "cancelled_operations" = pop,
    "emergency_elderly" = pop,
    "eol_care-0-2_days" = pop,
    "eol_care-3+_days" = pop,
    "frail_elderly-high" = pop,
    "frail_elderly-intermediate" = pop,
    "frail_elderly-low" = pop,
    "intentional_self_harm" = pop,
    "medically_unexplained_related_admissions" = pop, 
    "obesity_related_admissions" = pop,
    "preop_los-1day" = preop_los_denominator,
    "preop_los-2day" = preop_los_denominator,
    "raid_ae" = pop,
    "raid_ip" = pop, 
    "readmission_within_28_days" = pop,
    "smoking" = pop,
    "stroke_early_supported_discharge" = pop,
    "zero_los_no_procedure-adult" = pop,
    "zero_los_no_procedure-child" = pop
  ) |>
    dplyr::rename(d = "n")

  filename <- "inst/app/data/trend_data.csv"

  values |>
    dplyr::rename(year = "FYEAR") |>
    dplyr::inner_join(denominators, by = c("strategy", "year" = "fyear")) |>
    dplyr::mutate(rate = .data[["n"]] / .data[["d"]]) |>
    readr::write_csv(filename)
  
  filename
}


      # targets::tar_read(values) |>
      #   dplyr::rename(fyear = "FYEAR") |>
      #   dplyr::inner_join(
      #     pop(),
      #     by = dplyr::join_by("fyear")
      #   ) |>
      #   dplyr::mutate(
      #     value = .data[["n"]] / .data[["pop"]]
      #   ) |>