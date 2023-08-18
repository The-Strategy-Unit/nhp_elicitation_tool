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


      # targets::tar_read(values) |>
      #   dplyr::rename(fyear = "FYEAR") |>
      #   dplyr::inner_join(
      #     pop(),
      #     by = dplyr::join_by("fyear")
      #   ) |>
      #   dplyr::mutate(
      #     value = .data[["n"]] / .data[["pop"]]
      #   ) |>