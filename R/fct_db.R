#' db
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_db <- function(envir = parent.frame()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), "nhp_eet.db")

  withr::defer(DBI::dbDisconnect(con), envir = envir)

  if (!"results" %in% DBI::dbListTables(con)) {
    DBI::dbCreateTable(
      con,
      "results",
      list(
        id = "TEXT NOT NULL PRIMARY KEY",
        email = "TEXT NOT NULL",
        timestamp = "INT NOT NULL",
        strategy = "TEXT NOT NULL",
        lo = "REAL NOT NULL",
        hi = "REAL NOT NULL",
        comments_lo = "TEXT NOT NULL",
        comments_hi = "TEXT NOT NULL"
      )
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IX_results ON results (email, strategy, timestamp)"
    )
  }

  return(con)
}

insert_data <- function(email, strategy, values, comments_lo, comments_hi) {
  DBI::dbAppendTable(
    get_db(),
    "results",
    data.frame(
      id = uuid::UUIDgenerate(),
      email = email,
      timestamp = as.integer(Sys.time()),
      strategy = strategy,
      lo = values[[1]],
      hi = values[[2]],
      comments_lo = comments_lo,
      comments_hi = comments_hi
    )
  )
}

get_latest_results <- function(email) {
  dplyr::tbl(get_db(), "results") |>
    dplyr::filter(
      .data[["email"]] == .env[["email"]]
    ) |>
    dplyr::slice_max(order_by = timestamp, n = 1, by = "strategy") |>
    dplyr::select("strategy", tidyselect::matches("(lo|hi)$")) |>
    dplyr::collect()
}

get_latest_result <- function(email, strategy) {
  dplyr::tbl(get_db(), "results") |>
    dplyr::filter(
      .data[["email"]] == .env[["email"]],
      .data[["strategy"]] == .env[["strategy"]]
    ) |>
    dplyr::slice_max(order_by = timestamp, n = 1) |>
    dplyr::select(tidyselect::matches("(lo|hi)$")) |>
    dplyr::collect()
}
