#' db
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
get_db <- function(envir = parent.frame()) {
  db_path <- file.path(
    Sys.getenv("save_path", "."),
    "nhp_eet.db"
  )
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)

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
  id <- uuid::UUIDgenerate()
  timestamp <- as.integer(Sys.time())

  DBI::dbAppendTable(
    get_db(),
    "results",
    data.frame(
      id = id,
      email = email,
      timestamp = timestamp,
      strategy = strategy,
      lo = values[[1]],
      hi = values[[2]],
      comments_lo = comments_lo,
      comments_hi = comments_hi
    )
  )

  return(list(id, timestamp))
}

lazy_get_latest_results <- function(
    phase_1 = TRUE,
    con = get_db(parent.frame())) {
  # make sure con is owned by the calling function
  comp_fn <- ifelse(phase_1, `<=`, `>`)
  cutoff <- as.integer(get_phase_1_end())

  dplyr::tbl(con, "results") |>
    dplyr::filter(
      (!!comp_fn)(.data[["timestamp"]], cutoff)
    ) |>
    dplyr::slice_max(order_by = timestamp, n = 1, by = c("email", "strategy"))
}

get_all_users_results <- function(phase_1 = is_phase_1(), strategy) {
  r <- lazy_get_latest_results(phase_1) |>
    dplyr::select(-"id", -"timestamp")

  if (!missing(strategy)) {
    r <- dplyr::filter(r, .data[["strategy"]] == .env[["strategy"]])
  }

  dplyr::collect(r)
}

get_latest_results <- function(email, strategy, phase_1 = is_phase_1()) {
  r <- lazy_get_latest_results(phase_1) |>
    dplyr::filter(
      .data[["email"]] == .env[["email"]]
    ) |>
    dplyr::select("strategy", tidyselect::matches("(lo|hi)$"))

  if (!missing(strategy)) {
    r <- dplyr::filter(r, .data[["strategy"]] == .env[["strategy"]])
  }

  dplyr::collect(r)
}
