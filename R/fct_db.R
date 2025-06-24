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
        low_0_5 = "REAL NOT NULL",
        low_6_10 = "REAL NOT NULL",
        low_avg = "REAL NOT NULL",
        high_0_5 = "REAL NOT NULL",
        high_6_10 = "REAL NOT NULL",
        high_avg = "REAL NOT NULL",
        comments_low = "TEXT NOT NULL",
        comments_high = "TEXT NOT NULL"
      )
    )

    DBI::dbExecute(
      con,
      "CREATE INDEX IX_results ON results (email, strategy, timestamp)"
    )
  }

  return(con)
}

insert_data <- function(
  email,
  strategy,
  low_0_5,
  low_6_10,
  low_avg,
  high_0_5,
  high_6_10,
  high_avg,
  comments_low,
  comments_high
) {
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
      low_0_5 = low_0_5,
      low_6_10 = low_6_10,
      low_avg = low_avg,
      high_0_5 = high_0_5,
      high_6_10 = high_6_10,
      high_avg = high_avg,
      comments_low = comments_low,
      comments_high = comments_high
    )
  )

  return(list(id, timestamp))
}

lazy_get_latest_results <- function(
  phase_1,
  con = get_db(parent.frame())
) {
  res <- dplyr::tbl(con, "results")

  if (!missing(phase_1)) {
    # make sure con is owned by the calling function
    comp_fn <- ifelse(phase_1, `<=`, `>`)
    cutoff <- as.integer(get_phase_1_end())

    res <- res |>
      dplyr::filter(
        (!!comp_fn)(.data[["timestamp"]], cutoff)
      )
  }

  res |>
    dplyr::slice_max(order_by = timestamp, n = 1, by = c("email", "strategy"))
}

get_all_users_results <- function(phase_1, strategy) {
  r <- lazy_get_latest_results(phase_1) |>
    dplyr::select(-"id")

  if (!missing(strategy)) {
    r <- dplyr::filter(r, .data[["strategy"]] == .env[["strategy"]])
  }

  r |>
    dplyr::collect() |>
    dplyr::mutate(
      dplyr::across("timestamp", as.POSIXct)
    )
}

get_latest_results <- function(email, strategy, phase_1 = is_phase_1()) {
  r <- lazy_get_latest_results(phase_1) |>
    dplyr::filter(
      .data[["email"]] == .env[["email"]]
    ) |>
    dplyr::select("strategy", tidyselect::matches("(lo|hi)"))

  if (!missing(strategy)) {
    r <- dplyr::filter(r, .data[["strategy"]] == .env[["strategy"]])
  }

  dplyr::collect(r)
}
