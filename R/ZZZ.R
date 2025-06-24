hex_string_to_raw <- function(s) {
  l <- stringr::str_length(s)

  stopifnot(
    "not a valid hex string" = stringr::str_detect(s, "^([0-9A-Fa-f]{2})+$")
  )

  stringr::str_sub(s, seq(1, l, 2), seq(2, l, 2)) |>
    strtoi(16L) |>
    as.raw()
}

is_valid_number <- function(x) {
  !is.null(x) && is.numeric(x) && !is.na(x)
}

hash_email <- function(email, salt = Sys.getenv("NHP_SALT")) {
  if (Sys.getenv("SKIP_HASHING") != "") {
    return(email)
  }

  s <- hex_string_to_raw(salt)

  email |>
    stringr::str_to_lower() |>
    stringr::str_trim() |>
    openssl::bcrypt_pbkdf(s) |>
    base64enc::base64encode()
}

get_phase_end <- function(phase) {
  # if the phase end date isn't set, default to maximum value that can be
  # stored as an integer
  as.POSIXct(Sys.getenv(phase, "2038-01-19 03:14:07"), "Europe/London")
}

get_phase_1_end <- \() get_phase_end("PHASE_1_END")
get_phase_2_end <- \() get_phase_end("PHASE_2_END")

format_datetime_as_string <- function(d) {
  format(d, tz = "Europe/London", "%a %d %B, %Y at %H:%M:%S")
}

is_phase_1 <- function() {
  Sys.time() <= get_phase_1_end()
}

is_phase_2 <- function() {
  if (is_phase_1()) {
    return(FALSE)
  }
  if (Sys.getenv("PHASE_2_LIVE") == "") {
    return(FALSE)
  }
  Sys.time() <= get_phase_2_end()
}

is_finished <- \() Sys.time() > get_phase_2_end()

app_is_live <- \() is_phase_1() || is_phase_2()

app_state <- function() {
  if (is_phase_1()) {
    "phase_1"
  } else if (is_phase_2()) {
    "phase_2"
  } else if (is_finished()) {
    "finished"
  } else {
    "disabled"
  }
}
