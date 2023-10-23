hex_string_to_raw <- function(s) {
  l <- stringr::str_length(s)

  stopifnot(
    "not a valid hex string" = stringr::str_detect(s, "^([0-9A-Fa-f]{2})+$")
  )

  stringr::str_sub(s, seq(1, l, 2), seq(2, l, 2)) |>
    strtoi(16L) |>
    as.raw()
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

get_phase_1_end <- function() {
  # if the phase 1 end date isn't set, default to maximum value that can be
  # stored as an integer
  as.POSIXct(Sys.getenv("PHASE_1_END", "2038-01-19 03:14:07"), "Europe/London")
}

format_datetime_as_string <- function(d) {
  format(d, tz = "Europe/London", "%a %d %B, %Y at %H:%M:%S")
}

is_phase_1 <- function() {
  Sys.time() <= get_phase_1_end()
}
