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
  s <- hex_string_to_raw(salt)

  email |>
    stringr::str_to_lower() |>
    stringr::str_trim() |>
    openssl::bcrypt_pbkdf(s) |>
    base64enc::base64encode()
}
