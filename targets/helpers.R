year_to_fyear <- function(year) {
  as.integer(year * 100 + (year + 1) %% 100)
}
