# Prep for the THF data

# Helper function to calculate CAGR
calc_cagr <- function(start_val, end_val, years) {
  if (is.na(start_val) || is.na(end_val) || years <= 0) {
    return(NA)
  }
  return((end_val / start_val)^(1 / years) - 1)
}

# Calculate min/max CAGRs
calc_cagr_series <- function(data, window) {
  cagr_vals <- sapply(1:(nrow(data) - window + 1), function(i) {
    start_val <- data$Productivity_Index[i]
    end_val <- data$Productivity_Index[i + window - 1]
    calc_cagr(start_val, end_val, window) * 100
  })
  return(c(min(cagr_vals, na.rm = TRUE), max(cagr_vals, na.rm = TRUE)))
}

hist_data <-
  "targets/Mockup_elicitation_v2.xls" |>
  readxl::read_excel(
    sheet = 'Index',
    skip = 6
  ) |>
  dplyr::select(Period, `Non-Quality Adjusted Productivity Index`) |>
  dplyr::rename(
    Year = Period,
    Productivity_Index = `Non-Quality Adjusted Productivity Index`
  ) |>
  dplyr::mutate(
    Year = as.numeric(substr(Year, 1, 4)),
    Productivity_Index = as.numeric(Productivity_Index)
  ) |>
  dplyr::arrange(Year) |>
  dplyr::mutate(
    Growth = (Productivity_Index / dplyr::lag(Productivity_Index) - 1) * 100
  )
readr::write_csv(hist_data, "inst/app/data/hist_data.csv")

# Pre-COVID metrics (up to 2018)
hist_pre <- hist_data |> dplyr::filter(Year <= 2018)

pre_long_avg <- calc_cagr(
  hist_pre$Productivity_Index[1],
  tail(hist_pre$Productivity_Index, 1),
  nrow(hist_pre)
) *
  100

pre_f5_min <- pre_f5_max <- pre_f10_min <- pre_f10_max <- NA

if (nrow(hist_pre) >= 5) {
  vals <- calc_cagr_series(hist_pre, 5)
  pre_f5_min <- vals[1]
  pre_f5_max <- vals[2]
}
if (nrow(hist_pre) >= 10) {
  vals <- calc_cagr_series(hist_pre, 10)
  pre_f10_min <- vals[1]
  pre_f10_max <- vals[2]
}

# Including-COVID metrics (all data)
long_term_avg <- calc_cagr(
  hist_data$Productivity_Index[1],
  tail(hist_data$Productivity_Index, 1),
  nrow(hist_data)
) *
  100

f5_min <- f5_max <- f10_min <- f10_max <- NA
if (nrow(hist_data) >= 5) {
  vals <- calc_cagr_series(hist_data, 5)
  f5_min <- vals[1]
  f5_max <- vals[2]
}
if (nrow(hist_data) >= 10) {
  vals <- calc_cagr_series(hist_data, 10)
  f10_min <- vals[1]
  f10_max <- vals[2]
}

cagr_table <-
  tibble::tibble(
    `Metric` = c(
      "10 yr CAGR min",
      "5 yr CAGR min",
      "Long-term CAGR avg",
      "5 yr CAGR max",
      "10 yr CAGR max"
    ),
    `	Pre-Covid (1995/96 to 2018/19)` = c(
      pre_f10_min,
      pre_f5_min,
      pre_long_avg,
      pre_f5_max,
      pre_f10_max
    ),
    `Including Covid (1995/96 to present)` = c(
      f10_min,
      f5_min,
      long_term_avg,
      f5_max,
      f10_max
    )
  ) |>
  dplyr::mutate(across(where(is.numeric), ~ round(.x, 2)))
readr::write_csv(cagr_table, "inst/app/data/cagr_data.csv")


# Load recent estimated growth data
est_raw <- readxl::read_excel(
  "targets/referencetable2024q4 (2).xlsx",
  sheet = "Table_4",
  skip = 3
)
names(est_raw)[c(1, 7)] <- c("Year", "Growth")
est <- est_raw |>
  dplyr::select(Year, Growth) |>
  dplyr::mutate(Year = as.numeric(Year), Growth = as.numeric(Growth)) |>
  dplyr::arrange(Year) |>
  dplyr::filter(Year > 2021)

disc_start <- tail(hist_data$Productivity_Index, 1)
disc_idx <- disc_start * cumprod(1 + est$Growth / 100)
disc_data <- dplyr::bind_rows(
  hist_data |>
    dplyr::slice(dplyr::n()) |>
    dplyr::select(Year, Productivity_Index, Growth),
  tibble::tibble(
    Year = est$Year,
    Productivity_Index = disc_idx,
    Growth = est$Growth
  )
)
readr::write_csv(disc_data, "inst/app/data/disc_data.csv")
