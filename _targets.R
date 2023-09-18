library(targets)

tar_source("targets")

list(
  tar_target(
    fyears,
    year_to_fyear(2010:2019)
  ),
  # strategies ----
  tar_target(
    strategies_all,
    get_strategies()
  ),
  tar_target(
    strategies_los,
    get_strategies_subset(
      strategies_all,
      "emergency_elderly",
      "enhanced_recovery_.*",
      "excess_beddays",
      "raid_ip",
      "stroke_early_supported_discharge"
    )
  ),
  tar_target(
    strategies_pcnts,
    get_strategies_subset(
      strategies_all,
      "preop_los"
    )
  ),
  tar_target(
    strategies_rates,
    setdiff(
      strategies_all,
      c(
        strategies_los,
        strategies_pcnts
      )
    )
  ),
  # values ----
  tar_target(
    total_admissions,
    get_total_admissions(fyears),
    pattern = map(fyears)
  ),
  tar_target(
    values_rates,
    get_values(strategies_rates, fyears),
    pattern = cross(strategies_rates, fyears)
  ),
  tar_target(
    values_los,
    get_values_los(strategies_los, fyears),
    pattern = cross(strategies_los, fyears)
  ),
  # age standardisation ----
  tar_target(
    age_standardised_rates,
    get_age_standardised_rates(
      values_rates,
      values_los,
      total_admissions
    ),
  ),
  # save results ----
  tar_target(
    trend_data,
    create_trend_data(
      age_standardised_rates
    ),
    format = "file"
  )
)
