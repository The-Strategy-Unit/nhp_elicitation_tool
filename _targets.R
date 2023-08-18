library(targets)

tar_source("targets")

list(
  tar_target(
    england_pop,
    get_england_pop(),
    format = "file"
  ),
  tar_target(
    strategies,
    get_strategies()
  ),
  tar_target(
    fyears,
    year_to_fyear(2000:2019)
  ),
  tar_target(
    values,
    get_values(strategies, fyears),
    pattern = cross(strategies, fyears)
  ),
  tar_target(
    preop_los_denominator,
    get_preop_los_denominator(fyears),
    pattern = map(fyears)
  ),
  tar_target(
    rates,
    get_rates(values, england_pop, preop_los_denominator),
    format = "file"
  )
)
