library(targets)

tar_source("targets")

list(
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
  )
)
