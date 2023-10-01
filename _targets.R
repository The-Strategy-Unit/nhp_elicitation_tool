library(targets)
library(tarchetypes)

tar_source("targets")
source("R/ZZZ.R")

list(
  tar_target(start, 201011),
  tar_target(end, 201920),
  # strategies ----
  tar_target(
    strategies_all,
    get_strategies()
  ),
  tar_target(
    strategies_los,
    get_strategies_subset(
      strategies_all,
      paste0(
        "^ip_(",
        paste(
          "emergency_elderly",
          "enhanced_recovery_.*",
          "excess_beddays",
          "raid_ip",
          "stroke_early_supported_discharge",
          sep = "|"
        ),
        ")$"
      )
    )
  ),
  tar_target(
    strategies_pcnts,
    get_strategies_subset(
      strategies_all,
      paste0(
        "^ip_(",
        paste(
          "ambulatory_emergency_care_.*",
          "preop_los",
          sep = "|"
        ),
        ")$"
      )
    )
  ),
  tar_target(
    strategies_op,
    get_strategies_subset(
      strategies_all,
      "^op"
    )
  ),
  tar_target(
    strategies_aae,
    get_strategies_subset(
      strategies_all,
      "^aae"
    )
  ),
  tar_target(
    strategies_rates,
    setdiff(
      strategies_all,
      c(
        strategies_los,
        strategies_pcnts,
        strategies_op,
        strategies_aae
      )
    )
  ),
  # values ----
  tar_target(
    total_admissions,
    get_total_admissions(start, end)
  ),
  tar_target(
    values_rates,
    get_values_rates(strategies_rates[[1]], start, end),
    pattern = map(strategies_rates)
  ),
  tar_target(
    values_los,
    get_values_los(strategies_los[[1]], start, end),
    pattern = map(strategies_los)
  ),
  tar_target(
    values_pcnts,
    get_values_pcnts(strategies_pcnts[[1]], start, end),
    pattern = map(strategies_pcnts)
  ),
  tar_target(
    fixed_values_pcnts,
    get_fixed_values_pcnts(values_pcnts)
  ),
  tar_target(
    op_consultant_referrals,
    get_op_consultant_referrals(fyears),
    pattern = map(fyears)
  ),
  tar_target(
    op_followup_reduction,
    get_op_followup_reduction(fyears),
    pattern = map(fyears)
  ),
  tar_target(
    op_tele_conversion,
    get_op_tele_conversion(fyears),
    pattern = map(fyears)
  ),
  tar_target(
    values_op,
    get_values_op(strategies_op[[1]], start, end),
    pattern = map(strategies_op)
  ),
  tar_target(
    fixed_values_op,
    get_fixed_values_op(values_op)
  ),
  tar_target(
    values_aae,
    get_values_aae(strategies_aae[[1]], start, end),
    pattern = cross(strategies_aae)
  ),
  # age standardisation ----
  tar_target(
    age_standardised_rates,
    get_age_standardised_rates(
      values_rates,
      values_los,
      fixed_values_pcnts,
      fixed_values_op,
      values_aae,
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
  ),
  # generate user mappings ----
  tar_target(
    user_mapping_raw_excel,
    "recruitment.xlsx",
    format = "file"
  ),
  tar_target(
    user_mapping_file,
    generate_user_mappings(user_mapping_raw_excel),
    format = "file"
  )
)
