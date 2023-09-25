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
      "ambulatory_emergency_care_.*",
      "bads",
      "preop_los"
    )
  ),
  tar_target(
    strategies_op,
    stringr::str_subset(strategies_all, "^op")
  ),
  tar_target(
    strategies_aae,
    stringr::str_subset(strategies_all, "^aae")
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
  tar_target(
    values_pcnts,
    get_values_pcnts(strategies_pcnts, fyears),
    pattern = cross(strategies_pcnts, fyears)
  ),
  tar_target(
    values_pcnts_bads_opa,
    get_values_pcnts_bads_opa(fyears),
    pattern = map(fyears)
  ),
  tar_target(
    fixed_values_pcnts,
    get_fixed_values_pcnts(values_pcnts, values_pcnts_bads_opa)
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
    dplyr::bind_rows(
      op_consultant_referrals,
      op_followup_reduction,
      op_tele_conversion,
    )
  ),
  tar_target(
    values_aae,
    get_values_aae(strategies_aae, fyears),
    pattern = cross(strategies_aae, fyears)
  ),
  # age standardisation ----
  tar_target(
    age_standardised_rates,
    get_age_standardised_rates(
      values_rates,
      values_los,
      fixed_values_pcnts,
      values_op,
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
  )
)
