.data <- rlang::.data

generate_user_mappings <- function(file) {
  mappings <- list(
    # Public health focus conditions
    "prevention_public_health" = list(
      ip = c(
        "alcohol_partially_attributable-acute",
        "alcohol_partially_attributable-chronic",
        "alcohol_wholly_attributable",
        "intentional_self_harm",
        "raid_ae",
        "raid_ip",
        "medically_unexplained_related_admissions",
        "obesity_related_admissions",
        "smoking"
      )
    ),
    #  Ambulatory care and inpatient avoidance
    "ambulatory_care_inpatient_avoidance" = list(
      ip = c(
        "ambulatory_care_conditions_acute",
        "ambulatory_care_conditions_chronic",
        "ambulatory_care_conditions_vaccine_preventable",
        "ambulatory_emergency_care_high",
        "ambulatory_emergency_care_low",
        "ambulatory_emergency_care_moderate",
        "ambulatory_emergency_care_very_high",
        "readmission_within_28_days",
        "zero_los_no_procedure-adult"
      )
    ),
    #  Frail and Older People
    "fraily_elderly" = list(
      ip = c(
        "eol_care-0-2_days",
        "eol_care-3+_days",
        "falls_related_admissions",
        "frail_elderly-high",
        "frail_elderly-intermediate",
        "emergency_elderly",
        "general_los_reduction_emergency",
        "excess_beddays_emergency",
        "stroke_early_supported_discharge"
      )
    ),
  # Virtual Wards
  "virtual_wards" = list(
    ip = c(
        "virtual_wards_activity_avoidance_ari",
        "virtual_wards_activity_avoidance_heart_failure",
        "virtual_wards_efficiencies_ari",
        "virtual_wards_efficiencies_heart_failure"
      )
    )
  ) |>
    purrr::map_depth(1, tibble::enframe, "type", "strategy") |>
    tibble::enframe("code", "mappings")

  output_file <- "inst/app/data/user_mappings.json"

  readxl::read_excel(file, skip = 1) |>
    janitor::clean_names() |>
    tidyr::drop_na(tidyselect::any_of(c("contacts", "name"))) |>
    dplyr::rename(email = contacts) |>
    dplyr::select(
      "email",
      "prevention_public_health":"virtual_wards"
    ) |>
    tidyr::pivot_longer("prevention_public_health":"virtual_wards", names_to = "code", values_to = "include") |>
    dplyr::filter(.data[["include"]] == "Y") |>
    dplyr::inner_join(
      mappings,
      by = dplyr::join_by("code")
    ) |>
    tidyr::unnest("mappings") |>
    tidyr::unnest("strategy") |>
    dplyr::select("email", "type", "strategy") |>
    dplyr::arrange("email", "type", "strategy") |>
    dplyr::summarise(
      dplyr::across("strategy", list),
      .by = c("email", "type")
    ) |>
    dplyr::group_nest(.data[["email"]]) |>
    dplyr::mutate(
      dplyr::across("data", \(.x) purrr::map(.x, tibble::deframe))
    ) |>
    dplyr::mutate(
      dplyr::across("email", \(.x) purrr::map_chr(.x, hash_email))
    ) |>
    tibble::deframe() |>
    jsonlite::write_json(
      output_file,
      auto_unbox = TRUE,
      pretty = TRUE
    )

  output_file
}
