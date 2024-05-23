.data <- rlang::.data

generate_user_mappings <- function(file) {
  mappings <- list(
    # Hospital activity amenable to public health interventions
    "a" = list(
      ip = c(
        "alcohol_partially_attributable-acute",
        "alcohol_partially_attributable-chronic",
        "alcohol_wholly_attributable",
        "obesity_related_admissions",
        "smoking",
        "falls_related_admissions"
      )
    ),
    # Hospital activity amenable to primary care and community interventions
    "b" = list(
      ip = c(
        "ambulatory_care_conditions_acute",
        "ambulatory_care_conditions_chronic",
        "ambulatory_care_conditions_vaccine_preventable",
        "emergency_elderly",
        "eol_care-0-2_days",
        "eol_care-3+_days",
        "excess_beddays_elective",
        "excess_beddays_emergency",
        "frail_elderly-high",
        "frail_elderly-intermediate",
        "readmission_within_28_days",
        "stroke_early_supported_discharge",
        "zero_los_no_procedure-adult",
        "zero_los_no_procedure-child"
      )
    ),
    # Hospital activity amenable to psychiatric liaison and community mental
    # health services
    "c" = list(
      ip = c(
        "intentional_self_harm",
        "medically_unexplained_related_admissions",
        "raid_ae",
        "raid_ip"
      )
    ),
    # Hospital activity amenable to medicines management
    "d" = list(
      ip = c(
        "medicines_related_admissions_explicit",
        "medicines_related_admissions_implicit_anti_diabetics",
        "medicines_related_admissions_implicit_benzodiasepines",
        "medicines_related_admissions_implicit_diurectics",
        "medicines_related_admissions_implicit_nsaids"
      )
    ),
    # Emergency department and acute medicine activity
    "e" = list(
      ip = c(
        "ambulatory_emergency_care_low",
        "ambulatory_emergency_care_moderate",
        "ambulatory_emergency_care_high",
        "ambulatory_emergency_care_very_high"
      ),
      aae = c(
        "frequent_attender-adult_ambulance",
        "frequent_attender-adult_walk-in",
        "frequent_attender-child_ambulance",
        "frequent_attender-child_walk-in",
        "left_before_treatment-adult_ambulance",
        "left_before_treatment-adult_walk-in",
        "left_before_treatment-child_ambulance",
        "left_before_treatment-child_walk-in",
        "low_cost_discharged-adult_ambulance",
        "low_cost_discharged-adult_walk-in",
        "low_cost_discharged-child_ambulance",
        "low_cost_discharged-child_walk-in"
      )
    ),
    # Planned surgical activity (adult)
    "f" = list(
      ip = c(
        "cancelled_operations",
        "enhanced_recovery_bladder",
        "enhanced_recovery_breast",
        "enhanced_recovery_colectomy",
        "enhanced_recovery_hip",
        "enhanced_recovery_hysterectomy",
        "enhanced_recovery_knee",
        "enhanced_recovery_prostate",
        "enhanced_recovery_rectum",
        "evidence_based_interventions_ent",
        "evidence_based_interventions_general_surgery",
        "evidence_based_interventions_gi_surgical",
        "evidence_based_interventions_msk",
        "evidence_based_interventions_urology",
        "evidence_based_interventions_vasuclar_varicose_veins",
        "preop_los"
      ),
      op = c(
        "consultant_to_consultant_referrals-adult_surgical",
        "followup_reduction-adult_surgical",
        "tele_conversion-adult_surgical"
      )
    ),
    # Planned medical activity (adult)
    "g" = list(
      op = c(
        "consultant_to_consultant_referrals-adult_non-surgical",
        "followup_reduction-adult_non-surgical",
        "tele_conversion-adult_non-surgical"
      )
    ),
    # Planned paediatric activity
    "h" = list(
      op = c(
        "consultant_to_consultant_referrals-child_non-surgical",
        "consultant_to_consultant_referrals-child_surgical",
        "followup_reduction-child_non-surgical",
        "followup_reduction-child_surgical",
        "tele_conversion-child_non-surgical",
        "tele_conversion-child_surgical"
      )
    )
  ) |>
    purrr::map_depth(1, tibble::enframe, "type", "strategy") |>
    tibble::enframe("code", "mappings")

  output_file <- "inst/app/data/user_mappings.json"

  readxl::read_excel(file) |>
    janitor::clean_names() |>
    tidyr::drop_na(tidyselect::any_of("expert_no")) |>
    dplyr::filter(.data[["exclude"]] == 0) |>
    dplyr::select(
      "expert_no",
      "email",
      "a":"h"
    ) |>
    tidyr::pivot_longer("a":"h", names_to = "code", values_to = "include") |>
    dplyr::filter(.data[["include"]] == 1) |>
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
