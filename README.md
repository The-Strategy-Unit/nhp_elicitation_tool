
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NHP Elicitation Tool

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Core shiny app for Strategy Unit Elicitation Projects. This code has
been used a base for a number of elicitation exercises.

## Previous Elicitation Exercises

- 2023-10 [National Elicitation
  (NEE)](https://github.com/The-Strategy-Unit/archive-nhp_elicitation_tool)

- 2024-06 [National Elicitation Community
  (NEECom)](https://github.com/The-Strategy-Unit/elicitation_neecom_2024_06)

- 2024-10 [Non-demographic
  growth](https://github.com/The-Strategy-Unit/elicitation_ndg3_2024_10)

- 2025-07 [Productivity for The Health
  Foundation](https://github.com/The-Strategy-Unit/elicitation_thf_2025_07)

- 2025-11 [Health Status Adjustment
  (HSA)](https://github.com/The-Strategy-Unit/elicitation_hsa_2025_11)

## Deployment

The script `deploy.R` can be used to redeploy to the production and
development environments.

The app is controlled by some environment variables that need to be set:

- `PHASE_1_END` should be the time and date when the first phase should
  end (in the form `YYYY-mm-dd HH:MM:SS` for the timezone
  `Europe/London`)
- `PHASE_2_END` should be as `PHASE_1_END`, but the time and date for
  when the second phase should end
- `PHASE_2_LIVE` should be set to a non-empty string to enable the
  second phase of the app: this allows the app to enter a “disabled”
  state at the end of phase 1
- `NHP_SALT` is a value that is used when encrypting the emails of users
- `save_path` should point to the location where to save the database
  to. If not set, then it defaults to the current working directory

## Development usage

The data required to run the app can be rebuilt using
`targets::tar_make()`. There are a few things that need to be set up in
order to get this to work.

1)  you will need to create a `.Renviron` file, with the following
    items:

- DB_SERVER=…
- DB_DATABASE=…
- NHP_SALT=…

2)  you will need to be connected to the MLCSU VPN and have access to
    the database.

3)  you will need a copy of the file `recruitment.xlsx`, stored in the
    root of the project folder.
