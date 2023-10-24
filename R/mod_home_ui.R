#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_home_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$h1("NHP National Elicitation Tool"),
    shiny::fluidRow(
      bs4Dash::box(
        title = "Instructions",
        collapsible = FALSE,
        width = 8,
        md_file_to_html(
          "app",
          ifelse(
            is_phase_1(),
            "home_text_phase_1.md",
            "home_text_phase_2.md"
          )
        )
      ),
      bs4Dash::box(
        title = "Enter your email",
        collapsible = FALSE,
        width = 4,
        shiny::textInput(
          ns("email"),
          "Email"
        ),
        shiny::checkboxInput(
          ns("remember_email"),
          "Remember me (using cookies)"
        ),
        shiny::textOutput(
          ns("selected_mitigators")
        ),
        shiny::actionButton(
          ns("start"),
          "Start"
        ),
        gt::gt_output(
          ns("selected_strategies")
        )
      )
    )
  )
}
