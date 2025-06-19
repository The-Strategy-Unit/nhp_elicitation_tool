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
    shiny::fluidRow(
      bslib::layout_columns(
        col_widths = c(8, 4),
        bslib::card(
          bslib::card_title(
            "Instructions"
          ),
          bslib::card_body(
            md_file_to_html(
              "app",
              paste0("home_text_", app_state(), ".md")
            )
          )
        ),
        if (app_is_live()) {
          bslib::card(
            bslib::card_header(
              "Enter your email"
            ),
            bslib::card_body(
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
        }
      )
    )
  )
}
