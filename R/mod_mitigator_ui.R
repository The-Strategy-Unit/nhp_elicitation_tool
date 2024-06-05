#' mitigator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_mitigator_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::fluidRow(
    col_12(
      shiny::uiOutput(ns("strategy")),
      shinyjs::disabled(
        shinyWidgets::actionBttn(
          ns("prev_strat"),
          "Previous",
          style = "simple",
          color = "primary"
        )
      ),
      identity(
        shinyWidgets::actionBttn(
          ns("next_strat"),
          "Next",
          style = "simple",
          color = "primary"
        )
      ),
      shinyjs::hidden(
        shinyWidgets::actionBttn(
          ns("complete"),
          "Complete",
          style = "simple",
          color = "success"
        )
      ),
      shinyjs::hidden(
        shiny::selectInput(
          ns("change_strat"),
          "Strategy",
          NULL
        )
      ),
      shiny::tags$p("")
    ),
    col_9(
      shiny::fluidRow(
        bs4Dash::box(
          title = "Parameter Setting",
          collapsible = FALSE,
          width = 12,
          shiny::fluidRow(
            col_8(
              shinycssloaders::withSpinner(
                plotly::plotlyOutput(ns("trend_plot"), height = "950px")
              )
            ),
            col_1(
              shinyWidgets::noUiSliderInput(
                ns("param_values"),
                "Percentage Reduction",
                min = 0,
                max = 100,
                value = c(0, 100),
                step = 1,
                orientation = "vertical",
                width = "100%",
                height = "950px",
                color = "#fcdf83",
                format = shinyWidgets::wNumbFormat(decimals = 0)
              )
            ),
            col_3(
              shiny::tags$div(
                style = "position: absolute; top: 0px; width: 95%",
                shiny::textAreaInput(
                  ns("why_lo"),
                  label = HTML("Please provide a rationale for your forecasted range in the box below. <br> <br>
                  Please consider using 'MIRE' as a prompt for structuring your rationale: <br>
                  <ul>
                  <li>Mechanism (e.g. Prevent, Redirect, Substitute, Relocate, Efficiencies, De-adoption)</li>
                  <li>Intervention (the interventions or strategies being implemented)</li>
                  <li>Resource (the workforce, estate, digital infrastructure, financial requirements/implications)</li>
                  <li>Evidence (published evidence or experience/knowledge of intervention impact)</li>
                  </ul>"),
                  width = "100%",
                  height = "200px"
                )
              ),
              shiny::tags$div(
                style = "position: absolute; bottom: 0px; width: 95%",
                shiny::textAreaInput(
                  ns("why_hi"),
                  label = "Please describe any factors and/or barriers which may explain your surprisingly low percentage reduction value (or p10 value).", # nolint
                  width = "100%",
                  height = "200px"
                )
              )
            )
          ),
          if (!is_phase_1()) {
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("results_plot"), height = "400px")
            )
          }
        )
      )
    ),
    col_3(
      shiny::fluidRow(
        bs4Dash::bs4Card(
          title = "Progress",
          collapsible = FALSE,
          width = 12,
          shinyWidgets::progressBar(
            ns("progress"),
            0,
            display_pct = TRUE
          )
        ),
        bs4Dash::box(
          title = "Description",
          collapsible = FALSE,
          width = 12,
          shiny::uiOutput(ns("mitigator_text"))
        )
      )
    )
  )
}
