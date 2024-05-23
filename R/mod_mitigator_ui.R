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
                plotly::plotlyOutput(ns("trend_plot"), height = "600px")
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
                height = "500px",
                color = "#fcdf83",
                format = shinyWidgets::wNumbFormat(decimals = 0)
              )
            ),
            col_3(
              shiny::tags$div(
                style = "position: absolute; top: 0px; width: 95%",
                shiny::textAreaInput(
                  ns("why_lo"),
                  label = "What factors make it a surprisingly low % reduction",
                  width = "100%",
                  height = "200px"
                )
              ),
              shiny::tags$div(
                style = "position: absolute; bottom: 0px; width: 95%",
                shiny::textAreaInput(
                  ns("why_hi"),
                  label = "What factors make it a surprisingly high % reduction", # nolint
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
