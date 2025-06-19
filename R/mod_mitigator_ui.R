#' mitigator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_mitigator_ui <- function(id) {
  ns <- shiny::NS(id)

  complete_button <-
    shinyjs::hidden(
      shiny::actionButton(
        ns("complete"),
        "Complete"
      )
    )

  bslib::layout_columns(
    col_widths = c(8, 4, 4, 4, 4, 12, 12),
    bslib::card(
      bslib::card_title(shiny::textOutput(ns("strategy"))),
      shiny::uiOutput(ns("mitigator_text"))
    ),
    bslib::card(
      bslib::card_header("Navigation"),
      complete_button,
      shinyWidgets::progressBar(
        ns("progress"),
        0,
        display_pct = TRUE
      )
    ),

    bslib::card(
      tags$table(
        class = "table",
        tags$thead(
          tags$tr(
            tags$th(""),
            tags$th("0–5 yrs"),
            tags$th("6–10 yrs"),
            tags$th("Implied 10-yr avg")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Surprisingly high"),
            tags$td(shiny::numericInput(
              ns("high_0_5"),
              label = NULL,
              value = 0,
              step = 0.1,
              width = "80px"
            )),
            tags$td(shiny::numericInput(
              ns("high_6_10"),
              label = NULL,
              value = 0,
              step = 0.1,
              width = "80px"
            )),
            tags$td(shiny::textOutput(ns("high_avg")))
          ),
          tags$tr(
            tags$td("Surprisingly low"),
            tags$td(shiny::numericInput(
              ns("low_0_5"),
              label = NULL,
              value = 0,
              step = 0.1,
              width = "80px"
            )),
            tags$td(shiny::numericInput(
              ns("low_6_10"),
              label = NULL,
              value = 0,
              step = 0.1,
              width = "80px"
            )),
            tags$td(shiny::textOutput(ns("low_avg")))
          )
        )
      )
    ),

    bslib::card(
      bslib::card_header("Graph"),
      bslib::card_body(
        plotly::plotlyOutput(ns("growth_plot"), height = "400px")
      )
    ),

    bslib::card(
      bslib::card_header("Graph"),
      bslib::card_body(
        plotly::plotlyOutput(ns("index_plot"), height = "400px")
      )
    ),

    bslib::card(
      bslib::card_header("Rationale"),
      shiny::p(
        "What is your rationale for your predictions? If it differs across the two timeframes, please explain your thinking."
      ),
      bslib::layout_column_wrap(
        width = 1 / 2,
        shiny::textAreaInput(
          ns("comments_low"),
          label = "Surpringly low",
          width = "100%"
        ),
        shiny::textAreaInput(
          ns("comments_high"),
          label = "Surprisingly high",
          width = "100%"
        )
      ),
    ),
    if (!is_phase_1()) {
      bslib::card(
        bslib::card_header("Peer results"),

        bslib::layout_column_wrap(
          width = 1 / 2,

          shinycssloaders::withSpinner(
            plotly::plotlyOutput(ns("results_plot"), height = "400px")
          ),

          shiny::tableOutput(ns("results_table"))
        )
      )
    }
  )
}
