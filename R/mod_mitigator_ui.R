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
        "Submit estimates & rationales"
      )
    )

  bslib::layout_columns(
    col_widths = c(6, 6, 4, 4, 4, 12),
    bslib::card(
      bslib::card_title(shiny::textOutput(ns("strategy"))),
      shiny::uiOutput(ns("mitigator_text")),
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
      bslib::card_header("Rationale"),
      shiny::p(
        "Please provide a rationale for your surprisingly low and surprisingly high values, including any main enablers and barriers to productivity.
If these values vary across the two time periods, please provide an explanation for this."
      ),
      bslib::layout_column_wrap(
        width = 1 / 2,
        shiny::textAreaInput(
          ns("comments_low"),
          label = "Surpringly low",
          width = "100%",
          height = "200px"
        ),
        shiny::textAreaInput(
          ns("comments_high"),
          label = "Surprisingly high",
          width = "100%",
          height = "200px"
        )
      ),
      complete_button
    ),

    bslib::card(
      bslib::card_header("Long term historic averages (CAGR % per annum)"),
      shiny::tableOutput(ns("cagr_table"))
    ),

    bslib::card(
      bslib::card_header("Growth plot"),
      bslib::card_body(
        plotly::plotlyOutput(ns("growth_plot"), height = "400px")
      )
    ),

    bslib::card(
      bslib::card_header("Index plot"),
      bslib::card_body(
        plotly::plotlyOutput(ns("index_plot"), height = "400px")
      )
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
