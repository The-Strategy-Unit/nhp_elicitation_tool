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
    col_widths = c(6, 6, 12, 12, 12),
    bslib::card(
      bslib::card_title(shiny::textOutput(ns("strategy"))),
      shiny::uiOutput(ns("mitigator_text")),
      tags$table(
        class = "table",
        tags$thead(
          tags$tr(
            tags$th(""),
            tags$th("CAGR 0-5 years"),
            tags$th("CAGR 5-10 years"),
            tags$th("CAGR 0-10 years (derived)")
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
              ns("high_5_10"),
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
              ns("low_5_10"),
              label = NULL,
              value = 0,
              step = 0.1,
              width = "80px"
            )),
            tags$td(shiny::textOutput(ns("low_avg")))
          )
        )
      ),
      shiny::textOutput(ns("validation_status"))
    ),

    bslib::card(
      bslib::card_header("Rationale"),
      shiny::p(
        "Please provide a rationale for your surprisingly low and surprisingly high values, including any main enablers and barriers to productivity.
If these values vary across the two time periods, please provide an explanation for this."
      ),

      shiny::textAreaInput(
        ns("comments_high"),
        label = "Surprisingly high",
        width = "100%",
        height = "100px"
      ),

      shiny::textAreaInput(
        ns("comments_low"),
        label = "Surpringly low",
        width = "100%",
        height = "100px"
      ),

      complete_button
    ),

    bslib::card(
      bslib::card_header("Trend data"),
      bslib::card_body(
        bslib::layout_column_wrap(
          width = 1 / 2,
          plotly::plotlyOutput(ns("growth_plot"), height = "400px"),
          plotly::plotlyOutput(ns("index_plot"), height = "400px"),
        ),
        p(
          "ONS, Public Service Productivity. Estimates after 2022/23 are based on UK quarterly public sector productivity estimates (experimental); these are for the UK and based on calendar years."
        ),
        p("NQA: non-quality-adjusted, CAGR: Compound annual growth rate")
      )
    ),

    if (!is_phase_1()) {
      shiny::tagList(
        bslib::card(
          bslib::card_header(
            "Visualisation of peer results CAGR 0-10 years (derived)"
          ),
          bslib::card_body(
            plotly::plotlyOutput(ns("results_plot"), height = "400px")
          )
        ),
        bslib::card(
          bslib::card_header("All peer results"),
          bslib::card_body(
            reactable::reactableOutput(ns("results_table"))
          )
        )
      )
    }
  )
}
