#' mitigator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_mitigator_ui <- function(id) {
  ns <- shiny::NS(id)
  
  previous_button <- shinyjs::disabled(
    shiny::actionButton(ns("prev_strat"),  "Previous")
  )
  
  next_button <-
    identity(
      shiny::actionButton(ns("next_strat"), "Next")
    )
  
  complete_button <-
    shinyjs::hidden(
      shiny::actionButton(
        ns("complete"),
        "Complete"
      )
    )

    bslib::layout_columns(
      col_widths = c(3, 6, 3, 12),
      
      bslib::card(
        bslib::card_header("Mitigator selection"),
        shiny::uiOutput(ns("strategy")),
        
        shinyjs::hidden(
          shiny::selectInput(
            ns("change_strat"),
            "Strategy",
            NULL,
            width = "100%"
          )
        ),
        
        bslib::layout_columns(previous_button, next_button,col_widths = c(6, 6)),
        complete_button,
        shinyWidgets::progressBar(
          ns("progress"),
          0,
          display_pct = TRUE
        )
      ),
      
      bslib::card(
        bslib::card_header("Trend and predictions"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(10, 2),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("trend_plot"), 
                                   height = "600px")
            ),
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
          )
        )
      ), 
      
      bslib::card(
        bslib::card_header("Reasoning"),
        shiny::textAreaInput(
          ns("why_hi"),
          width = "100%",
          label = "What factors make it a surprisingly high % reduction?",
          height = "100px",
        ), 
        shiny::textAreaInput(
          ns("why_lo"),
          width = "100%",
          height = "100px",
          label = "What factors make it a surprisingly low % reduction?")
      ), 
      
        bslib::card(
          bslib::card_header("Description"),
          shiny::uiOutput(ns("mitigator_text"))
        ),
      
      if (!is_phase_1()) {
        bslib::card(
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("results_plot"), height = "400px")
        )
        )
      }
   
  )


}
