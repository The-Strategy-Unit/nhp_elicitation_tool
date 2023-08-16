#' mitigator UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_mitigator_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    bs4Dash::box(
      title = "Mitigator",
      collapsible = FALSE,
      width = 12,
      shiny::uiOutput(ns("mitigator_text"))
    ),
    bs4Dash::box(
      title = "Parameter Setting",
      collapsible = FALSE,
      width = 12,
      shiny::fluidRow(
        col_8(
          shiny::plotOutput(ns("trend_plot"))
        ),
        col_1(
          shiny::sliderInput(
            ns("param_values"),
            "Values",
            min = 0,
            max = 100,
            value = c(0, 100),
            step = 1
          )
        ),
        col_3(
          shiny::textAreaInput(
            ns("why_lo"),
            label = NULL,
            value = "why low?",
            width = "100%"
          ),
          shiny::textAreaInput(
            ns("why_hi"),
            label = NULL,
            value = "why high?",
            width = "100%"
          )
        )
      )
    ),
    shiny::fluidRow(
      col_3(
        shiny::actionButton(ns("prev"), "Previous")
      ),
      col_3(
        shiny::actionButton(ns("next"), "Next")
      ),
      col_3(
        shiny::actionButton(ns("skip"), "Skip")
      ),
      col_3(
        shiny::actionButton(ns("save"), "Save")
      )
    )
  )
}

#' mitigator Server Functions
#'
#' @noRd
mod_mitigator_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    .data <- rlang::.data

    selected_data <- shiny::reactive({
      set.seed(123)
      tibble::tibble(year = 2000:2019) |>
        dplyr::mutate(
          value = (.data[["year"]] - 2000) * 0.05 + 10 + rnorm(n = dplyr::n()),
          year = .data[["year"]] * 100 + .data[["year"]] - 1999
        )
    })

    param_table <- shiny::reactive({
      last_year <- selected_data() |>
        dplyr::slice_tail(n = 1)

      p <- input$param_values / 100

      tibble::tibble(
        year = last_year$year + c(0, 2020),
        value_lo = last_year$value * c(1, p[[1]]),
        value_hi = last_year$value * c(1, p[[2]])
      )
    })

    output$mitigator_text <- shiny::renderUI({
      shiny::tags$p("mitigators text")
    })

    output$trend_plot <- shiny::renderPlot({
      mitigator_trend_plot(
        selected_data(),
        param_table(),
        scales::number_format(0.1)
      )
    })
  })
}
