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
    bs4Dash::box(
      title = "Mitigator",
      collapsible = FALSE,
      width = 2,
      shinyWidgets::pickerInput(
        ns("strategy_selection"),
        "Mitigator",
        get_golem_config("strategies") |>
          # flip the names and values
          (\(s) setNames(names(s), s))(),
        options = list(
          "live-search" = TRUE
        )
      ),
      shiny::uiOutput(ns("mitigator_text"))
    ),
    col_10(
      shiny::fluidRow(
        bs4Dash::box(
          title = "Parameter Setting",
          collapsible = FALSE,
          width = 12,
          shiny::fluidRow(
            col_8(
              shiny::plotOutput(ns("trend_plot"), height = "600px")
            ),
            col_1(
              shinyWidgets::noUiSliderInput(
                ns("param_values"),
                "Values",
                min = 0,
                max = 100,
                value = c(0, 100),
                step = 1,
                orientation = "vertical",
                direction = "rtl",
                width = "100%", height = "500px",
                color = "#fcdf83"
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
        col_4(
          shiny::actionButton(ns("prev"), "Previous")
        ),
        col_4(
          shiny::actionButton(ns("next"), "Next")
        ),
        col_4(
          shiny::actionButton(ns("save"), "Save")
        )
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

    trend_data <- app_sys("app", "data", "trend_data.csv") |>
      readr::read_csv(col_types = "dcddd")

    min_year <- min(trend_data$year)

    selected_data <- shiny::reactive({
      s <- shiny::req(input$strategy_selection)

      dplyr::filter(
        trend_data,
        .data[["strategy"]] == s
      )
    })

    value_format <- shiny::reactive({
      v <- mean(selected_data()$rate)
      s <- round(1 - log10(v))

      scales::number_format(accuracy = 0.1, scale = 10^s)
    })

    param_table <- shiny::reactive({
      last_year <- selected_data() |>
        dplyr::slice_tail(n = 1)

      p <- input$param_values / 100

      tibble::tibble(
        year = last_year$year + c(0, 2020),
        value_lo = last_year$rate * c(1, p[[1]]),
        value_hi = last_year$rate * c(1, p[[2]])
      )
    })

    selected_strategy_group <- shiny::reactive({
      input$strategy_selection |>
        shiny::req() |>
        stringr::str_remove("-.*$")
    })

    output$mitigator_text <- shiny::renderUI({
      s <- shiny::req(selected_strategy_group())

      md_file_to_html("app", "mitigators_text", paste0(s, ".md"))
    })

    output$trend_plot <- shiny::renderPlot({
      mitigator_trend_plot(
        selected_data(),
        param_table(),
        value_format(),
        min_year
      )
    })

    shiny::observe({
      s <- shiny::req(input$strategy_selection)
    })
  })
}
