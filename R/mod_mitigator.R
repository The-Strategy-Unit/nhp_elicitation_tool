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
      shinyWidgets::actionBttn(
        ns("next_strat"),
        "Next",
        style = "simple",
        color = "primary"
      ),
      shiny::tags$p("")
    ),
    bs4Dash::box(
      title = "Description",
      collapsible = FALSE,
      width = 2,
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

    strategies <- get_golem_config("strategies")

    min_year <- min(trend_data$year)

    selected_strategy <- shiny::reactiveVal(1)

    selected_strategy_text <- shiny::reactive({
      s <- selected_strategy()
      strategies[[s]]
    })

    selected_strategy_id <- shiny::reactive({
      s <- selected_strategy()
      names(strategies)[[s]]
    })

    output$strategy <- shiny::renderUI({
      shiny::tags$h2(selected_strategy_text())
    })

    shiny::observe({
      ls <- length(strategies)
      s <- pmin(selected_strategy() + 1, ls)
      selected_strategy(s)
      if (s == ls) {
        shinyjs::disable("next_strat")
      }
      shinyjs::enable("prev_strat")
    }) |>
      shiny::bindEvent(input$next_strat)

    shiny::observe({
      s <- pmax(selected_strategy() - 1, 1)
      selected_strategy(s)
      if (s == 1) {
        shinyjs::disable("prev_strat")
      }
      shinyjs::enable("next_strat")
    }) |>
      shiny::bindEvent(input$prev_strat)

    selected_data <- shiny::reactive({
      s <- selected_strategy_id()

      dplyr::filter(
        trend_data,
        .data[["strategy"]] == s
      )
    })

    selected_data_scale <- shiny::reactive({
      v <- mean(selected_data()$rate)
      10^round(1 - log10(v))
    })

    value_format <- shiny::reactive({
      s <- selected_data_scale()
      scales::number_format(accuracy = 0.1, scale = s)
    })

    y_axis_title <- shiny::reactive({
      s <- selected_data_scale()
      paste("Rate per", scales::comma(s), "population")
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
      selected_strategy_id() |>
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
        min_year,
        y_axis_title()
      )
    })
  })
}
