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
          )
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

#' mitigator Server Functions
#'
#' @noRd
mod_mitigator_server <- function(id, email, strategies) {
  shiny::moduleServer(id, function(input, output, session) {
    .data <- rlang::.data

    trend_data <- app_sys("app", "data", "trend_data.csv") |>
      readr::read_csv(col_types = "dcddd") |>
      dplyr::filter(.data[["year"]] >= 201011)

    min_year <- min(trend_data$year)

    selected_strategy <- shiny::reactiveVal(1)
    has_visited_all_strategies <- shiny::reactiveVal()

    shiny::observe({
      completed <- nrow(get_latest_results(email()))
      total <- length(strategies())

      v <- completed == total
      has_visited_all_strategies(v)

      shinyjs::toggle("complete", condition = v)
    })

    shiny::observe({
      s <- selected_strategy()
      n <- length(strategies())

      shinyWidgets::updateProgressBar(session, "progress", s - 1, n)
    })

    selected_strategy_text <- shiny::reactive({
      s <- selected_strategy()
      strategies()[[s]]$name
    })

    selected_strategy_id <- shiny::reactive({
      s <- selected_strategy()
      names(strategies())[[s]]
    })

    shiny::observe({
      e <- email()
      s <- selected_strategy_id()

      v <- get_latest_result(e, s)

      v <- if (nrow(v) == 0) {
        list(
          lo = 0,
          hi = 100,
          comments_lo = "",
          comments_hi = ""
        )
      } else {
        as.list(v)
      }

      shinyWidgets::updateNoUiSliderInput(
        session,
        "param_values",
        value = c(v$lo, v$hi)
      )

      shiny::updateTextAreaInput(session, "why_lo", value = v$comments_lo)
      shiny::updateTextAreaInput(session, "why_hi", value = v$comments_hi)
    }) |>
      shiny::bindEvent(selected_strategy_id())

    output$strategy <- shiny::renderUI({
      shiny::tags$h2(selected_strategy_text())
    })

    save_values <- function() {
      s <- selected_strategy_id()

      insert_data(email(), s, input$param_values, input$why_lo, input$why_hi)
    }

    shiny::observe(button_pressed(+1)) |>
      shiny::bindEvent(input$next_strat)

    shiny::observe(button_pressed(-1)) |>
      shiny::bindEvent(input$prev_strat)

    button_pressed <- function(n) {
      save_values()
      ls <- length(strategies())
      s <- pmin(pmax(selected_strategy() + n, 1), ls)
      selected_strategy(s)

      if (s == ls) {
        has_visited_all_strategies(TRUE)
      }

      shinyjs::toggle("next_strat", condition = s < ls)
      shinyjs::toggle("complete", condition = has_visited_all_strategies())

      shinyjs::disable("prev_strat")
      shinyjs::disable("next_strat")
      promises::future_promise({
        if (is.finite(n)) {
          Sys.sleep(1)
        }
      }) |>
        promises::then(\(.x) {
          shinyjs::toggleState("prev_strat", s > 1)
          shinyjs::toggleState("next_strat", s < ls)
        })
    }

    shiny::observe({
      session$userData$complete("complete")
      button_pressed(-Inf)
    }) |>
      shiny::bindEvent(input$complete)

    selected_data <- shiny::reactive({
      s <- selected_strategy_id()

      dplyr::filter(
        trend_data,
        .data[["strategy"]] == s
      )
    })

    selected_data_scale <- shiny::reactive({
      s <- selected_strategy()
      if (!strategies()[[s]]$label |> stringr::str_detect("\\{n\\}")) {
        return(1)
      }
      v <- mean(selected_data()$rate)
      10^round(1 - log10(v))
    })

    value_format <- shiny::reactive({
      s <- selected_strategy()
      if (strategies()[[s]]$label |> stringr::str_detect("\\%")) {
        return(scales::percent_format(accuracy = 0.1))
      }

      scales::number_format(accuracy = 0.1, scale = selected_data_scale())
    })

    y_axis_title <- shiny::reactive({
      s <- selected_strategy()
      n <- scales::comma(selected_data_scale()) # nolint
      glue::glue(strategies()[[s]]$label)
    })

    param_table <- shiny::reactive({
      last_year <- selected_data() |>
        dplyr::slice_tail(n = 1)

      p <- 1 - input$param_values / 100

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

    output$trend_plot <- plotly::renderPlotly({
      p <- suppressWarnings(
        mitigator_trend_plot(
          selected_data(),
          param_table(),
          value_format(),
          min_year,
          y_axis_title()
        )
      )

      plotly::ggplotly(p, tooltip = "text")
    })

    shiny::observe({
      shiny::removeModal()

      cat("saving:", filename, "\n")
    }) |>
      shiny::bindEvent(input$save_results)
  })
}
