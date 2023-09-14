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
mod_mitigator_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    .data <- rlang::.data

    trend_data <- app_sys("app", "data", "trend_data.csv") |>
      readr::read_csv(col_types = "dcddd") |>
      dplyr::filter(.data[["year"]] >= 201011)

    strategies <- get_golem_config("strategies") |>
      purrr::keep(~ .x$include %||% TRUE)

    values <- do.call(
      shiny::reactiveValues,
      purrr::map(
        strategies, ~ list(
          values = c(lo = 0, hi = 100),
          comments_lo = "",
          comments_hi = ""
        )
      )
    )

    min_year <- min(trend_data$year)

    selected_strategy <- shiny::reactiveVal(1)

    shiny::observe({
      s <- selected_strategy()
      n <- length(strategies)

      shinyWidgets::updateProgressBar(session, "progress", s - 1, n)
    })


    selected_strategy_text <- shiny::reactive({
      s <- selected_strategy()
      strategies[[s]]$name
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
        shinyjs::show("complete")
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
      shinyjs::hide("complete")
    }) |>
      shiny::bindEvent(input$prev_strat)

    shiny::observe({
      modal <- shiny::modalDialog(
        "are you finished?",
        shiny::tableOutput(
          session$ns("results")
        ),
        title = "Submit Values",
        footer = shiny::tagList(
          shiny::modalButton("Return"),
          shinyWidgets::actionBttn(
            session$ns("save_results"),
            "Save and Exit",
            style = "simple",
            color = "success"
          )
        ),
        size = "l"
      )
      shiny::showModal(modal)
    }) |>
      shiny::bindEvent(input$complete)

    output$results <- shiny::renderTable({
      values |>
        shiny::reactiveValuesToList() |>
        tibble::enframe("strategy") |>
        tidyr::unnest_wider("value") |>
        tidyr::unnest_wider("values")
    })

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
      s <- selected_strategy()
      n <- scales::comma(selected_data_scale()) # nolint
      glue::glue(strategies[[s]]$label)
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
      s <- shiny::req(selected_strategy_id())
      values[[s]]$values[["lo"]] <- input$param_values[[1]]
      values[[s]]$values[["hi"]] <- input$param_values[[2]]
    }) |>
      shiny::bindEvent(input$param_values)

    shiny::observe({
      s <- shiny::req(selected_strategy_id())
      values[[s]]$comments_lo <- input$why_lo
    }) |>
      shiny::bindEvent(input$why_lo)

    shiny::observe({
      s <- shiny::req(selected_strategy_id())
      values[[s]]$comments_hi <- input$why_hi
    }) |>
      shiny::bindEvent(input$why_hi)

    shiny::observe({
      s <- shiny::req(selected_strategy_id())
      v <- values[[s]]
      shinyWidgets::updateNoUiSliderInput(
        session,
        "param_values",
        value = unname(v$values)
      )
      shiny::updateTextAreaInput(
        session,
        "why_lo",
        value = v$comments_lo
      )
      shiny::updateTextAreaInput(
        session,
        "why_hi",
        value = v$comments_hi
      )
    }) |>
      shiny::bindEvent(selected_strategy_id())

    shiny::observe({
      shiny::removeModal()

      filename <- file.path(
        Sys.getenv("save_path", tempdir()),
        paste0(
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".json"
        )
      )

      cat("saving:", filename, "\n")

      values |>
        shiny::reactiveValuesToList() |>
        jsonlite::write_json(filename, auto_unbox = TRUE, pretty = TRUE)
    }) |>
      shiny::bindEvent(input$save_results)
  })
}
