#' mitigator Server Functions
#'
#' @noRd
mod_mitigator_server <- function(id, email, strategies) {
  shiny::moduleServer(id, function(input, output, session) {
    # suppresses lintr warnings in vscode
    .data <- rlang::.data

    # load the data
    trend_data <- app_sys("app", "data", "trend_data.csv") |>
      readr::read_csv(col_types = "dcddd")

    # keep track of the minimum year in the data, so we always show the same
    # range on the x-axis
    min_year <- min(trend_data$year)

    # reactives ----------------------------------------------------------------

    # this reactive value holds the index of the currently selected strategy
    # it's incremented/decremented by the next/previous buttons
    selected_strategy <- shiny::reactiveVal(1)
    # this is a boolean that indicates whether or not all of the strategies
    # have been visited, and is used to show/hide the complete button
    has_visited_all_strategies <- shiny::reactiveVal()

    # when the selected_strategy changes, get the name to display in the title
    selected_strategy_text <- shiny::reactive({
      s <- selected_strategy()
      strategies()[[s]]$name
    })

    # when the selected_strategy changes, get the id of the strategy to use
    # for selecting data
    selected_strategy_id <- shiny::reactive({
      s <- selected_strategy()
      names(strategies())[[s]]
    })

    # when the selected_strategy changes, get the "group" to use for loading
    # the helper text. any strategy of the form a_b-c_d will be reduced to
    # a_b, so some strategies can share text files
    selected_strategy_group <- shiny::reactive({
      selected_strategy_id() |>
        shiny::req() |>
        stringr::str_remove("-.*$")
    })

    # when the selected strategy changes, subset the data for that strategy
    selected_data <- shiny::reactive({
      s <- selected_strategy_id()

      dplyr::filter(
        trend_data,
        .data[["strategy"]] == s
      )
    })

    # for charts that show a rate per N admissions, figure out what scale to
    # use for N. for all othe charts, the scale is 1.
    selected_data_scale <- shiny::reactive({
      s <- selected_strategy()
      if (!strategies()[[s]]$label |> stringr::str_detect("\\{n\\}")) {
        return(1)
      }
      v <- mean(selected_data()$rate)
      10^round(1 - log10(v))
    })

    # choose the format to use for the y-axis values in the chart. if the
    # y-axis label contains a % character, then format as a percentage.
    # otherwise, format as a number using the scale determined by
    # selected_data_scale
    value_format <- shiny::reactive({
      s <- selected_strategy()
      if (strategies()[[s]]$label |> stringr::str_detect("\\%")) {
        return(scales::percent_format(accuracy = 0.1))
      }

      scales::number_format(accuracy = 0.1, scale = selected_data_scale())
    })

    # get the title to use for the y-axis
    y_axis_title <- shiny::reactive({
      s <- selected_strategy()
      n <- scales::comma(selected_data_scale())
      glue::glue(strategies()[[s]]$label)
    })

    # for the chart, figure out what the future values are to display on the
    # chart as the yellow highlighted area
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

    # observers ----------------------------------------------------------------

    # when the module loads, decide whether the user has visited all of the
    # strategies or not. if so, show the complete button
    shiny::observe({
      completed <- nrow(get_latest_results(email()))
      total <- length(strategies())

      v <- completed == total
      has_visited_all_strategies(v)

      shinyjs::toggle("complete", condition = v)
    })

    # update the progress bar when the selected strategy changes
    # it indicates the % through based on the currently selected strategy, so
    # it will decrement when the user presses the previous button
    shiny::observe({
      s <- selected_strategy()
      n <- length(strategies())

      shinyWidgets::updateProgressBar(session, "progress", s - 1, n)
    })

    # when the selected strategy is changed, get the data from the db and
    # update the inputs, or use default values if the user has not yet saved
    # anything for this strategy
    shiny::observe({
      e <- email()
      s <- selected_strategy_id()

      v <- get_latest_result(e, s)

      # if no rows of data are returned, then the user has not yet saved
      # values, so use some default values instead
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

    # handle the next/previous buttons
    # in both cases, these call the `button_pressed` helper with a value that
    # either increments, or decrements, the selected_strategy reactiveVal
    shiny::observe(button_pressed(+1)) |>
      shiny::bindEvent(input$next_strat)

    shiny::observe(button_pressed(-1)) |>
      shiny::bindEvent(input$prev_strat)

    # the complete button works similar to previous/next buttons, except it
    # triggers the complete event in the main app server function. It also
    # resets the mitigars module back to the beginning
    shiny::observe({
      session$userData$complete("complete")
      button_pressed(-Inf)
    }) |>
      shiny::bindEvent(input$complete)

    # helper functions ---------------------------------------------------------

    # save the values the user has entered for the currently selected strategy
    # back to the database
    save_values <- function() {
      s <- selected_strategy_id()

      insert_data(email(), s, input$param_values, input$why_lo, input$why_hi)
    }

    # when the user presses one of the buttons
    # - save the results
    # - move to the next/previous stratetgy
    # - debounce the button presses, so the user can't spam press the button
    # in testing, the debouncing was an issue, because you could end up with a
    # situation where the save happened before the data for a strategy was
    # loaded - this would cause the strategies values to be overwritten with
    # the values that they had set for a different strategy
    button_pressed <- function(n) {
      # first, save the values for what we have selected
      save_values()
      # then update the selected strategy
      ls <- length(strategies())
      # make sure we don't go below the first strategy, or above the last
      s <- pmin(pmax(selected_strategy() + n, 1), ls)
      selected_strategy(s)

      # if we have reached the end, then cause the "complete" button to display
      if (s == ls) {
        has_visited_all_strategies(TRUE)
      }

      # if we are on the last strategy, hide the next button
      shinyjs::toggle("next_strat", condition = s < ls)
      # if we have visited all the strategies, show the complte button
      shinyjs::toggle("complete", condition = has_visited_all_strategies())

      # debounce the buttons: we first disable all of the buttons so they can't
      # be pressed
      shinyjs::disable("prev_strat")
      shinyjs::disable("next_strat")
      shinyjs::disable("complete")
      # then, we wait 1 second before reenabling the buttons. promises is the
      # most reliable way to achieve this, as we want other things to happen
      # like loading the data while we wait to re-enable the button
      promises::future_promise({
        # in the case of the complete button, we are moved to a different page
        # anyway, so there is no need to sleep
        if (is.finite(n)) {
          Sys.sleep(1)
        }
      }) |>
        promises::then(\(.x) {
          # determine whether to enable/disable buttons: if we are at the first
          # strategy then don't enable previous. likewise, if we are at the last
          # strategy then don't enable the next button (it should be hidden
          # though). and only enable the complete button if we have visited all
          # the strategies (again, it should be hidden if this isn't the case)
          shinyjs::toggleState("prev_strat", s > 1)
          shinyjs::toggleState("next_strat", s < ls)
          shinyjs::toggleState()("complete", has_visited_all_strategies())
        })
    }

    # output renderers ---------------------------------------------------------

    # the title displayed at the top of the page
    output$strategy <- shiny::renderUI({
      shiny::tags$h2(selected_strategy_text())
    })

    # the box next to the chart which shows the text describing the selected
    # strategy. these are markdown files which are loaded in dynamically
    output$mitigator_text <- shiny::renderUI({
      s <- shiny::req(selected_strategy_group())

      md_file_to_html("app", "mitigators_text", paste0(s, ".md"))
    })

    # the main plot that shows the trend data and the selected range of values
    output$trend_plot <- plotly::renderPlotly({
      # ggplot throws a warning about an unknown aesthetic, because we override
      # the plotly tooltip.
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

    # return -------------------------------------------------------------------
    # no need to return anything from this module
    NULL
  })
}
