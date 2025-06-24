#' mitigator Server Functions
#'
#' @noRd
mod_mitigator_server <- function(id, email, strategies) {
  shiny::moduleServer(id, function(input, output, session) {
    # suppresses lintr warnings in vscode
    .data <- rlang::.data

    # load the data
    hist_data <- app_sys("app", "data", "hist_data.csv") |>
      readr::read_csv()

    cagr_data <- app_sys("app", "data", "cagr_data.csv") |>
      readr::read_csv()

    long_term_avg <- cagr_data |>
      dplyr::filter(Metric == "Long-term CAGR avg") |>
      dplyr::pull("Including Covid (1995/96 to present)")

    disc_data <- app_sys("app", "data", "disc_data.csv") |>
      readr::read_csv()

    # reactives ----------------------------------------------------------------

    # this reactive value holds the index of the currently selected strategy
    # it's incremented/decremented by the next/previous buttons
    selected_strategy <- shiny::reactiveVal(1)
    # this is a boolean that indicates whether or not all of the strategies
    # have been visited, and is used to show/hide the complete button
    has_visited_all_strategies <- shiny::reactiveVal()

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

    # observers ----------------------------------------------------------------

    # when the module loads, decide whether the user has visited all of the
    # strategies or not. if so, show the complete button
    shiny::observe({
      e <- shiny::req(email())
      completed <- nrow(get_latest_results(e))
      total <- length(strategies())

      v <- completed == total
      has_visited_all_strategies(v)

      shinyjs::toggle("complete", condition = v)

      # skip the completed questions, unless we have 100% completed
      if (completed < total) {
        shinyjs::toggleState("prev_strat", TRUE)
        # move to the first strategy not complete
        selected_strategy(completed + 1)

        # edge case, we have done all but one, so hide next, show complete
        if (completed == total - 1) {
          shinyjs::toggle("next", FALSE)
          shinyjs::toggleState("next", FALSE)
          shinyjs::toggle("complete", TRUE)
          shinyjs::toggleState("complete", TRUE)
        }
      }
    })

    # Validation

    high_avg <- shiny::reactive({
      shiny::req(valid_inputs())
      (((1 + input$high_0_5 / 100)^5 * (1 + input$high_6_10 / 100)^5)^(1 / 10) -
        1) *
        100
    })

    output$high_avg <- shiny::renderText({
      paste0(round(high_avg(), 1), "%")
    })

    low_avg <- shiny::reactive({
      shiny::req(valid_inputs())
      (((1 + input$low_0_5 / 100)^5 * (1 + input$low_6_10 / 100)^5)^(1 / 10) -
        1) *
        10
    })

    output$low_avg <- shiny::renderText({
      paste0(round(low_avg(), 1), "%")
    })

    proj <- reactive({
      shiny::req(valid_inputs())
      years <- 2025:2034
      data.frame(
        Year = years,
        Low = c(rep(input$low_0_5, 5), rep(input$low_6_10, 5)),
        High = c(rep(input$high_0_5, 5), rep(input$high_6_10, 5))
      )
    })

    output$cagr_table <- shiny::renderTable(
      cagr_data
    )

    output$index_plot <- plotly::renderPlotly({
      index_plot(hist_data, disc_data, proj())
    })

    output$growth_plot <- plotly::renderPlotly({
      growth_plot(hist_data, disc_data, proj(), long_term_avg = long_term_avg)
    })

    # update the progress bar when the selected strategy changes
    # it indicates the % through based on the currently selected strategy, so
    # it will decrement when the user presses the previous button
    shiny::observe({
      s <- selected_strategy()
      n <- length(strategies())

      shinyWidgets::updateProgressBar(session, "progress", s, n)
    })

    # Validation

    valid_inputs <- reactive({
      is_valid_number(input$low_0_5) &
        is_valid_number(input$low_6_10) &
        is_valid_number(input$high_0_5) &
        is_valid_number(input$high_6_10) &
        (input$high_6_10 >= input$low_6_10) &
        (input$high_0_5 >= input$low_0_5)
    })

    output$validation_status <- shiny::renderText({
      text <- ifelse(
        !valid_inputs(),
        "Please enter a numeric value for all estimates. Your surprisingly low estimate must be lower than your surprisingly high estimate for each time period.",
        ""
      )
      text
    })

    # Enforce rounding to 1dp

    shiny::observeEvent(input$low_0_5, {
      shiny::req(valid_inputs())
      rounded_val <- round(input$low_0_5, 1)
      if (rounded_val != input$low_0_5) {
        shiny::updateNumericInput(session, "low_0_5", value = rounded_val)
      }
    })

    shiny::observeEvent(input$high_0_5, {
      shiny::req(valid_inputs())
      rounded_val <- round(input$high_0_5, 1)
      if (rounded_val != input$high_0_5) {
        shiny::updateNumericInput(session, "high_0_5", value = rounded_val)
      }
    })

    shiny::observeEvent(input$low_6_10, {
      shiny::req(valid_inputs())
      rounded_val <- round(input$low_6_10, 1)
      if (rounded_val != input$low_6_10) {
        shiny::updateNumericInput(session, "low_6_10", value = rounded_val)
      }
    })

    shiny::observeEvent(input$high_6_10, {
      shiny::req(valid_inputs())
      rounded_val <- round(input$high_6_10, 1)
      if (rounded_val != input$high_6_10) {
        shiny::updateNumericInput(session, "high_6_10", value = rounded_val)
      }
    })

    #TODO
    # when the selected strategy is changed, get the data from the db and
    # update the inputs, or use default values if the user has not yet saved
    # anything for this strategy
    shiny::observe({
      shiny::req(valid_inputs())
      e <- email()
      s <- selected_strategy_id()

      v <- get_latest_results(e, s)

      # if no rows of data are returned, then the user has not yet saved
      # values, so use some default values instead
      v <- if (nrow(v) == 0) {
        if (is_phase_1()) {
          list(
            low_0_5 = 0,
            low_6_10 = 0,
            low_avg = 0,
            high_0_5 = 0,
            high_6_10 = 0,
            high_avg = 0,
            comments_low = "",
            comments_high = ""
          )
        } else {
          v <- as.list(get_latest_results(e, s, TRUE))
        }
      } else {
        as.list(v)
      }

      shiny::updateNumericInput(
        session,
        "low_0_5",
        value = v$low_0_5
      )

      shiny::updateNumericInput(
        session,
        "low_6_10",
        value = v$low_6_10
      )

      shiny::updateNumericInput(
        session,
        "high_0_5",
        value = v$high_0_5
      )

      shiny::updateNumericInput(
        session,
        "high_6_10",
        value = v$high_6_10
      )

      shiny::updateTextAreaInput(
        session,
        "comments_low",
        value = v$comments_low
      )
      shiny::updateTextAreaInput(
        session,
        "comments_high",
        value = v$comments_high
      )
    }) |>
      shiny::bindEvent(selected_strategy_id(), strategies())

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

      session$userData$last_saved(
        insert_data(
          email(),
          s,
          input$low_0_5,
          input$low_6_10,
          low_avg(),
          input$high_0_5,
          input$high_6_10,
          high_avg(),
          input$comments_low,
          input$comments_high
        )
      )
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
          shinyjs::toggleState("complete", has_visited_all_strategies())
        })
    }

    # output renderers ---------------------------------------------------------

    # the title displayed at the top of the page
    output$strategy <- shiny::renderText({
      strategies()[[selected_strategy()]]$name
    })

    # the box next to the chart which shows the text describing the selected
    # strategy. these are markdown files which are loaded in dynamically
    output$mitigator_text <- shiny::renderUI({
      s <- shiny::req(selected_strategy_group())

      md_file_to_html("app", "mitigators_text", paste0(s, ".md"))
    })

    if (!is_phase_1()) {
      output$results_plot <- plotly::renderPlotly({
        p <- suppressWarnings(
          mitigator_results_plot(
            get_all_users_results(
              phase_1 = TRUE,
              strategy = selected_strategy_id()
            ),
            values = list(low_avg(), high_avg()), # input$param_values,
            email()
          )
        )

        plotly::ggplotly(p, tooltip = "text") |>
          plotly::style(hoverinfo = "none", traces = 2) |>
          plotly::config(
            displayModeBar = FALSE,
            displaylogo = FALSE
          )
      })
    }

    output$results_table <- reactable::renderReactable({
      mitigator_results_table(
        get_all_users_results(
          phase_1 = TRUE,
          strategy = selected_strategy_id()
        ),
        values = list(low_avg(), high_avg()), # input$param_values,
        email()
      ) |>
        reactable::reactable(
          columns = list(
            low_0_5 = reactable::colDef(name = "Low (0–5 yrs)"),
            low_6_10 = reactable::colDef(name = "Low (6–10 yrs)"),
            low_avg = reactable::colDef(
              name = "Low (10-yr avg)",
              style = list(fontWeight = "bold")
            ),
            high_0_5 = reactable::colDef(name = "High (0–5 yrs)"),
            high_6_10 = reactable::colDef(name = "High (6–10 yrs)"),
            high_avg = reactable::colDef(
              name = "High (10-yr avg)",
              style = list(fontWeight = "bold")
            ),
            comments_low = reactable::colDef(name = "Low rationale"),
            comments_high = reactable::colDef(name = "High rationale"),
            is_me = reactable::colDef(show = FALSE)
          ),
          rowStyle = reactable::JS(
            "
          function(rowInfo) {
            if (rowInfo.row.is_me) {
              return { backgroundColor: '#5881c1', color: '#ffffff' }
            } else {
              return { backgroundColor: '#ffffff', color: '#000000' }
            }
          }
        "
          )
        )
    })

    # return -------------------------------------------------------------------
    # no need to return anything from this module
    NULL
  })
}
