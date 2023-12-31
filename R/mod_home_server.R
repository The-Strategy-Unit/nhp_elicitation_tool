#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # suppress lintr warnings
    .data <- rlang::.data

    if (!app_is_live()) {
      return(NULL)
    }

    shiny::observe({
      shinyjs::runjs(
        r"{
          $(document).ready(() => {
            email = localStorage.getItem("email");

            if (email !== null) {
              $("#home-email").val(email);
              $("#home-remember_email").prop("checked", true);

              Shiny.setInputValue("home-email", email);
              Shiny.setInputValue("home-remember_email", true);
            }
          });
        }"
      )
    })

    user_mappings <- app_sys("app", "data", "user_mappings.json") |>
      jsonlite::read_json(simplifyVector = TRUE)

    strategies <- get_golem_config("strategies")

    email_hashed <- shiny::reactive({
      input$email |>
        shiny::req() |>
        hash_email()
    })

    selected_strategies <- shiny::reactive({
      um <- user_mappings[[email_hashed()]]

      shiny::validate(
        shiny::need(!is.null(um), "unrecognised email address"),
        shiny::need(
          is_phase_1() || nrow(phase_1_results()) > 0,
          "did not complete phase 1"
        )
      )

      s <- purrr::imap(um, \(t, s) strategies[[s]][t]) |>
        # order by the y-axis label, then the name of the mitigator
        purrr::map(\(at) {
          at |>
            purrr::keep(~ .x$include %||% TRUE) |>
            (\(.x) {
              order <- .x |>
                dplyr::bind_rows(.id = "id") |>
                dplyr::arrange(.data[["label"]], .data[["name"]]) |>
                dplyr::pull("id")

              .x[order]
            })()
        }) |>
        purrr::flatten()

      if (!is_phase_1()) {
        p1r <- phase_1_results()

        s <- s[p1r$strategy]
      }

      s
    })

    shiny::observe({
      if (!input$remember_email) {
        shinyjs::runjs(
          r"{
            localStorage.removeItem("email");
          }"
        )
        return()
      }

      shiny::req(selected_strategies())
      shiny::req(input$remember_email)

      shinyjs::runjs(
        r"{
          localStorage.setItem("email", $("#home-email").val());
        }"
      )
    })

    phase_1_results <- shiny::reactive({
      get_latest_results(email_hashed(), phase_1 = TRUE)
    })

    completed_strategies <- shiny::reactive({
      # only check if the email has been validated
      selected_strategies() |>
        shiny::req() |>
        dplyr::bind_rows(.id = "strategy") |>
        dplyr::transmute(
          .data[["strategy"]],
          complete = FALSE
        ) |>
        dplyr::rows_update(
          email_hashed() |>
            get_latest_results() |>
            dplyr::transmute(
              .data[["strategy"]],
              complete = TRUE
            ),
          by = "strategy"
        )
    })

    output$selected_mitigators <- shiny::renderText({
      cs <- completed_strategies()
      n <- sum(cs$complete)
      d <- nrow(cs)

      paste0(n, "/", d, " mitigators completed")
    })

    output$selected_strategies <- gt::render_gt({
      selected_strategies() |>
        dplyr::bind_rows(.id = "strategy") |>
        dplyr::left_join(
          completed_strategies(),
          by = dplyr::join_by("strategy")
        ) |>
        dplyr::select(-"strategy", -"min_year") |>
        gt::gt(groupname_col = "label") |>
        gt::cols_label(name = "Mitigator") |>
        gt::tab_options(
          row_group.border.top.width = gt::px(2),
          row_group.border.top.color = "black",
          row_group.border.bottom.color = "black",
          row_group.background.color = "#686f73",
        ) |>
        gt::tab_style(
          style = list(
            gt::cell_fill(color = "#cccccc")
          ),
          locations = gt::cells_body(
            rows = .data[["complete"]]
          )
        ) |>
        gt::cols_hide("complete")
    })

    home_return <- shiny::reactive({
      list(
        email = email_hashed(),
        strategies = selected_strategies()
      )
    }) |>
      shiny::bindEvent(input$start)

    return(home_return)
  })
}
